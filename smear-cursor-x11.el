;;; smear-cursor-x11.el --- Composite a cursor trail over Emacs's own text -*- lexical-binding: t -*-

;; Copyright (C) 2026 smear-cursor contributors

;; Author: Daskeladden
;; Keywords: frames
;; URL: https://github.com/Daskeladden/smear-cursor

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file manages a stage for each frame and converts Emacs
;; coordinates to X window coordinates.  The module draws the trail.
;;
;; An Emacs canvas image replaces text glyphs.  Copying text in the X
;; server lets the trail appear over text without recreating fonts,
;; ligatures, folded Org blocks or scaled headings.
;;
;; X11 only.  Wayland does not let clients read other surfaces or place
;; override-redirect windows, so this backend cannot run in pgtk Emacs.
;; Use `smear-cursor-x11-available-p' to check availability and choose
;; another backend when needed.

;;; Code:

(require 'cl-lib)

(defvar smear-cursor-x11--module-path
  (expand-file-name "x11/smear-cursor-x11-module.so"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "Specify the expected path to the dynamic module.

Build it with `make' in x11/.  If it is absent,
`smear-cursor-backend' falls back to the Lisp rasterizer.")

(defvar smear-cursor-x11--stages (make-hash-table :test 'eq)
  "Map each frame to one stage shared by its cursor trails.")

(declare-function smear-cursor-x11--open "smear-cursor-x11-module" (window-id display))
(declare-function smear-cursor-x11--trouble "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--describe "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--begin "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--draw "smear-cursor-x11-module"
                  (stage layer))
(declare-function smear-cursor-x11--frame-begin "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--frame-end "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--can-blur-p "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--renderer "smear-cursor-x11-module"
                  (stage gl))
(declare-function smear-cursor-x11--end "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--close "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--background-has-detail-p "smear-cursor-x11-module" (stage))
;; The player thread renders animation frames.  See smear-cursor-x11-play.h
;; for why rendering runs outside Lisp.
(declare-function smear-cursor-x11--play-start "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--play "smear-cursor-x11-module"
                  (stage track layers frames fps &optional frozen still))
(declare-function smear-cursor-x11--freeze "smear-cursor-x11-module"
                  (stage x y w h))
(declare-function smear-cursor-x11--play-position "smear-cursor-x11-module"
                  (stage track))
(declare-function smear-cursor-x11--play-stop-track "smear-cursor-x11-module"
                  (stage track))
(declare-function smear-cursor-x11--play-stop "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--play-stats "smear-cursor-x11-module" (stage))

(defun smear-cursor-x11-load ()
  "Load the dynamic module if it has been built.

Return non-nil when the module is present."
  (or (featurep 'smear-cursor-x11-module)
      (and (file-exists-p smear-cursor-x11--module-path)
           (ignore-errors (module-load smear-cursor-x11--module-path))
           (featurep 'smear-cursor-x11-module))))

;;;; Coordinates

;; `pos-visible-in-window-p' measures X from the text area, excluding
;; the fringe and left margin.  It measures Y from the window top,
;; including the header and tab lines.
;;
;; In a window with header and tab lines totaling 43 pixels, the first
;; text line measured Y = 43.  Do not add the text area's top offset again.
;;
;; The X window identified by `window-id' is the native frame.  Its measured
;; position matches `frame-edges' NATIVE-EDGES, and its X geometry matches
;; frame-native-width by frame-native-height.  Both edge functions below
;; use that origin, but X needs the text area's edge and Y the window's.

(defun smear-cursor-x11--frame-xy (win x y)
  "Convert X and Y in WIN to frame pixel coordinates.

Use coordinates from `pos-visible-in-window-p' and return (X . Y).
X is relative to the text area; Y is relative to the whole window."
  (cons (+ (nth 0 (window-inside-pixel-edges win)) x)
        (+ (nth 1 (window-pixel-edges win)) y)))

(defun smear-cursor-x11--frame-offset (win)
  "Return the pixel offset (DX . DY) from WIN to its frame.

Reuse this offset to avoid repeated window geometry queries when
converting corners with `smear-cursor-x11--corners-in-frame'."
  (cons (nth 0 (window-inside-pixel-edges win))
        (nth 1 (window-pixel-edges win))))

(defun smear-cursor-x11--corners-offset (corners dx dy)
  "Return CORNERS shifted by DX and DY.

CORNERS must contain eight coordinates.  Signal `wrong-length-argument'
if its length is not eight."
  (unless (= (length corners) 8)
    (signal 'wrong-length-argument (list 8 (length corners))))
  (let ((out (make-vector 8 0.0)))
    (dotimes (i 4)
      (aset out (* 2 i) (float (+ dx (aref corners (* 2 i)))))
      (aset out (1+ (* 2 i)) (float (+ dy (aref corners (1+ (* 2 i)))))))
    out))

(defun smear-cursor-x11--corners-in-frame (win corners)
  "Convert the eight coordinates in CORNERS from WIN to frame pixels."
  (let ((off (smear-cursor-x11--frame-offset win)))
    (smear-cursor-x11--corners-offset corners (car off) (cdr off))))

(defun smear-cursor-x11--head-of (win rect)
  "Return the center of RECT in WIN as frame pixel coordinates [X Y].

RECT is [X Y W H].  Its center is the brightest point of the trail
and the start of its gradient."
  (let ((xy (smear-cursor-x11--frame-xy win
                                (+ (aref rect 0) (/ (aref rect 2) 2.0))
                                (+ (aref rect 1) (/ (aref rect 3) 2.0)))))
    (vector (float (car xy)) (float (cdr xy)))))

;;;; Stages

(defun smear-cursor-x11-available-p (&optional frame)
  "Return non-nil if FRAME supports the X11 backend.

FRAME defaults to the selected frame.  Return nil outside X11 or
without the module."
  (let ((frame (or frame (selected-frame))))
    (and (fboundp 'smear-cursor-x11--open)
         (eq (framep frame) 'x)
         (frame-parameter frame 'window-id)
         t)))

(defun smear-cursor-x11-stage (frame)
  "Return the stage for FRAME, creating it if needed.

Return nil if FRAME cannot support a stage.  Each stage contains an
X window and a redirection, and is reused for trails in that frame."
  (or (gethash frame smear-cursor-x11--stages)
      (when (smear-cursor-x11-available-p frame)
        (let ((stage (smear-cursor-x11--open (frame-parameter frame 'window-id)
                                     (frame-parameter frame 'display))))
          (when stage
            (unless (smear-cursor-x11--trouble stage)
              (smear-cursor-x11--choose-renderer frame stage))
            (puthash frame stage smear-cursor-x11--stages))
          stage))))

(defun smear-cursor-x11-shutdown ()
  "Close all stages and wait for their player threads to finish.

Run from `kill-emacs-hook' to prevent drawing during display shutdown."
  (when (fboundp 'smear-cursor-x11--close)
    (maphash (lambda (_frame stage)
               (ignore-errors (smear-cursor-x11--close stage)))
             smear-cursor-x11--stages))
  (clrhash smear-cursor-x11--stages))

(add-hook 'kill-emacs-hook #'smear-cursor-x11-shutdown)

(defun smear-cursor-x11--frame-gone (frame)
  "Close the stage for FRAME when FRAME is deleted."
  (let ((stage (gethash frame smear-cursor-x11--stages)))
    (when stage
      (remhash frame smear-cursor-x11--stages)
      (when (fboundp 'smear-cursor-x11--close)
        (ignore-errors (smear-cursor-x11--close stage))))))

(add-hook 'delete-frame-functions #'smear-cursor-x11--frame-gone)

(defcustom smear-cursor-x11-renderer 'auto
  "Select the renderer for the cursor trail layers.

`render' sends coordinates for X server RENDER primitives, so no image
crosses the wire.  It supports gradients, coverage masks and convolution,
but no per-pixel program.

`gl' runs a fragment shader locally and uploads each image.  It supports
signed-distance falloff and effects unavailable in RENDER.  Uploading
an image a frame is nothing across a local socket, and far too much
across a forwarded connection: an effect that moves arrives there as a
few stills.  Over a forward, use `render'.

`auto' treats a display such as `:0' as local and any display naming
a host as remote."
  :type '(choice (const :tag "Pick by display" auto)
                 (const :tag "RENDER, in the X server" render)
                 (const :tag "A fragment shader, uploaded" gl))
  :group 'smear-cursor)

(defun smear-cursor-x11--local-display-p (frame)
  "Return non-nil if the display for FRAME is on this machine.

Displays without a host, such as `:0' and `unix:0', are local.
All others use a network connection; `localhost:10.0' is a slow SSH forward."
  (let ((d (or (frame-parameter frame 'display) (getenv "DISPLAY") "")))
    (and (string-match-p "\\`\\(unix\\)?:" d) t)))

(defun smear-cursor-x11--choose-renderer (frame stage)
  "Set the renderer for STAGE using `smear-cursor-x11-renderer' and FRAME.

Use the display for FRAME in automatic mode.  Return `gl', `render',
or a string explaining why GL could not be used."
  (let ((want (cond ((eq smear-cursor-x11-renderer 'gl) t)
                    ((eq smear-cursor-x11-renderer 'render) nil)
                    (t (smear-cursor-x11--local-display-p frame)))))
    (smear-cursor-x11--renderer stage want)))

(defun smear-cursor-x11-release (&optional frame)
  "Release the stage for FRAME, or all stages if FRAME is nil."
  (if frame
      (let ((stage (gethash frame smear-cursor-x11--stages)))
        (when stage
          (ignore-errors (smear-cursor-x11--close stage))
          (remhash frame smear-cursor-x11--stages)))
    (maphash (lambda (_f stage) (ignore-errors (smear-cursor-x11--close stage)))
             smear-cursor-x11--stages)
    (clrhash smear-cursor-x11--stages)))

(add-hook 'delete-frame-functions #'smear-cursor-x11-release)

(provide 'smear-cursor-x11)
;;; smear-cursor-x11.el ends here
