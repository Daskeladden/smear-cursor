;;; smear-cursor.el --- Stretchy smear cursor animation on canvas -*- lexical-binding: t -*-

;; Copyright (C) 2026 smear-cursor contributors

;; Author: Daskeladden
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0.50") (transient "0.7.0"))
;; Keywords: frames, faces
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

;; The cursor trail makes large cursor movements easier to follow.
;; Selection already shows the cursor position, so trails during selection
;; are optional.  Screen distance determines whether to animate, including
;; when scrolling moves the cursor across the window.
;;
;; The trail covers text for about a fifth of a second.  Frame budgets
;; limit drawing time so animation does not delay editing.
;;
;; This package draws a smear-cursor.nvim-style quadrilateral using the
;; Emacs 32 canvas image type and per-glyph overlays.  Monospace rows use
;; a character grid.  Proportional and scaled rows use measured glyph boxes.
;;
;; Enable with `smear-cursor-mode'.  Requires a GUI and canvas support.

;;; Code:

(require 'cl-lib)

(defgroup smear-cursor nil
  "Customize cursor trail animation."
  :group 'cursor
  :prefix "smear-cursor-")

(defcustom smear-cursor-while-selecting t
  "Control whether cursor trails appear while a region is active.

On by default.  Modal editing makes a selection out of every motion --
`meow-next-word\=' is a motion that leaves a word selected -- so standing
down whenever a region is active turns the package off for anyone
editing that way, and says nothing about why.

Turn it off for the older behaviour, where a region on the screen is
taken as a selection in progress and the trail keeps out of it.  Cursor
positions are recorded either way, so the next movement starts from
where the selection ended.

A mouse drag is not covered by this: the pointer is doing the moving
and the trail would trot after it."
  :type 'boolean)

(defcustom smear-cursor-while-prompting t
  "Control whether cursor trails appear while a prompt is up.

On, but waiting for the cursor to settle: see
`smear-cursor-prompt-settle\='.  Off draws nothing at all while a prompt
is up."
  :type 'boolean
  :group 'smear-cursor)

(defcustom smear-cursor-prompt-settle 0.25
  "Seconds the cursor must be still behind a prompt before a trail draws.

Nil to draw every jump as it happens.

A completion that previews its candidates -- consult shows each one in
the window behind the prompt -- moves the cursor across the window for
every key pressed, and a flight for each of them is what falls behind
on a display reached over a network.  Waiting for the cursor to settle
draws the one trail that says where it ended up, which is what the
trail is for, and skips the ones nobody was looking at.

The trail then starts from where the cursor was before the burst
rather than from the candidate before last, because nothing is
recorded while the burst runs."
  :type '(choice (const :tag "draw every jump" nil) number)
  :group 'smear-cursor)

(defcustom smear-cursor-min-distance 1.5
  "Set the minimum movement in character cells that starts a trail."
  :type 'number)

(defcustom smear-cursor-fps 60
  "Set the animation timer rate in frames per second.

Rates above 60 only help on displays faster than 60 Hz.  Time scaling
keeps the trajectory identical at every rate."
  :type 'integer)

(defcustom smear-cursor-stiffness-head 0.6
  "Set the head corners' spring stiffness from 0 to 1 at 60 Hz."
  :type 'float)

(defcustom smear-cursor-stiffness-tail 0.2
  "Set the tail corners' spring stiffness from 0 to 1 at 60 Hz.

A lower value than `smear-cursor-stiffness-head' makes the tail move more
slowly than the head.  Equal values produce only a single frame of blur at
the destination."
  :type 'float)

(defcustom smear-cursor-damping 0.85
  "Set the velocity reduction per frame from 0 (none) to 1 (full)."
  :type 'float)

(defcustom smear-cursor-trailing-exponent 3.0
  "Set how spring stiffness changes from the head to the tail.

Higher values keep the middle corners closer to the head stiffness."
  :type 'float)

(defcustom smear-cursor-max-overshoot '(0.0 . 0.5)
  "Limit overshoot as a fraction of the cursor's size.

Use a number for both axes, or (SIDEWAYS . UPDOWN) for separate limits.
The default allows no horizontal overshoot and half a cursor vertically.
Use nil for no limit."
  :type '(choice (const :tag "Uncapped" nil)
                 (float :tag "Fraction of a cursor, both axes")
                 (cons :tag "Sideways and up/down"
                       (float :tag "Sideways") (float :tag "Up and down"))))

(defun smear-cursor--overshoot-cap (cw lh)
  "Return the overshoot limits in pixels using cell width CW and height LH.

Convert `smear-cursor-max-overshoot' to (X . Y), or return nil if
uncapped."
  (let ((o smear-cursor-max-overshoot))
    (cond ((null o) nil)
          ((consp o) (cons (* (car o) cw) (* (cdr o) lh)))
          (t (cons (* o cw) (* o lh))))))

(defcustom smear-cursor-max-anticipation 1.0
  "Limit backward movement to a fraction of the cursor's size.

Use nil to let it grow with the jump distance.  The initial movement is
set by `smear-cursor-anticipation'."
  :type '(choice (const :tag "Unbounded" nil) float))

(defcustom smear-cursor-anticipation 0.2
  "Set the initial velocity factor away from the target."
  :type 'float)

(defcustom smear-cursor-max-length nil
  "Limit the trail length in character cells, or use nil for no limit.

The tail moves toward the cursor while the head stays in place.  This
never places the trail where the cursor has not been.  The default is nil."
  :type '(choice (const :tag "No limit" nil) integer))

(defcustom smear-cursor-typing-highlight nil
  "Highlight typed characters and fade them back to the background.

Deletion flashes the character's former position.  This uses faces and
overlays independently of the trail, without canvases or hiding the
cursor."
  :type 'boolean)

(defcustom smear-cursor-typing-highlight-color nil
  "Set the typing highlight color, or use nil for the cursor color."
  :type '(choice (const :tag "Cursor colour" nil) color))

(defcustom smear-cursor-typing-highlight-strength 0.55
  "Set how far a highlight blends from its background toward its color."
  :type 'float)

(defcustom smear-cursor-typing-highlight-duration 0.2
  "Set how many seconds a typing highlight takes to fade."
  :type 'float)

(defcustom smear-cursor-typing-highlight-fps 30
  "Set the typing highlight frame rate in frames per second."
  :type 'integer)

(defcustom smear-cursor-typing-highlight-max-change 4
  "Set the largest edit in characters that counts as typing."
  :type 'integer)

(defcustom smear-cursor-color nil
  "Set the trail fill color, or use nil for the frame's cursor color."
  :type '(choice (const :tag "Cursor color" nil) color))

(defcustom smear-cursor-fade-tail t
  "Make the trail thinner toward its tail when non-nil."
  :type 'boolean)

(defcustom smear-cursor-measure-budget 0.004
  "Limit glyph measurement time per animation frame, in seconds.

Rows outside the character grid are measured across multiple frames as
needed, so a whole row need not be measured at once."
  :type 'number)

(defcustom smear-cursor-frame-budget 0.033
  "Set the frame time in seconds above which an animation stops.

Use nil for no limit.  This supplements the glyph measurement limit in
`smear-cursor-measure-budget'."
  :type '(choice (const :tag "No limit" nil) number))

(defcustom smear-cursor-max-cells 512
  "Stop an animation that needs more than this many cells."
  :type 'integer)

;;;; Geometry: corner easing

(defcustom smear-cursor-max-duration 0.45
  "Limit a single trail's duration in seconds, or use nil for no cap.

The limit applies to the animation regardless of the trail style."
  :type '(choice (const :tag "No cap" nil) float))

(defconst smear-cursor--eps 3.0
  "Set the convergence threshold in pixels.

The animation ends when the tail is within this distance of its target.")

(defvar smear-cursor--dist (make-vector 4 0.0)
  "Store each corner's projection along the movement direction.")

(defun smear-cursor--wind-up (delta size)
  "Return the initial backward velocity for a target DELTA pixels away.

SIZE is the cursor's extent on that axis, used to apply
`smear-cursor-max-anticipation'."
  (let ((v (* (- smear-cursor-anticipation) delta)))
    (if (null smear-cursor-max-anticipation)
        v
      (let ((cap (* smear-cursor-max-anticipation size)))
        (max (- cap) (min cap v))))))

(defun smear-cursor--target-x (target i)
  "Return the x coordinate of corner I of TARGET.

Corners run clockwise from the top left, numbered 0 to 3."
  (+ (aref target 0) (if (memq i '(1 2)) (aref target 2) 0.0)))

(defun smear-cursor--target-y (target i)
  "Return the y coordinate of corner I of TARGET.

Corners run clockwise from the top left, numbered 0 to 3."
  (+ (aref target 1) (if (memq i '(2 3)) (aref target 3) 0.0)))

(defun smear-cursor--corners-from-rect (corners rect)
  "Fill the eight-element CORNERS vector from RECT, given as [X Y W H]."
  (dotimes (i 4)
    (aset corners (* 2 i) (smear-cursor--target-x rect i))
    (aset corners (1+ (* 2 i)) (smear-cursor--target-y rect i))))

(defun smear-cursor--clamp-corners (corners target max-len)
  "Translate CORNERS toward TARGET to place the farthest within MAX-LEN.

MAX-LEN is in pixels from TARGET's center.  Translation preserves the
shape of the quadrilateral."
  (let* ((tcx (+ (aref target 0) (/ (aref target 2) 2.0)))
         (tcy (+ (aref target 1) (/ (aref target 3) 2.0)))
         (dmax2 -1.0) (fx 0.0) (fy 0.0))
    (dotimes (i 4)
      (let* ((dx (- (aref corners (* 2 i)) tcx))
             (dy (- (aref corners (1+ (* 2 i))) tcy))
             (d2 (+ (* dx dx) (* dy dy))))
        (when (> d2 dmax2)
          (setq dmax2 d2 fx dx fy dy))))
    (let ((dmax (sqrt dmax2)))
      (when (> dmax max-len)
        (let ((ux (/ (* fx (- dmax max-len)) dmax))
              (uy (/ (* fy (- dmax max-len)) dmax)))
          (dotimes (i 4)
            (aset corners (* 2 i) (- (aref corners (* 2 i)) ux))
            (aset corners (1+ (* 2 i))
                  (- (aref corners (1+ (* 2 i))) uy))))))))

(defun smear-cursor--cap-overshoot (corners velocities i was target cap)
  "Limit CORNERS at index I to CAP past TARGET after a crossing.

WAS is the position before the step.  Clear the corresponding entry in
VELOCITIES when the limit is applied."
  (let ((now (aref corners i)))
    (when (and (< (* (- target was) (- target now)) 0.0)
               (> (abs (- now target)) cap))
      (aset corners i (+ target (if (> now target) cap (- cap))))
      (aset velocities i 0.0))))

(defun smear-cursor--shorten-corners (corners target max-len)
  "Move CORNERS toward TARGET's center to stay within MAX-LEN pixels.

Corners already within the limit stay in place, so the head stays on the
cursor and only the tail moves."
  (let ((tcx (+ (aref target 0) (/ (aref target 2) 2.0)))
        (tcy (+ (aref target 1) (/ (aref target 3) 2.0))))
    (dotimes (i 4)
      (let* ((xi (* 2 i)) (yi (1+ (* 2 i)))
             (dx (- (aref corners xi) tcx))
             (dy (- (aref corners yi) tcy))
             (d (sqrt (+ (* dx dx) (* dy dy)))))
        (when (> d max-len)
          (let ((k (/ max-len d)))
            (aset corners xi (+ tcx (* dx k)))
            (aset corners yi (+ tcy (* dy k)))))))))

(defun smear-cursor--ease-step (corners velocities target head-k tail-k
                                        damping exponent dt &optional cap)
  "Advance CORNERS and VELOCITIES toward TARGET by one spring step.

HEAD-K and TAIL-K set stiffness, with EXPONENT controlling the change
between them.  DAMPING reduces velocity per frame.  DT is time in 60 Hz
frame units.  CAP is (X-PIXELS . Y-PIXELS), or nil for no overshoot limit.

Length is limited only at start or retarget by
`smear-cursor--clamp-corners', never per frame.  Return the largest
remaining distance or velocity magnitude among the corners."
  (let* ((tcx (+ (aref target 0) (/ (aref target 2) 2.0)))
         (tcy (+ (aref target 1) (/ (aref target 3) 2.0)))
         (dmin 1e30) (dmax -1e30) (ret 0.0)
         (vcf (expt (- 1.0 damping) dt)))
    (dotimes (i 4)
      (let ((d (sqrt (+ (expt (- tcx (aref corners (* 2 i))) 2)
                        (expt (- tcy (aref corners (1+ (* 2 i)))) 2)))))
        (aset smear-cursor--dist i d)
        (setq dmin (min dmin d) dmax (max dmax d))))
    (let ((spread (max 1e-6 (- dmax dmin))))
      (dotimes (i 4)
        (let* ((x (/ (- (aref smear-cursor--dist i) dmin) spread))
               (st (min 1.0 (+ head-k (* (- tail-k head-k)
                                         (expt x exponent)))))
               (keff (- 1.0 (expt (- 1.0 st) dt)))
               (tx (smear-cursor--target-x target i))
               (ty (smear-cursor--target-y target i))
               (xi (* 2 i)) (yi (1+ (* 2 i))))
          (let ((px (aref corners xi)) (py (aref corners yi)))
            (aset velocities xi (+ (aref velocities xi)
                                   (* keff (- tx (aref corners xi)))))
            (aset velocities yi (+ (aref velocities yi)
                                   (* keff (- ty (aref corners yi)))))
            (aset corners xi (+ (aref corners xi) (aref velocities xi)))
            (aset corners yi (+ (aref corners yi) (aref velocities yi)))
            ;; Limit overshoot on each axis and clear its velocity.  Without a cap,
            ;; a long jump can move the head several characters past the cursor.
            (when cap
              (smear-cursor--cap-overshoot corners velocities xi px tx (car cap))
              (smear-cursor--cap-overshoot corners velocities yi py ty (cdr cap)))
            (aset velocities xi (* (aref velocities xi) vcf))
            (aset velocities yi (* (aref velocities yi) vcf))))))
    (dotimes (i 4)
      (let* ((xi (* 2 i)) (yi (1+ (* 2 i)))
             (dd (sqrt (+ (expt (- (smear-cursor--target-x target i)
                                   (aref corners xi)) 2)
                          (expt (- (smear-cursor--target-y target i)
                                   (aref corners yi)) 2))))
             (vv (sqrt (+ (expt (aref velocities xi) 2)
                          (expt (aref velocities yi) 2)))))
        (setq ret (max ret dd vv))))
    ret))

;;;; Rasterizer: even-odd scanline fill of the (possibly non-convex) quad

(defun smear-cursor--scanline-xs (corners y xs)
  "Find the intersections of horizontal line Y with quadrilateral CORNERS.

Write sorted x coordinates into XS, which has at least four elements, and
return their count.  Include lower endpoints and exclude upper endpoints
and horizontal edges."
  (let ((n 0))
    (dotimes (i 4)
      (let* ((j (mod (1+ i) 4))
             (x0 (aref corners (* 2 i))) (y0 (aref corners (1+ (* 2 i))))
             (x1 (aref corners (* 2 j))) (y1 (aref corners (1+ (* 2 j)))))
        (when (or (and (<= y0 y) (< y y1))
                  (and (<= y1 y) (< y y0)))
          (aset xs n (+ x0 (* (- x1 x0) (/ (- y y0) (- y1 y0)))))
          (setq n (1+ n)))))
    (let ((i 1))
      (while (< i n)
        (let ((v (aref xs i)) (j (1- i)))
          (while (and (>= j 0) (> (aref xs j) v))
            (aset xs (1+ j) (aref xs j))
            (setq j (1- j)))
          (aset xs (1+ j) v))
        (setq i (1+ i))))
    n))

(defun smear-cursor--row-coverage (corners y row x0 w xs)
  "Accumulate coverage of quadrilateral CORNERS for pixel row Y.

Clear ROW and fill its first W entries for pixels starting at X0.  Use XS
as four-element scratch space.  Coverage is 0 to 32, from two samples at
Y+0.25 and Y+0.75 with rounded edge overlaps.  Return non-nil if any pixel
has coverage."
  (fillarray row 0)
  (let ((any nil) (sub 0))
    (while (< sub 2)
      (let* ((sy (+ y 0.25 (* sub 0.5)))
             (n (smear-cursor--scanline-xs corners sy xs))
             (k 0))
        (while (< k n)
          (let ((xa (max (aref xs k) (float x0)))
                (xb (min (aref xs (1+ k)) (float (+ x0 w)))))
            (when (< xa xb)
              (setq any t)
              (let* ((pa (floor xa)) (pb (ceiling xb)) (px pa))
                (while (< px pb)
                  ;; Use integer coverage for interior pixels.  Only the two edge pixels
                  ;; need floating-point arithmetic.
                  (let ((c (if (and (>= px xa) (<= (1+ px) xb))
                               16
                             (round (* 16.0 (- (min xb (float (1+ px)))
                                               (max xa (float px))))))))
                    (when (> c 0)
                      (aset row (- px x0) (+ (aref row (- px x0)) c))))
                  (setq px (1+ px))))))
          (setq k (+ k 2))))
      (setq sub (1+ sub)))
    any))

;;;; Color: opaque ARGB blending and tail fade

(defsubst smear-cursor--blend (bg fg f)
  "Blend FG over BG with coverage F and return an opaque ARGB32 integer.

FG and BG are [R G B] vectors with values from 0 to 255.  F ranges from 0
to 1."
  (let ((f (min 1.0 (max 0.0 f))))
    (logior #xFF000000
            (ash (round (+ (aref bg 0)
                           (* f (- (aref fg 0) (aref bg 0))))) 16)
            (ash (round (+ (aref bg 1)
                           (* f (- (aref fg 1) (aref bg 1))))) 8)
            (round (+ (aref bg 2)
                      (* f (- (aref fg 2) (aref bg 2))))))))

(defun smear-cursor--fade-coeffs (corners target coeffs)
  "Write tail fade coefficients for CORNERS and TARGET into COEFFS.

COEFFS is [A B C] for A*x+B*y+C, clamped by callers to 0 through 1.  Fade
is zero at the rear corner and one at TARGET's center.  Use [0 0 1] when
there is no useful movement axis."
  (let* ((cx 0.0) (cy 0.0)
         (tcx (+ (aref target 0) (/ (aref target 2) 2.0)))
         (tcy (+ (aref target 1) (/ (aref target 3) 2.0))))
    (dotimes (i 4)
      (setq cx (+ cx (aref corners (* 2 i)))
            cy (+ cy (aref corners (1+ (* 2 i))))))
    (setq cx (/ cx 4.0) cy (/ cy 4.0))
    (let* ((vx (- tcx cx)) (vy (- tcy cy))
           (vlen (sqrt (+ (* vx vx) (* vy vy)))))
      (if (< vlen 1.0)
          (progn (aset coeffs 0 0.0) (aset coeffs 1 0.0) (aset coeffs 2 1.0))
        (let* ((ux (/ vx vlen)) (uy (/ vy vlen))
               (pmin 1e30)
               (phead (+ (* ux tcx) (* uy tcy))))
          (dotimes (i 4)
            (let ((p (+ (* ux (aref corners (* 2 i)))
                        (* uy (aref corners (1+ (* 2 i)))))))
              (when (< p pmin) (setq pmin p))))
          (let ((denom (- phead pmin)))
            (if (< denom 1.0)
                (progn (aset coeffs 0 0.0) (aset coeffs 1 0.0)
                       (aset coeffs 2 1.0))
              (aset coeffs 0 (/ ux denom))
              (aset coeffs 1 (/ uy denom))
              (aset coeffs 2 (- (/ pmin denom))))))))))

(defconst smear-cursor--gamma 2.2
  "Set the gamma used for smooth color blending at edges.")

(defun smear-cursor--make-lut (bg fg)
  "Build a 33-entry ARGB table blending FG over BG at coverage L/32.

Blend in linear light with gamma 2.2."
  (let ((lut (make-vector 33 0))
        (g smear-cursor--gamma))
    (dotimes (lvl 33)
      (let ((f (/ lvl 32.0)) (px #xFF000000))
        (dotimes (ch 3)
          (let* ((lin-bg (expt (/ (aref bg ch) 255.0) g))
                 (lin-fg (expt (/ (aref fg ch) 255.0) g))
                 (c (round (* 255.0 (expt (+ (* (- 1.0 f) lin-bg)
                                             (* f lin-fg))
                                          (/ 1.0 g))))))
            (setq px (logior px (ash c (* 8 (- 2 ch)))))))
        (aset lut lvl px)))
    lut))

(defun smear-cursor--lut-for (anim cell)
  "Return the blend table for CELL's background, cached in ANIM."
  (let ((luts (smear-cursor--anim-luts anim))
        (bgpix (smear-cursor--cell-bgpix cell)))
    (or (gethash bgpix luts)
        (puthash bgpix
                 (smear-cursor--make-lut (smear-cursor--cell-bg cell)
                                         (smear-cursor--anim-color anim))
                 luts))))

;;;; Animation state

(cl-defstruct (smear-cursor--cell
               (:constructor smear-cursor--cell-create))
  data      ; pixel vector, w*ch ARGB fixnums (shared with the canvas)
  image
  overlay   ; overlay carrying the display prop, or nil for pad cells
  bg        ; [r g b] background at this cell
  bgpix
  lut       ; 33-entry blend table bg->color, filled lazily by the painter
  stamp     ; Last frame that painted this cell.
  col row
  x         ; left edge in window text-area pixels
  w)        ; canvas width: the grid's cell width on a simple row, the
            ; glyph's own advance on a proportional one

(cl-defstruct (smear-cursor--anim
               (:constructor smear-cursor--anim-create))
  window buffer
  corners   ; 8-float vector
  (velocities (make-vector 8 0.0)) ; Eight per-corner velocity floats.
  target    ; [x y w h] float rect
  cells     ; hash cell-key -> cell struct or 'skip
  pads      ; hash row -> (NCOLS . OVERLAY) end-of-line padding
  (rows (make-hash-table :test 'eql)) ; hash row -> row info / 'fallback / 'bad
  anchor    ; cons (ROW . BOL) of the first successfully probed row
  (luts (make-hash-table :test 'eql)) ; hash bgpix -> 33-entry blend table
  (gap-max 0.0) (gap-sum 0.0)         ; tick cadence statistics
  (gc0 0.0)                           ; `gc-elapsed' at animation start.
  cw lh     ; cell width and row pitch (line height incl. line-spacing)
  ch        ; canvas height = glyph height (lh minus line-spacing)
  y-base    ; pixel y of the grid's first row top (rows at y-base + k*lh)
  win-w win-h ; window text area size in pixels
  color     ; smear [r g b]
  fade      ; fade coeff vector [a b c]
  frame
  (blanks 0) ; consecutive frames that painted nothing
  timer last-time cursor-saved
  paint-total (paint-frames 0)
  ;; Append slots because accessors inline slot numbers.  Inserting a slot
  ;; makes already compiled accessors read the wrong fields.
  tick      ; buffer-chars-modified-tick the row cache was built against
  grid      ; `yes'/`no' once the character grid has been verified
  (slow 0)   ; consecutive frames that overran the frame budget
  window-start ; `window-start' the row cache was built against
  born ; Start time from `float-time', for the duration cap.
  x11 ; the X stage while its overlay is up, or nil
  history ; recent corner vectors, newest first, for echo layers
  ;; One-based frame index of the worst gap.  A first-frame stall is overlay
  ;; setup; a later stall is an Emacs delay.
  (gap-frames 0) (gap-max-at 0)
  ;; Keep every gap for the median.  One stall can distort the mean.
  gaps
  ;; Save each frame so retargeting starts at the frame displayed by the
  ;; player thread, not the last frame computed by Lisp.
  springs
  end-timer)

(defsubst smear-cursor--cell-key (idx row)
  "Combine target index IDX and ROW into an integer hash key."
  (+ (* row 4096) idx))

(defvar smear-cursor--row (make-vector 0 0.0)
  "Store scratch coverage values for one pixel row, growing as needed.")
(defvar smear-cursor--xs (make-vector 4 0.0)
  "Store scratch x coordinates for scanline intersections.")

(defvar smear-cursor--measure-deadline 0.0
  "Record the time at which the current frame stops measuring glyphs.

Each frame sets this from `smear-cursor-measure-budget'.")

(defvar smear-cursor--offscreen nil
  "Record whether the last frame's quadrilateral was entirely off screen.

An empty frame outside the window does not mean the trail cannot be drawn.")

(defvar smear-cursor--measure-deferred nil
  "Record whether the current frame deferred layout measurements.

An empty frame with deferred work can draw after later measurements
finish.")

(defun smear-cursor--may-measure-p ()
  "Return non-nil while the frame has time for more layout measurements.

Otherwise, record that measurement was deferred."
  (or (< (float-time) smear-cursor--measure-deadline)
      (progn (setq smear-cursor--measure-deferred t) nil)))

(defcustom smear-cursor-pad-line-ends t
  "Allow the trail to extend past line ends using display-only padding.

This is enabled by default and does not change buffer text.  Characters
typed at a padded line end appear after the blank columns.  Typing effects
use separate canvases and do not use this padding."
  :type 'boolean)

(defvar smear-cursor--stale-cells 0
  "Count cached cells that did not contain the requested pixel.

A nonzero count indicates that cached geometry differs from the row
layout.")

(defvar smear-cursor--pool (make-hash-table :test 'eql)
  "Store unused cell records by canvas width in pixels.")
(defvar smear-cursor--pool-size nil
  "Record the canvas height of pooled cells so a change clears the pool.")

(defun smear-cursor--cell-at (anim idx row &optional no-pad)
  "Look up ANIM's cached cell at target IDX on ROW, resolving as needed.

Non-nil NO-PAD prevents adding padding past the line end."
  (let* ((key (smear-cursor--cell-key idx row))
         (cells (smear-cursor--anim-cells anim))
         (hit (gethash key cells)))
    (or hit
        (puthash key (smear-cursor--resolve-cell anim idx row no-pad) cells))))

(defun smear-cursor--release-cell (_anim cell)
  "Detach CELL's overlay and return the cell record to the pool.

Clear and refresh padding cells, which stay in the line's after-string
until the animation ends.  _ANIM is unused."
  (let ((ov (smear-cursor--cell-overlay cell)))
    (if ov
        (delete-overlay ov)
      (fillarray (smear-cursor--cell-data cell)
                 (smear-cursor--cell-bgpix cell))
      (canvas-refresh (smear-cursor--cell-image cell) 'reload-data)))
  (push cell (gethash (smear-cursor--cell-w cell) smear-cursor--pool)))

(defun smear-cursor--finish-frame (anim frame)
  "Refresh ANIM's cells stamped for FRAME and return the painted count.

Reload the Lisp pixel data of each painted cell.  Return stale overlay
cells to the pool and clear and park stale padding cells."
  (let ((cells (smear-cursor--anim-cells anim))
        (live-rows (make-hash-table :test 'eql))
        (stale nil) (dirty 0))
    (maphash (lambda (key cell)
               (when (smear-cursor--cell-p cell)
                 (cond
                  ((= (smear-cursor--cell-stamp cell) frame)
                   (setq dirty (1+ dirty))
                   (puthash (smear-cursor--cell-row cell) t live-rows)
                   (canvas-refresh (smear-cursor--cell-image cell) 'reload-data))
                  ((smear-cursor--cell-overlay cell)
                   (push key stale))
                  ((/= (smear-cursor--cell-stamp cell) -2)
                   (fillarray (smear-cursor--cell-data cell)
                              (smear-cursor--cell-bgpix cell))
                   (canvas-refresh (smear-cursor--cell-image cell) 'reload-data)
                   (setf (smear-cursor--cell-stamp cell) -2)))))
             cells)
    (dolist (key stale)
      (smear-cursor--release-cell anim (gethash key cells))
      (remhash key cells))
    ;; Remove padding from rows outside the trail.  Its saved background can
    ;; leave a stale block of colour after the current-line highlight moves.
    ;; Retargeting at the same row pitch and phase keeps cells, so padding
    ;; needs separate cleanup.
    (let ((pads (smear-cursor--anim-pads anim)) (gone nil))
      (maphash (lambda (row pad)
                 (unless (gethash row live-rows)
                   (when (overlayp (cdr-safe pad)) (delete-overlay (cdr pad)))
                   (push row gone)))
               pads)
      (dolist (row gone) (remhash row pads)))
    dirty))

(defun smear-cursor--paint-frame-cpu (anim)
  "Rasterize ANIM's quadrilateral into cells and refresh the painted cells.

Release stale cells."
  (setq smear-cursor--measure-deadline (+ (float-time)
                                          smear-cursor-measure-budget))
  (let* ((corners (smear-cursor--anim-corners anim))
         (lh (smear-cursor--anim-lh anim))
         (fade (smear-cursor--anim-fade anim))
         (fa 0.0) (fb 0.0) (fc 1.0)
         (fade-p smear-cursor-fade-tail)
         (frame (1+ (smear-cursor--anim-frame anim)))
         (minx 1e30) (miny 1e30) (maxx -1e30) (maxy -1e30))
    (setf (smear-cursor--anim-frame anim) frame)
    (when fade-p
      (smear-cursor--fade-coeffs corners (smear-cursor--anim-target anim)
                                 fade)
      (setq fa (aref fade 0) fb (aref fade 1) fc (aref fade 2)))
    ;; Use 1/1024 units to avoid per-pixel floating-point arithmetic:
    ;; fq = fbase(row) + fstep * pixel-index.
    (let ((fstep (if fade-p (round (* 1024.0 fa)) 0)))
    (dotimes (i 4)
      (let ((x (aref corners (* 2 i))) (y (aref corners (1+ (* 2 i)))))
        (setq minx (min minx x) maxx (max maxx x)
              miny (min miny y) maxy (max maxy y))))
    (let* ((y-base (smear-cursor--anim-y-base anim))
           (ch (smear-cursor--anim-ch anim))
           (x0 (max 0 (floor minx)))
           (x1 (min (smear-cursor--anim-win-w anim) (ceiling maxx)))
           (y0 (max y-base (floor miny)))
           (y1 (min (smear-cursor--anim-win-h anim) (ceiling maxy)))
           (w (- x1 x0)))
      (setq smear-cursor--offscreen (not (and (> w 0) (> y1 y0))))
      (when (and (> w 0) (> y1 y0))
        (when (< (length smear-cursor--row) w)
          (setq smear-cursor--row (make-vector w 0.0)))
        (let ((y y0))
          (while (< y y1)
            (when (smear-cursor--row-coverage corners y smear-cursor--row
                                              x0 w smear-cursor--xs)
              (let* ((crow (/ (- y y-base) lh)) (ly (% (- y y-base) lh))
                     (px 0)
                     ;; Resolve cells only at cell boundaries to avoid per-pixel lookups.
                     (cell nil) (cell-x0 0) (cell-x1 0)
                     (fbase (if fade-p
                                (round (* 1024.0 (+ (* fa (+ x0 0.5))
                                                    (* fb (+ y 0.5))
                                                    fc)))
                              1024)))
                (while (< px w)
                  (let ((cov (aref smear-cursor--row px)))
                    ;; Canvases do not cover the line-spacing strip at ly >= ch.
                    (when (and (> cov 0) (< ly ch))
                      (let ((ax (+ x0 px)))
                        (unless (and (>= ax cell-x0) (< ax cell-x1))
                          (setq cell (smear-cursor--target-at
                                      anim ax crow
                                      (not smear-cursor-pad-line-ends)))
                          ;; A stale cell can put AX outside its canvas after a layout change.
                          ;; Count and skip it to avoid writing past the canvas buffer.
                          ;; `smear-cursor--revalidate' should prevent this.
                          (if (and (smear-cursor--cell-p cell)
                                   (>= ax (smear-cursor--cell-x cell))
                                   (< ax (+ (smear-cursor--cell-x cell)
                                            (smear-cursor--cell-w cell))))
                              (setq cell-x0 (smear-cursor--cell-x cell)
                                    cell-x1 (+ (smear-cursor--cell-x cell)
                                               (smear-cursor--cell-w cell)))
                            (when (smear-cursor--cell-p cell)
                              (setq smear-cursor--stale-cells
                                    (1+ smear-cursor--stale-cells))
                              (setq cell nil))
                            (setq cell-x0 ax cell-x1 (1+ ax))))
                        (when (smear-cursor--cell-p cell)
                          (let ((data (smear-cursor--cell-data cell))
                                (lut (or (smear-cursor--cell-lut cell)
                                         (setf (smear-cursor--cell-lut cell)
                                               (smear-cursor--lut-for
                                                anim cell)))))
                            (when (/= (smear-cursor--cell-stamp cell) frame)
                              (fillarray data (smear-cursor--cell-bgpix cell))
                              (setf (smear-cursor--cell-stamp cell) frame))
                            ;; feff in 256..1024 sets the 0.25 minimum tail opacity.
                            ;; lvl = cov*feff/1024 stays in 0..32.
                            (let* ((fq (min 1024 (max 0 (+ fbase
                                                           (* fstep px)))))
                                   (feff (+ 256 (ash (* 768 fq) -10)))
                                   (lvl (min 32 (ash (* cov feff) -10))))
                              (aset data (+ (* ly (smear-cursor--cell-w cell))
                                            (- ax (smear-cursor--cell-x cell)))
                                    (aref lut lvl))))))))
                  (setq px (1+ px)))))
            (setq y (1+ y)))))))
    (smear-cursor--finish-frame anim frame)))

;;;; Backend

(defcustom smear-cursor-backend 'cpu
  "Choose the renderer for cursor trails.

Use `cpu' for coverage computed in Lisp.  Use `x11' to blend over a
copy of the frame's pixels without covering text with cells.  It needs
X11 and the module built with make in x11/.  An unavailable backend
falls back to the Lisp renderer.  X11 does not work with pgtk or
Wayland."
  :type '(choice (const :tag "Lisp rasterizer" cpu)
                 (const :tag "X compositing over live text" x11)))

(defun smear-cursor--max-length-px (frame cw)
  "Return the trail length limit in pixels, or nil if uncapped.

CW is the cell width in pixels.  FRAME is unused."
  (ignore frame)
  (and smear-cursor-max-length (float (* smear-cursor-max-length cw))))

;;;; When effects run

(defvar smear-cursor-mode)              ; defined by the minor mode below

(defun smear-cursor--typing-looking-at-p ()
  "Return non-nil when the selected window shows the current buffer."
  (eq (current-buffer) (window-buffer (selected-window))))

(defun smear-cursor--minibuffer-p ()
  "Return non-nil while the minibuffer is active.

Most effects mark an edit, and there is nothing worth marking about
one in a prompt, so they stand down here.  The idle effect is the
exception: see `smear-cursor-idle-while-prompting'."
  (minibufferp))

(defvar smear-cursor--complained nil
  "Record whether an effect failure has been reported.")

(defmacro smear-cursor--flourish (&rest body)
  "Run BODY for its effects without allowing any signal to escape.

Report the first failure and return nil when an error occurs."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (error
      (unless smear-cursor--complained
        (setq smear-cursor--complained t)
        (message "smear-cursor: an effect failed and was skipped: %S" err))
      nil)))

(defun smear-cursor--child-frame-showing-p ()
  "Return non-nil while any child frame is on the screen.

This is used to avoid drawing effects over completion popups.

On the screen, rather than merely not invisible: `frame-visible-p\='
answers t, nil, or the symbol `icon\=', and the last of those is truthy.
Corfu keeps one child frame for the session and puts it away by
iconifying it, so from the first completion onwards every effect that
marks an edit stood down -- no flash on a kill, none on a yank, none
on a keystroke -- and nothing said why."
  (catch 'showing
    (dolist (frame (frame-list))
      (when (and (frame-parent frame) (eq t (frame-visible-p frame)))
        (throw 'showing t)))))

(defun smear-cursor--own-edit-p (beg end)
  "Return non-nil when BEG to END is a change made by the user.

The buffer must be selected, writable, and outside the minibuffer, with
point inside the change and no visible child frame."
  (and (smear-cursor--typing-looking-at-p)
       (not (smear-cursor--minibuffer-p))
       (not (smear-cursor--child-frame-showing-p))
       (not buffer-read-only)
       (<= beg (point) end)))

;;;; Trail styles

;; Styles define shapes and head-to-tail opacity separately from drawing.
;; X RENDER supports gradients, blur and transforms, but no per-pixel
;; programs.  The same style data can also be drawn by shaders.
;;
;; Styles apply only to the `x11' backend.  Cell backends cannot draw glow
;; between cells without replacing more text, so they ignore styles.

(defvar smear-cursor--trails (make-hash-table :test 'eq)
  "Store trail styles by name.

See `smear-cursor-define-trail'.")

(defconst smear-cursor--history-max 12
  "Limit the number of earlier trail shapes kept for echo layers.")

(defun smear-cursor-define-trail (name &rest plist)
  "Define trail style NAME from PLIST.

PLIST accepts :doc and :layers.  Each layer is a plist with these keys:
  :shape   `quad' (default) or `radial' for a round glow at the head.
  :alpha   Head opacity, falling to 8% of that value at the tail.
  :stops   ((AT . ALPHA) ...), with AT from 0 at the head to 1 at the
             tail.
  :grow    Outward expansion in pixels.
  :blur    Blur radius, ignored if the display cannot blur.
  :radius  Radius of a radial glow.
  :echo    Number of frames back to use for the layer's shape."
  (declare (indent 1))
  (puthash name plist smear-cursor--trails)
  name)

(defun smear-cursor-trail (name)
  "Return trail style NAME, or the plain style if NAME is undefined."
  (or (gethash name smear-cursor--trails)
      (gethash 'plain smear-cursor--trails)))

(defcustom smear-cursor-trail-style 'plain
  "Choose the trail style for the `x11' backend.

Cell backends ignore this option and draw plain trails.  See
`smear-cursor-define-trail' to add styles."
  :type '(choice (const :tag "One flat quad" plain)
                 (const :tag "A bright head with a soft halo" comet)
                 (const :tag "Discrete copies along the path" ghost)
                 (const :tag "A thin bright filament" ribbon)
                 (const :tag "A red laser pointer" laser)
                 (symbol :tag "Another, by name")))

(defun smear-cursor--layer-color (layer style default)
  "Return the color for LAYER from STYLE or DEFAULT.

Use LAYER's :color first, then STYLE's :color, then DEFAULT."
  (or (plist-get layer :color)
      (and smear-cursor-color (smear-cursor--color-rgb smear-cursor-color))
      (plist-get style :color)
      default))

(defun smear-cursor--trail-color ()
  "Return the current trail color as [R G B].

Colors assigned to individual layers do not affect this value."
  (or (and smear-cursor-color (smear-cursor--color-rgb smear-cursor-color))
      (plist-get (smear-cursor-trail smear-cursor-trail-style) :color)
      (smear-cursor--color-rgb (face-background 'cursor nil t))
      [200 200 200]))

(defun smear-cursor--spring (key default)
  "Return spring parameter KEY for the current trail, or DEFAULT.

A style's :spring settings apply to every backend."
  (let ((spring (plist-get (smear-cursor-trail smear-cursor-trail-style)
                           :spring)))
    (or (and spring (plist-get spring key)) default)))

(defun smear-cursor--layer-stops (layer)
  "Return LAYER's opacity stops as ((AT . ALPHA) ...) from head to tail."
  (or (plist-get layer :stops)
      (let ((a (or (plist-get layer :alpha) 0.7)))
        (list (cons 0.0 a) (cons 1.0 (* a 0.08))))))

(defun smear-cursor--layer-corners (anim layer)
  "Return LAYER's current or earlier shape from ANIM.

If an echo exceeds the recorded history, use the oldest available shape."
  (let ((echo (plist-get layer :echo))
        (history (smear-cursor--anim-history anim)))
    (if (and echo history (> echo 0))
        (or (nth (1- echo) history)
            (car (last history)))
      (smear-cursor--anim-corners anim))))

(defun smear-cursor--remember-shape (anim)
  "Save ANIM's current shape for later echo layers."
  (let ((history (cons (copy-sequence (smear-cursor--anim-corners anim))
                       (smear-cursor--anim-history anim))))
    (setf (smear-cursor--anim-history anim)
          (if (> (length history) smear-cursor--history-max)
              (butlast history (- (length history) smear-cursor--history-max))
            history))))

(defun smear-cursor--trail-bbox (layers shapes head)
  "Return the bounding rectangle for LAYERS at SHAPES around HEAD.

SHAPES contains frame-pixel corners for each layer.  Include expansion,
blur, and glow radius in the returned (X Y W H) rectangle."
  (let ((x0 1e30) (y0 1e30) (x1 -1e30) (y1 -1e30))
    (cl-loop for layer in layers
             for shape in shapes do
             (let ((pad (+ 2 (abs (or (plist-get layer :grow) 0))
                           (* 2 (or (plist-get layer :blur) 0)))))
               (if (eq (plist-get layer :shape) 'radial)
                   (let ((r (+ pad (or (plist-get layer :radius) 0))))
                     (setq x0 (min x0 (- (aref head 0) r))
                           y0 (min y0 (- (aref head 1) r))
                           x1 (max x1 (+ (aref head 0) r))
                           y1 (max y1 (+ (aref head 1) r))))
                 (dotimes (i 4)
                   (setq x0 (min x0 (- (aref shape (* 2 i)) pad))
                         x1 (max x1 (+ (aref shape (* 2 i)) pad))
                         y0 (min y0 (- (aref shape (1+ (* 2 i))) pad))
                         y1 (max y1 (+ (aref shape (1+ (* 2 i))) pad)))))))
    (when (< x0 x1)
      (list (floor x0) (floor y0)
            (ceiling (- x1 x0)) (ceiling (- y1 y0))))))

(defun smear-cursor--layer-vector (layer corners head color)
  "Convert LAYER, CORNERS, HEAD and COLOR to the X module's vector format.

Return [KIND CORNERS HEAD COLOR GROW BLUR RADIUS STOPS], with flattened
stops."
  (let* ((stops (smear-cursor--layer-stops layer))
         (flat (make-vector (* 2 (length stops)) 0.0))
         (i 0))
    (dolist (s stops)
      (aset flat i (float (car s)))
      (aset flat (1+ i) (float (cdr s)))
      (setq i (+ i 2)))
    (vector (if (eq (plist-get layer :shape) 'radial) 1 0)
            corners head color
            (or (plist-get layer :grow) 0)
            (or (plist-get layer :blur) 0)
            (or (plist-get layer :radius) 0)
            flat)))

;; Styles differ in shape, brightness and duration so they are distinct
;; during a roughly 0.2-second trail.  In a comparison, changing alpha and
;; echo distance covered 1.7 times as many pixels but looked the same.

(smear-cursor-define-trail 'plain
  :doc "One quad, fading toward the tail.  What the trail was first."
  :layers '((:shape quad :alpha 0.72)))

(smear-cursor-define-trail 'comet
  :doc "A bright head with a wide soft halo dragging behind it."
  :layers '((:shape quad :grow 10 :blur 7 :alpha 0.34)
            (:shape quad :stops ((0.0 . 0.9) (0.5 . 0.4) (1.0 . 0.03)))
            (:shape radial :radius 30 :alpha 0.75))
  ;; Keep the tail responsive.  A measured jump at :tail 0.10 and
  ;; :damping 0.9 took 735 ms, compared with 317 ms for plain.
  :spring '(:head 0.62 :tail 0.17 :damping 0.84 :exponent 3.5))

(smear-cursor-define-trail 'ghost
  :doc "Discrete copies of the cursor, strung out along its path."
  :layers '((:shape quad :echo 9 :alpha 0.22)
            (:shape quad :echo 6 :alpha 0.34)
            (:shape quad :echo 3 :alpha 0.5)
            (:shape quad :alpha 0.85))
  ;; High stiffness keeps echoes shaped like separate cursors.
  :spring '(:head 0.9 :tail 0.75 :damping 0.7 :exponent 1.0))

(smear-cursor-define-trail 'ribbon
  :doc "A thin bright filament, snapping quickly to the cursor."
  ;; Use -2 for the perpendicular inset.  At -7 the inset exceeds the
  ;; trail half-width and erases the trail.
  :layers '((:shape quad :grow -2
             :stops ((0.0 . 1.0) (0.12 . 0.7) (0.45 . 0.22) (1.0 . 0.0))))
  :spring '(:head 1.0 :tail 0.35 :damping 0.75 :exponent 4.0))

(smear-cursor-define-trail 'laser
  :doc "A red laser pointer: a hot near-white dot inside a red bloom,
dragging a short streak.  Deliberately unlike the rest -- if a style is
hard to tell from another, this one is the control."
  :color [255 45 30]
  :layers '((:shape quad :grow 8 :blur 6 :alpha 0.4)        ; Soft trail.
            (:shape quad :stops ((0.0 . 0.95) (0.4 . 0.5) (1.0 . 0.02)))
            (:shape radial :radius 26 :alpha 0.55)          ; Glow around the centre.
            (:shape radial :radius 8 :alpha 1.0             ; Bright centre.
             :color [255 240 232]))
  ;; A :tail value of 0.45 barely stretches the quad.  Shrinking a layer
  ;; by 4 px on a ten-pixel cursor leaves a red block with no visible trail.
  :spring '(:head 1.0 :tail 0.16 :damping 0.8 :exponent 3.0))

(defvar smear-cursor--refused 0
  "Count animations rejected by the module since Emacs started.

A rejection means the player lock was unavailable within the allowed wait.")

(defvar smear-cursor--last-command nil
  "Store the command that scheduled the pending cursor sample.

Timers do not retain `this-command', so traces use this value.")

(defvar smear-cursor--last-stats nil
  "Store the last animation's (TOTAL-PAINT-SECONDS . FRAMES) pair.")

(defun smear-cursor--drain-line (drain age)
  "Format display delay DRAIN with its sample AGE.

Both are in seconds.  A negative AGE means no sample has been taken."
  (cond ((not drain) "not measured")
        ((and age (< age 0)) "not sampled yet")
        ((and age (>= age 1.0))
         (format "%.1f ms behind (sampled %.0f s ago)" (* 1000.0 drain) age))
        (t (format "%.1f ms behind" (* 1000.0 drain)))))

(defun smear-cursor--report-verdict (paint gap worst at want &optional drain refused)
  "Return a one-line performance assessment of the last animation.

PAINT, median GAP, WORST gap and desired interval WANT are milliseconds.
AT identifies the worst frame.  DRAIN is a display delay in seconds or
nil, and REFUSED counts rejected animations.  Prioritize display delay,
paint cost, setup cost, then interruptions."
  (cond
   ;; Without playback, the figures describe a previous animation.
   ((and refused (> refused 0))
    (format "%d flight(s) the player would not take -- dropped to keep Emacs moving"
            refused))
   ;; Check network delay first.  Frames timed at the socket can appear
   ;; after the animation ends on a forwarded display.  Blocking writes
   ;; count as paint time, so paint timing alone can misidentify the cause.
   ((and drain (> (* 1000.0 drain) (* 3 want)))
    "the display is behind -- the frames are drawn, the connection has not carried them")
   ((> paint want) "painting is over budget -- ours")
   ((and (> worst (* 3 want)) (<= at 1))
    "the stall is frame 1 -- putting the overlay up, once a flight")
   ((> gap (* 2 want))
    "gap is much larger than the paint -- Emacs is busy elsewhere")
   ((> worst (* 3 want))
    "one frame stalled mid-flight -- Emacs was busy, not the trail")
   (t "within budget")))

(defun smear-cursor-report ()
  "Report the last animation's timing measurements for this session.

Show paint time, median frame gap, late frames, and the worst gap with its
frame number.  Display delay measures a round trip after the last frame.
Nothing accumulates: each completed animation replaces the previous
measurements."
  (interactive)
  (let ((s smear-cursor--last-stats))
    (if (not s)
        (message "smear-cursor: no smear yet -- move the cursor, then run this")
      (let* ((n (max 1 (plist-get s :frames)))
             (paint (/ (* 1000.0 (plist-get s :paint-total)) n))
             (gaps (plist-get s :gaps))
             (want (/ 1.0 (max 1 smear-cursor-fps)))
             (gap (* 1000.0 (smear-cursor--median gaps)))
             (late (smear-cursor--late-frames gaps want))
             (worst (* 1000.0 (plist-get s :gap-max)))
             (drain (plist-get s :drain))
             (at (or (plist-get s :gap-max-at) 0)))
        (setq want (* 1000.0 want))
        (message
         (concat
          "smear-cursor: %s/%s %s  |  %d frames in %.0f ms\n"
          "  paint %.1f ms/frame (budget %.1f)\n"
          "  gap %.1f typical, %.1f worst at frame %d/%d, %d late\n"
          "  display %s  |  gc %.1f ms  |  refused %d  |  %s\n"
          "  this flight ended %s")
         (if (plist-get s :threaded)
             (format "%s+thread" smear-cursor-backend)
           (format "%s" smear-cursor-backend))
         (or (plist-get s :renderer) "-")
         (or (plist-get s :style) "-")
         n (* 1000.0 (or (plist-get s :seconds) 0))
         paint want
         gap worst at n late
         (smear-cursor--drain-line drain (plist-get s :drain-age))
         (* 1000.0 (or (plist-get s :gc) 0))
         smear-cursor--refused
         (smear-cursor--report-verdict paint gap worst at want drain
                                       smear-cursor--refused)
         (let ((at (plist-get s :at)))
           (if at
               (format "%.1f s ago" (- (float-time) at))
             "at some point")))))))

;;;; The X compositing backend

;; Emacs canvas images replace glyphs, so the Lisp rasterizer hides the
;; text that its trail covers.
;;
;; The X backend copies Emacs output and blends over it, preserving fonts,
;; ligatures, folded org blocks and inline images.  Drawing runs in the
;; X server, so forwarded connections carry coordinates rather than pixels.
;;
;; The implementation is in x11/.  When unavailable, the Lisp rasterizer
;; is used.

(defcustom smear-cursor-x11-alpha 0.72
  "Set the trail's head opacity for the `x11' backend."
  :type 'float)

(defcustom smear-cursor-x11-tail-alpha 0.10
  "Set the trail's tail opacity for the `x11' backend.

This is ignored when `smear-cursor-fade-tail' is nil, which gives uniform
opacity."
  :type 'float)

(declare-function smear-cursor-x11-load "smear-cursor-x11" ())
(declare-function smear-cursor-x11-stage "smear-cursor-x11" (frame))
(declare-function smear-cursor-x11--play "smear-cursor-x11-module"
                  (stage track layers frames fps &optional frozen still))
(declare-function smear-cursor-x11--freeze "smear-cursor-x11-module"
                  (stage x y w h))
(declare-function smear-cursor-x11--frame-xy "smear-cursor-x11" (win x y))
(declare-function smear-cursor-x11--sample "smear-cursor-x11-module"
                  (stage x y w h))
(declare-function smear-cursor-x11--play-position "smear-cursor-x11-module"
                  (stage track))
(declare-function smear-cursor-x11--describe "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--play-stop-track "smear-cursor-x11-module"
                  (stage track))
(declare-function smear-cursor-x11-release "smear-cursor-x11" (&optional frame))
(declare-function smear-cursor-x11--frame-offset "smear-cursor-x11" (win))
(declare-function smear-cursor-x11--corners-offset "smear-cursor-x11"
                  (corners dx dy))
(declare-function smear-cursor-x11--play-stop-track "smear-cursor-x11-module"
                  (stage track))
(declare-function smear-cursor-x11-release "smear-cursor-x11" (&optional frame))
(declare-function smear-cursor-x11--choose-renderer "smear-cursor-x11"
                  (frame stage))
(declare-function smear-cursor-x11--trouble "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--begin "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--end "smear-cursor-x11-module" (stage))
(declare-function smear-cursor-x11--draw "smear-cursor-x11-module"
                  (stage layer))
(declare-function smear-cursor-x11--frame-begin "smear-cursor-x11-module"
                  (stage))
(declare-function smear-cursor-x11--frame-end "smear-cursor-x11-module"
                  (stage))
(declare-function smear-cursor-x11--corners-in-frame "smear-cursor-x11"
                  (win corners))
(declare-function smear-cursor-x11--head-of "smear-cursor-x11" (win rect))

(defvar smear-cursor--x11-loaded nil
  "Record X11 module lookup as `yes', `no', or nil before lookup.

Look for the module only once.")

(defun smear-cursor--x11-available-p ()
  "Return non-nil when the X11 module is loaded and usable."
  (unless smear-cursor--x11-loaded
    (setq smear-cursor--x11-loaded
          (if (and (require 'smear-cursor-x11 nil t)
                   (fboundp 'smear-cursor-x11-load)
                   (smear-cursor-x11-load))
              'yes
            'no)))
  (eq smear-cursor--x11-loaded 'yes))

(defun smear-cursor--x11-stage (anim)
  "Return the X11 stage for ANIM, or nil if unavailable.

Callers fall back to another renderer when the stage is unavailable."
  (when (and (eq smear-cursor-backend 'x11)
             (smear-cursor--x11-available-p)
             (fboundp 'smear-cursor-x11-stage))
    (let* ((win (smear-cursor--anim-window anim))
           (stage (and win (smear-cursor-x11-stage (window-frame win)))))
      (and stage (not (smear-cursor-x11--trouble stage)) stage))))

(defun smear-cursor--x11-renderer-name (anim)
  "Return ANIM's renderer name as a symbol, or nil."
  (let ((stage (smear-cursor--anim-x11 anim)))
    (when (and stage (fboundp 'smear-cursor-x11--renderer))
      ;; Query with the current setting to avoid changing it.
      (ignore-errors
        (smear-cursor-x11--choose-renderer
         (window-frame (or (smear-cursor--anim-window anim)
                           (selected-window)))
         stage)))))

(defconst smear-cursor--track-trail 0
  "Identify the player track for cursor trails.

Separate effect tracks allow trails and effects to appear together.  This
matches SMEAR_TRACK_TRAIL in smear-cursor-x11-play.h.")

(defconst smear-cursor--track-occasion 1
  "Identify the shared track for pulse, copy and deletion effects.

Each new effect on this track replaces the previous one.")

(defconst smear-cursor--track-typing 2
  "Identify the typing track, which can run alongside other effects.")

(defconst smear-cursor--track-rest 4
  "Identify the player track for the effect the cursor carries.

A track of its own, because what the cursor wears never stops: sharing
the trail\='s, it could only be aimed again between flights and a
movement left it behind for the length of a turn.  This matches
SMEAR_MAX_TRACKS in smear-cursor-x11-play.h, which had to make room
for it.")

(defconst smear-cursor--track-idle 3
  "Identify the track for whatever plays while the cursor is left alone.

Its own track so that stopping it, which happens at the first sign of
anybody working, stops nothing else.")

(defun smear-cursor--x11-down (anim)
  "Remove ANIM's X11 overlay from the screen if present."
  (let ((stage (smear-cursor--anim-x11 anim)))
    (when stage
      (setf (smear-cursor--anim-x11 anim) nil)
      ;; Stop only the trail track.  Stopping all tracks can cancel the pulse
      ;; from the same keystroke, making long-jump pulses intermittent.
      ;; The module removes the overlay when the last track stops.
      (ignore-errors
        (cond ((fboundp 'smear-cursor-x11--play-stop-track)
               (smear-cursor-x11--play-stop-track
                stage smear-cursor--track-trail))
              ((fboundp 'smear-cursor-x11--play-stop)
               (smear-cursor-x11--play-stop stage))
              (t (smear-cursor-x11--end stage)))))))

(defun smear-cursor--effects-down ()
  "Remove all X11 tracks from the screen, including effects.

`smear-cursor--x11-down' stops only the trail track."
  (dolist (frame (frame-list))
    (when (and (fboundp 'smear-cursor-x11-stage)
               (eq smear-cursor-backend 'x11))
      (let ((stage (ignore-errors (smear-cursor-x11-stage frame))))
        (when (and stage (fboundp 'smear-cursor-x11--play-stop))
          (ignore-errors (smear-cursor-x11--play-stop stage)))))))

(defun smear-cursor--paint-frame-x11 (anim)
  "Blend ANIM's trail over the text it crosses using X11.

Return 1 if drawn, or nil if this backend is unavailable.  The return
value is not a cell count."
  (let ((stage (smear-cursor--x11-stage anim)))
    (if (not stage)
        (progn (smear-cursor--x11-down anim) nil)
      (let* ((win (smear-cursor--anim-window anim))
             (color (smear-cursor--anim-color anim))
             (head (smear-cursor-x11--head-of
                    win (smear-cursor--anim-target anim)))
             (style (smear-cursor-trail smear-cursor-trail-style))
             (layers (plist-get style :layers)))
        ;; Show the overlay once per animation because this copies the whole window.
        (unless (eq (smear-cursor--anim-x11 anim) stage)
          (smear-cursor--x11-down anim)
          (smear-cursor-x11--begin stage)
          (setf (smear-cursor--anim-x11 anim) stage))
        (setf (smear-cursor--anim-frame anim)
              (1+ (or (smear-cursor--anim-frame anim) 0)))
        ;; Copy the background once.  Copying it per layer erases earlier layers.
        (let* ((shapes (mapcar (lambda (layer)
                                 (smear-cursor-x11--corners-in-frame
                                  win (smear-cursor--layer-corners anim layer)))
                               layers))
               (box (smear-cursor--trail-bbox layers shapes head)))
          (when box
            (apply #'smear-cursor-x11--frame-begin stage box)
            (cl-loop for layer in layers
                     for shape in shapes do
                     (smear-cursor-x11--draw
                      stage (smear-cursor--layer-vector
                             layer shape head
                             (smear-cursor--layer-color layer style color))))))
        (smear-cursor-x11--frame-end stage)
        ;; Advance after drawing so echoes show earlier trail positions.
        (smear-cursor--remember-shape anim)
        1))))


;;;; Flights: precomputed animation frames

;; Lisp animation pauses during other Emacs work.  In a measured session,
;; one of fifteen frames arrived 235 ms late despite taking 6 ms to paint.
;;
;; The spring is deterministic and `smear-cursor-max-duration' bounds the
;; frame count.  Lisp computes all frames for a separate player thread,
;; so drawing can continue during Emacs work.  See smear-cursor-x11-play.h.

(defconst smear-cursor--flight-max 96
  "Set the hard limit on frames per animation.

This matches SMEAR_MAX_FRAMES in smear-cursor-x11-play.h.")

(defcustom smear-cursor-long-jump-rows 12
  "Set the row distance that makes a movement a long jump.

See `smear-cursor-long-jump-duration'."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-long-jump-duration 0.85
  "Limit long jump duration in seconds, or use nil for the ordinary limit.

Long jumps can last longer than `smear-cursor-max-duration'."
  :type '(choice (const :tag "same as any other move" nil) number)
  :group 'smear-cursor)

(defun smear-cursor--long-jump-p (anim)
  "Return non-nil when ANIM moves far enough to use the long jump duration."
  (let ((lh (max 1 (or (smear-cursor--anim-lh anim) 1)))
        (corners (smear-cursor--anim-corners anim))
        (target (smear-cursor--anim-target anim)))
    (and smear-cursor-long-jump-duration corners target
         (>= (/ (abs (- (aref target 1) (aref corners 1))) (float lh))
             smear-cursor-long-jump-rows))))

(defun smear-cursor--flight-limit (&optional anim)
  "Return the maximum frame count for an animation.

Use the longer duration for ANIM when it is a long jump.  See
`smear-cursor-long-jump-duration'."
  (let ((secs (or (and anim (smear-cursor--long-jump-p anim)
                       smear-cursor-long-jump-duration)
                  smear-cursor-max-duration 0.45)))
    (min smear-cursor--flight-max
         (max 1 (round (* secs (max 1 smear-cursor-fps)))))))

(defun smear-cursor--flight-stride (nlayers)
  "Return the number of values per frame for NLAYERS layers.

Count four box coordinates, then eight corners, two head coordinates, and
an opacity scale per layer."
  (+ 4 (* nlayers 11)))

(defun smear-cursor--flight-step (anim)
  "Advance ANIM's spring by one frame and return the remaining distance.

The player controls the frame rate, so the time step is exactly one frame."
  (let ((dist (smear-cursor--ease-step
               (smear-cursor--anim-corners anim)
               (smear-cursor--anim-velocities anim)
               (smear-cursor--anim-target anim)
               (smear-cursor--spring :head smear-cursor-stiffness-head)
               (smear-cursor--spring :tail smear-cursor-stiffness-tail)
               (smear-cursor--spring :damping smear-cursor-damping)
               (smear-cursor--spring :exponent smear-cursor-trailing-exponent)
               1.0
               (smear-cursor--overshoot-cap (smear-cursor--anim-cw anim)
                                            (smear-cursor--anim-lh anim))))
        (cap (smear-cursor--max-length-px nil (smear-cursor--anim-cw anim))))
    (when cap
      (smear-cursor--shorten-corners (smear-cursor--anim-corners anim)
                                     (smear-cursor--anim-target anim) cap))
    dist))

(defun smear-cursor--flight-springs (anim limit)
  "Advance ANIM's spring for up to LIMIT frames.

Return (N SPRINGS), where N is the frame count and SPRINGS stores (CORNERS
. VELOCITIES) for each frame for later retargeting."
  (let ((springs (make-vector limit nil))
        (n 0))
    (catch 'settled
      (dotimes (k limit)
        (let ((dist (smear-cursor--flight-step anim)))
          (aset springs k
                (cons (copy-sequence (smear-cursor--anim-corners anim))
                      (copy-sequence (smear-cursor--anim-velocities anim))))
          (setq n (1+ k))
          (when (< dist smear-cursor--eps) (throw 'settled n)))))
    (list n springs)))

(defun smear-cursor--springs-history (springs k)
  "Return earlier shapes at frame K of SPRINGS, newest first.

Frame K-1 is the shape from one frame earlier."
  (cl-loop for i from 1 to (min k smear-cursor--history-max)
           collect (copy-sequence (car (aref springs (- k i))))))

(defun smear-cursor--flight-write (out at box shapes head &optional alphas heads)
  "Write BOX, SHAPES and HEAD into vector OUT starting at AT.

ALPHAS gives each layer's opacity scale for this frame, defaulting to
one.  HEADS gives a head per layer, for layers drawn off centre, and
falls back to HEAD."
  (dotimes (i 4) (aset out (+ at i) (float (nth i box))))
  (let ((k (+ at 4)) (i 0))
    (dolist (shape shapes)
      (let ((h (or (nth i heads) head)))
        (dotimes (c 8) (aset out (+ k c) (float (aref shape c))))
        (aset out (+ k 8) (float (aref h 0)))
        (aset out (+ k 9) (float (aref h 1)))
        (aset out (+ k 10) (float (if alphas (nth i alphas) 1.0))))
      (setq k (+ k 11) i (1+ i)))))

(defun smear-cursor--flight-frames (anim win layers springs n head)
  "Pack N frames of SPRINGS for ANIM in WIN into a playback vector.

LAYERS gives the style's layers and HEAD is the fixed brightest position.
Only the quadrilaterals move."
  (let* ((stride (smear-cursor--flight-stride (length layers)))
         (out (make-vector (* n stride) 0.0))
         ;; Compute once per animation.  See `smear-cursor-x11--frame-offset'.
         (off (smear-cursor-x11--frame-offset win))
         (dx (car off)) (dy (cdr off)))
    ;; Keep history restored from the displayed frame during retargeting.
    ;; Clearing it would restart echoes on every scroll movement.
    (dotimes (k n)
      ;; Copy corners so stepping the next animation cannot overwrite stored
      ;; frames needed for retargeting.
      (setf (smear-cursor--anim-corners anim)
            (copy-sequence (car (aref springs k))))
      (let* ((shapes (mapcar
                      (lambda (layer)
                        (smear-cursor-x11--corners-offset
                         (smear-cursor--layer-corners anim layer) dx dy))
                      layers))
             (box (smear-cursor--trail-bbox layers shapes head)))
        (when box
          (smear-cursor--flight-write out (* k stride) box shapes head)))
      (smear-cursor--remember-shape anim))
    out))

(defun smear-cursor--flight-layers (anim style layers)
  "Convert STYLE's LAYERS for ANIM to fixed module layer data.

Colors, opacity stops, expansion, blur and shape stay fixed during
playback.  Corners and head coordinates are supplied per frame."
  (let ((color (smear-cursor--anim-color anim))
        (zeros (make-vector 8 0.0))
        (nohead (vector 0.0 0.0)))
    (vconcat (mapcar (lambda (layer)
                       (smear-cursor--layer-vector
                        layer zeros nohead
                        (smear-cursor--layer-color layer style color)))
                     layers))))

(defun smear-cursor--note-handover (took)
  "Count a rejected animation when the module's result TOOK is nil."
  (unless took (setq smear-cursor--refused (1+ smear-cursor--refused)))
  took)

(defun smear-cursor--x11-player (stage)
  "Ensure STAGE has a player thread and return non-nil if it does."
  (and stage
       (fboundp 'smear-cursor-x11--play-start)
       (eq t (ignore-errors (smear-cursor-x11--play-start stage)))))

(defun smear-cursor--x11-resume (anim)
  "Restore ANIM's spring to the frame reached by the player thread."
  (let ((stage (smear-cursor--anim-x11 anim))
        (springs (smear-cursor--anim-springs anim)))
    (when (and stage springs (fboundp 'smear-cursor-x11--play-position))
      (let ((k (ignore-errors
                 (smear-cursor-x11--play-position
                  stage smear-cursor--track-trail))))
        (when (and (integerp k) (> k 0) (< k (length springs))
                   (aref springs k))
          (setf (smear-cursor--anim-corners anim)
                (copy-sequence (car (aref springs k)))
                (smear-cursor--anim-velocities anim)
                (copy-sequence (cdr (aref springs k)))
                (smear-cursor--anim-history anim)
                (smear-cursor--springs-history springs k)))))))

(defun smear-cursor--fly-x11 (anim)
  "Compute ANIM's frames and pass them to the X11 player thread.

Return non-nil when the thread accepts them; playback then needs no Lisp
frame timer."
  (let ((stage (smear-cursor--x11-stage anim)))
    ;; Trace playback failures here.  The sample trace only records the
    ;; decision to animate, so it can show "-> smear" without a visible trail.
    (unless stage
      (smear-cursor--trace "%-28s no stage -- falling back to the cell painter"
                           (or smear-cursor--last-command "-")))
    (when (and stage (not (smear-cursor--x11-player stage)))
      (smear-cursor--trace "%-28s stage but no player thread"
                           (or smear-cursor--last-command "-")))
    (when (and stage (smear-cursor--x11-player stage))
      (let* ((win (smear-cursor--anim-window anim))
             (style (smear-cursor-trail smear-cursor-trail-style))
             (layers (plist-get style :layers))
             (head (smear-cursor-x11--head-of
                    win (smear-cursor--anim-target anim)))
             (flight (smear-cursor--flight-springs
                      anim (smear-cursor--flight-limit anim)))
             (n (nth 0 flight))
             (springs (nth 1 flight)))
        (when (> n 0)
          (let ((frames (smear-cursor--flight-frames
                         anim win layers springs n head)))
            (setf (smear-cursor--anim-springs anim) springs
                  (smear-cursor--anim-x11 anim) stage
                  (smear-cursor--anim-paint-frames anim) n)
            (let ((took (smear-cursor--note-handover
                         (smear-cursor-x11--play
                          stage smear-cursor--track-trail
                          (smear-cursor--flight-layers anim style layers)
                          frames (float (max 1 smear-cursor-fps))))))
              (smear-cursor--trace
               "%-28s handed over %d frame(s) on track %d -> %s"
               (or smear-cursor--last-command "-") n
               smear-cursor--track-trail
               (if took "taken" "REFUSED")))
            (smear-cursor--x11-schedule-end anim n)
            t))))))

(defun smear-cursor--x11-schedule-end (anim frames)
  "Schedule cleanup of ANIM after FRAMES frames have played.

The thread removes its own overlay; the timer clears Lisp state."
  (when (timerp (smear-cursor--anim-end-timer anim))
    (cancel-timer (smear-cursor--anim-end-timer anim)))
  (setf (smear-cursor--anim-end-timer anim)
        (run-at-time (+ (/ (float frames) (max 1 smear-cursor-fps)) 0.05)
                     nil #'smear-cursor--stop)))

(defun smear-cursor--play-stats (stage)
  "Return a timing plist for the last animation drawn by STAGE's thread."
  (when (and stage (fboundp 'smear-cursor-x11--play-stats))
    (let ((v (ignore-errors (smear-cursor-x11--play-stats stage))))
      ;; Accept six or seven elements because a module loads only once per
      ;; Emacs session and may lack the drain field.  Rejecting six elements
      ;; reports "no smear yet" after a visible trail.  A missing drain value
      ;; is reported as "not measured".
      (when (and (vectorp v) (>= (length v) 6) (> (aref v 0) 0))
        (list :frames (aref v 0)
              :paint-total (aref v 1)
              :gap-max (aref v 2)
              :gap-max-at (aref v 3)
              :gap-sum (aref v 4)
              :gaps (append (aref v 5) nil)
              :drain (and (>= (length v) 7) (aref v 6))
              :drain-age (and (>= (length v) 8) (aref v 7)))))))

(defun smear-cursor--paint-frame (anim)
  "Paint one frame of ANIM with the configured backend.

Fall back to the Lisp renderer when the selected backend is unavailable."
  (cond
   ((eq smear-cursor-backend 'x11)
    (or (smear-cursor--paint-frame-x11 anim)
        (smear-cursor--paint-frame-cpu anim)))
   (t (smear-cursor--paint-frame-cpu anim))))

;;;; Effects: highlights for edits and cursor destinations

;; Effects highlight copied or deleted text and the destination line.
;; They use the same player thread and renderers as trails, with fixed
;; geometry and changing opacity.
;;
;; Region shapes follow the partial first and last lines.  A bounding
;; box would highlight text outside the region on those lines.

(defvar smear-cursor--effects (make-hash-table :test 'eq)
  "Store effects by name.

See `smear-cursor-define-effect'.")

(defconst smear-cursor--effect-max-layers 32
  "Limit the number of layers drawn for one effect.

The module's ceiling, which it may report less than: the shader's
share is settled against the graphics driver, whose guaranteed uniform
space runs to sixteen layers and no further.  Ask
`smear-cursor--max-layers' rather than this.

Thirty-two rather than sixteen because sixteen was the whole budget
for an effect, and a figure drawn from a sprite is a dozen of them on
its own -- which left Pacman four stretches of text to eat with, and
he had to give one up to take another.")

(defconst smear-cursor--effect-max-layers-was 8
  "What the module held before the limit was raised.

A module loaded before an update is still the one running, and it
refuses a flight asking for more layers than it holds, so an effect
built to the new limit would not appear at all.")

(defun smear-cursor--max-layers ()
  "Return how many layers an effect may use.

Ask the module rather than assume: the one loaded in a running Emacs
is whatever was built when it started, and a flight over its limit is
refused whole.  Fewer layers is a plainer effect; a refused flight is
no effect at all."
  (min smear-cursor--effect-max-layers
       (if (fboundp 'smear-cursor-x11--max-layers)
           (funcall 'smear-cursor-x11--max-layers)
         smear-cursor--effect-max-layers-was)))

(defun smear-cursor-define-effect (name &rest plist)
  "Define effect NAME from PLIST or a function returning a plist.

PLIST accepts these keys:
  :doc       Purpose of the effect.
  :duration  Duration in seconds.
  :color     [R G B] or a color name.
  :layers    Trail-style layers, each with an :envelope of
             ((AT . SCALE) ...) over time from 0 to 1.
  :shape     `region' for the supplied rectangles or `point' for a
             single mark at the cursor.
  :play      A function of (WIN RECTS) that plays the effect itself,
             for one that is not a shape drawn over the text but
             something the package already does -- the trail thrown
             about, say.  An effect with this draws no layers.

A single function argument is called each time the effect plays, so its
options can be read at playback time."
  (puthash name (if (and (null (cdr plist)) (functionp (car plist)))
                    (car plist)
                  plist)
           smear-cursor--effects)
  name)

(defun smear-cursor-effect (name)
  "Return effect NAME, or nil if undefined."
  (let ((e (and name (gethash name smear-cursor--effects))))
    (if (functionp e) (funcall e) e)))

(defcustom smear-cursor-effect-strength 1.0
  "Scale the strength of every effect by this multiplier.

Opacity cannot exceed one, regardless of the multiplier."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-effect-color nil
  "Override the color of every effect.

Use nil for each effect's own color, or `trail' for the current trail
color."
  :type '(choice (const :tag "each effect's own" nil)
                 (const :tag "the trail's" trail)
                 color)
  :group 'smear-cursor)

(defun smear-cursor--envelope-at (env u)
  "Return the value of envelope ENV at normalized time U.

ENV is ((AT . SCALE) ...) in increasing AT order.  Use endpoint values
outside the range."
  (let ((prev (car env)) (out nil))
    (cond
     ((null env) 1.0)
     ((<= u (car (car env))) (cdr (car env)))
     (t (dolist (stop (cdr env))
          (when (and (not out) (<= u (car stop)))
            (let ((span (max 1e-6 (- (car stop) (car prev)))))
              (setq out (+ (cdr prev)
                           (* (- (cdr stop) (cdr prev))
                              (/ (- u (car prev)) span))))))
          (unless out (setq prev stop)))
        (or out (cdr (car (last env))))))))

(defun smear-cursor--effect-layer (layer)
  "Prepare LAYER's opacity stops for an effect.

Quadrilaterals have uniform opacity, while radial shapes keep a gentler
fade than a trail.  Trail opacity falls to one twelfth at the tail."
  (if (plist-get layer :stops)
      layer
    (let* ((a (or (plist-get layer :alpha) 0.7))
           (stops (if (eq (plist-get layer :shape) 'radial)
                      ;; Out to nothing at the rim.  A gradient that
                      ;; ends part way up is not painted past its last
                      ;; stop, so a glow ending at half opacity draws a
                      ;; hard circle at that radius.
                      (list (cons 0.0 a) (cons 0.45 (* a 0.55)) (cons 1.0 0.0))
                    (list (cons 0.0 a) (cons 1.0 a)))))
      (append (list :stops stops) layer))))

(defcustom smear-cursor-noise-seed 1
  "Choose which arrangement the noise in an effect takes.

Any whole number.  The same seed gives the same shape every time, so
an effect looks the same from one keystroke to the next.  Change it
for another arrangement of the same effect.  `smear-cursor-menu-noise'
steps through seeds with the preview panel open."
  :type 'integer
  :group 'smear-cursor)

(defun smear-cursor--noise (seed i)
  "Return a repeatable number from -1 to 1 for SEED at index I.

`random' would do as well, except that it walks a state of its own, so
one seed would not give one shape twice.

Mixed rather than stepped.  One pass of a linear generator leaves the
result linear in SEED: every index moves by the same amount, so the
next seed is the same arrangement slid sideways, and it comes back
round within a few presses.  Multiplying and folding the high bits
down twice scatters neighbouring seeds instead.  Everything stays
inside a fixnum: 24 bits of state times a 32-bit constant is 56."
  (let* ((x (logand (+ (* seed 2654435761) (* (1+ i) 40503)) #xFFFFFF))
         (x (logand (* (logxor x (ash x -7)) 2246822519) #xFFFFFF))
         (x (logand (* (logxor x (ash x -11)) 3266489917) #xFFFFFF))
         (x (logxor x (ash x -9))))
    (- (/ x 8388607.5) 1.0)))

(defun smear-cursor--radial-cluster (radius alpha noise seed envelope)
  "Return the layers of one round glow of RADIUS at opacity ALPHA.

NOISE from 0 to 1 breaks the circle up: at zero this is a single round
glow, and above it smaller glows are pushed off centre by up to NOISE
times the radius, which leaves an outline that is no longer a circle.
SEED chooses the arrangement and ENVELOPE is the opacity envelope they
all share."
  (let ((main `(:shape radial :alpha ,alpha :radius ,radius
                :envelope ,envelope)))
    (if (<= noise 0.0)
        (list main)
      (cons main
            (cl-loop
             for i below 3
             collect
             (let ((dx (* noise radius (smear-cursor--noise seed (* 2 i))))
                   (dy (* noise radius (smear-cursor--noise seed (1+ (* 2 i)))))
                   (r (* radius (+ 0.45 (* 0.35 (abs (smear-cursor--noise
                                                      seed (+ 8 i))))))))
               `(:shape radial
                 :alpha ,(* alpha 0.75)
                 :radius ,r
                 :offset ,(cons dx dy)
                 :envelope ,envelope)))))))

(defun smear-cursor--rect-grown (rect g)
  "Return RECT expanded by G pixels on every side."
  (vector (- (aref rect 0) g) (- (aref rect 1) g)
          (+ (aref rect 2) (* 2 g)) (+ (aref rect 3) (* 2 g))))

(defun smear-cursor--edge-rect (rect edge thickness)
  "Return one side of RECT as a band THICKNESS pixels wide.

EDGE is `top', `bottom', `left' or `right'.  Four such bands make a
ring with nothing in the middle, which is what leaves the character
under the cursor readable.  The left and right bands stop where the
top and bottom ones start, so the corners are not drawn twice and do
not come out brighter than the sides.  THICKNESS is held to half the
rectangle so the bands cannot meet in the middle."
  (let* ((x (aref rect 0)) (y (aref rect 1))
         (w (aref rect 2)) (h (aref rect 3))
         (tw (max 1.0 (min thickness (/ w 2.0))))
         (th (max 1.0 (min thickness (/ h 2.0))))
         (mid (max 1.0 (- h (* 2 th)))))
    (pcase edge
      ('top    (vector x y w th))
      ('bottom (vector x (+ y h (- th)) w th))
      ('left   (vector x (+ y th) tw mid))
      ('right  (vector (+ x w (- tw)) (+ y th) tw mid))
      (_ (error "No such edge: %S" edge)))))

(defun smear-cursor--envelope-max (env)
  "Return the largest value in envelope ENV, or zero when it has none."
  (if env (apply #'max (mapcar #'cdr env)) 0.0))

(defun smear-cursor--effect-moving-p (effect)
  "Return non-nil when any layer of EFFECT changes shape or size over time."
  (cl-some (lambda (layer)
             (or (plist-get layer :grow-envelope)
                 (plist-get layer :quads)
                 (plist-get layer :offsets)))
           (plist-get effect :layers)))

(defun smear-cursor--layer-rect (layer rect u)
  "Return the rectangle LAYER covers over RECT at normalized time U.

Grow RECT by the layer's `:grow-envelope', then take the `:edge' band
of the result when the layer draws one side of a ring."
  (let* ((env (plist-get layer :grow-envelope))
         (r (if env
                (smear-cursor--rect-grown
                 rect (smear-cursor--envelope-at env u))
              rect))
         (edge (plist-get layer :edge)))
    (if edge
        (smear-cursor--edge-rect r edge (or (plist-get layer :thickness) 2.0))
      r)))

(defun smear-cursor--layer-anchor (layer rect)
  "Return the y in RECT that LAYER\='s own corners are measured from.

The middle of the cell, or its foot when the layer is anchored to
`bottom'.  Fire stands on the character; a bolt lands on it."
  (+ (aref rect 1)
     (if (eq (plist-get layer :anchor) 'bottom)
         (aref rect 3)
       (/ (aref rect 3) 2.0))))

(defun smear-cursor--layer-phase (layer u)
  "Return the shape LAYER holds at normalized time U, if it holds several.

A layer with `:quads' carries one shape for each phase of its life and
changes to the next as it plays, which is what makes a spark crackle
rather than sit still and fade."
  (let ((quads (plist-get layer :quads)))
    (when (and quads (> (length quads) 0))
      (aref quads (min (1- (length quads))
                       (max 0 (floor (* u (length quads)))))))))

(defun smear-cursor--effect-corners (layer rect u)
  "Return the eight corners LAYER covers over RECT at time U.

A layer carrying its own `:quad' is placed around the middle of RECT.
That is how a shape which is not a rectangle is expressed, a bolt of
lightning being the one that needs it."
  (let ((quad (or (plist-get layer :quad)
                  (smear-cursor--layer-phase layer u)))
        (c (make-vector 8 0.0)))
    (if (not quad)
        (progn (smear-cursor--corners-from-rect
                c (smear-cursor--layer-rect layer rect u))
               c)
      (let ((cx (+ (aref rect 0) (/ (aref rect 2) 2.0)))
            (cy (smear-cursor--layer-anchor layer rect)))
        (dotimes (i 4)
          (aset c (* 2 i) (+ cx (aref quad (* 2 i))))
          (aset c (1+ (* 2 i)) (+ cy (aref quad (1+ (* 2 i))))))
        c))))

(defun smear-cursor--layer-bounds (layer rect)
  "Return the area LAYER covers over RECT, before blur and glow.

RECT itself, unless the layer carries its own corners, in which case
the box has to reach wherever those go."
  (let* ((quads (append (plist-get layer :quads) nil))
         (all (if (plist-get layer :quad)
                  (cons (plist-get layer :quad) quads)
                quads)))
    (if (null all)
        rect
      ;; Every phase, since the box is fixed for the whole flight and a
      ;; later phase reaching further would be cut off by one measured
      ;; from the first.
      (let* ((cx (+ (aref rect 0) (/ (aref rect 2) 2.0)))
             (cy (smear-cursor--layer-anchor layer rect))
             (xs nil) (ys nil))
        (dolist (quad all)
          (dotimes (i 4)
            (push (+ cx (aref quad (* 2 i))) xs)
            (push (+ cy (aref quad (1+ (* 2 i)))) ys)))
        (vector (apply #'min xs) (apply #'min ys)
                (- (apply #'max xs) (apply #'min xs))
                (- (apply #'max ys) (apply #'min ys)))))))

(defun smear-cursor--effect-heads (pairs head u)
  "Return where each layer in PAIRS is drawn from at time U.

HEAD is the effect\='s own centre, which a layer may be moved from."
  (mapcar (lambda (pair)
            (smear-cursor--layer-head (car pair) head (cdr pair) u))
          pairs))

(defun smear-cursor--effect-shapes (pairs u)
  "Return corner vectors for layer and rectangle PAIRS at time U."
  (mapcar (lambda (pair)
            (smear-cursor--effect-corners (car pair) (cdr pair) u))
          pairs))

(defun smear-cursor--effect-still-p (effect)
  "Return non-nil when EFFECT can be drawn once and stamped again.

Such effects can be rendered and uploaded once, then drawn with one
varying opacity.  That asks two things of the layers: one opacity
envelope between them, and no change of size, since a stamp holds the
size it was drawn at."
  (let ((layers (plist-get effect :layers)))
    (and (not (smear-cursor--effect-moving-p effect))
         (or (null (cdr layers))
             (let ((first (plist-get (car layers) :envelope)))
               (cl-every (lambda (layer)
                           (equal first (plist-get layer :envelope)))
                         (cdr layers)))))))

(defun smear-cursor--effect-scale (layer u)
  "Return LAYER's envelope at time U with the global effect strength.

Limit the scale so layer opacity cannot exceed one."
  (let ((base (max 1e-6 (or (plist-get layer :alpha) 0.7)))
        (env (smear-cursor--envelope-at (plist-get layer :envelope) u)))
    (min (/ 1.0 base) (* env (max 0.0 smear-cursor-effect-strength)))))

(defun smear-cursor--rects-between (beg end left right)
  "Return rectangles covering the text from rectangle BEG to rectangle END.

LEFT and RIGHT bound the text area.  Return one rectangle for a single
row, or the first row's end, intervening rows, and the last row's start."
  (let ((by (aref beg 1)) (ey (aref end 1))
        (h (aref beg 3)))
    (cond
     ((= by ey)
      (list (vector (aref beg 0) by
                    (max 1.0 (- (aref end 0) (aref beg 0))) h)))
     (t
      (let ((rects (list (vector (aref beg 0) by
                                 (max 1.0 (- right (aref beg 0))) h))))
        (when (> (- ey by) h)
          (push (vector left (+ by h) (- right left) (- ey by h)) rects))
        (push (vector left ey (max 1.0 (- (aref end 0) left)) (aref end 3))
              rects)
        (nreverse rects))))))

(defun smear-cursor--effect-layers (effect rects)
  "Return EFFECT\='s layer and rectangle pairs for RECTS.

Limit the count to what the module holds, keeping the last of them.
Layers are drawn in the order they are given, so the last are the ones
on top: for Pacman that is Pacman himself, over the text he has eaten.
Dropping those instead leaves the hole in the text and takes away the
character who is supposed to be eating it."
  (let ((out nil))
    (dolist (rect rects)
      (dolist (layer (plist-get effect :layers))
        (push (cons layer rect) out)))
    (setq out (nreverse out))
    (let ((limit (smear-cursor--max-layers)))
      (if (<= (length out) limit)
          out
        (last out limit)))))

(defun smear-cursor--effect-frames (effect rects nframes)
  "Pack NFRAMES frames of EFFECT over RECTS into a playback vector.

Only opacity changes, unless a layer carries a `:grow-envelope', in
which case its corners are worked out afresh for every frame."
  (let* ((pairs (smear-cursor--effect-layers effect rects))
         (n (length pairs))
         (stride (smear-cursor--flight-stride n))
         (out (make-vector (* nframes stride) 0.0))
         (moving (smear-cursor--effect-moving-p effect))
         (fixed (unless moving (smear-cursor--effect-shapes pairs 0.0)))
         (box (smear-cursor--effect-box pairs))
         (head (smear-cursor--effect-head rects))
         (steady (unless moving
                   (smear-cursor--effect-heads pairs head 0.0))))
    (dotimes (k nframes)
      (let* ((u (/ (float k) (max 1 (1- nframes))))
             (shapes (or fixed (smear-cursor--effect-shapes pairs u)))
             (heads (or steady (smear-cursor--effect-heads pairs head u)))
             (alphas (mapcar
                      (lambda (pair) (smear-cursor--effect-scale (car pair) u))
                      pairs))
             ;; Tight around this frame when anything moves, and the
             ;; one box for the whole flight when nothing does.
             (frame-box (if moving
                            ;; Nothing drawn this frame wants nothing
                            ;; composited: the cursor's own cell, not
                            ;; the route it will take later.
                            (or (smear-cursor--effect-frame-box
                                 pairs shapes heads alphas)
                                (smear-cursor--effect-box
                                 (list (car pairs))))
                          box)))
        (smear-cursor--flight-write out (* k stride) frame-box shapes head
                                    alphas heads)))
    out))

(defun smear-cursor--effect-frame-box (pairs shapes heads alphas)
  "Return the box one frame of PAIRS needs, from its SHAPES and HEADS.

ALPHAS says what each layer is drawing this frame; one drawing nothing
is left out.  The module lays the window back down over the box and
composites it for every frame, and the overlay shows only what the box
covers, so a box around a whole route costs the route on every frame
of it.  Trails have always carried a box per frame; an effect that
moves needs one too."
  (let ((x0 1e30) (y0 1e30) (x1 -1e30) (y1 -1e30))
    (cl-loop
     for pair in pairs
     for shape in shapes
     for head in heads
     for alpha in alphas
     do (let* ((layer (car pair))
               (pad (+ 2 (abs (or (plist-get layer :grow) 0))
                       (* 2 (or (plist-get layer :blur) 0))))
               (radius (or (plist-get layer :radius) 0)))
          (when (> alpha 0.004)
            (if (eq (plist-get layer :shape) 'radial)
                ;; Drawn around its head, wherever its corners are.
                (setq x0 (min x0 (- (aref head 0) radius pad))
                      x1 (max x1 (+ (aref head 0) radius pad))
                      y0 (min y0 (- (aref head 1) radius pad))
                      y1 (max y1 (+ (aref head 1) radius pad)))
              (dotimes (i 4)
                (let ((x (aref shape (* 2 i)))
                      (y (aref shape (1+ (* 2 i)))))
                  (setq x0 (min x0 (- x pad)) x1 (max x1 (+ x pad))
                        y0 (min y0 (- y pad)) y1 (max y1 (+ y pad)))))))))
    (when (< x0 x1)
      (list (floor x0) (floor y0) (ceiling (- x1 x0)) (ceiling (- y1 y0))))))

(defun smear-cursor--effect-box (pairs)
  "Return the bounding rectangle for PAIRS, including blur and glow."
  (let ((x0 1e30) (y0 1e30) (x1 -1e30) (y1 -1e30))
    (dolist (pair pairs)
      (let* ((layer (car pair))
             (r (smear-cursor--layer-bounds (car pair) (cdr pair)))
             (pad (+ 2 (abs (or (plist-get layer :grow) 0))
                     ;; A layer that breathes is at its widest somewhere in
                     ;; the middle of the flight, and the box has to hold
                     ;; that frame rather than the first one.
                     (smear-cursor--envelope-max
                      (plist-get layer :grow-envelope))
                     (* 2 (or (plist-get layer :blur) 0))
                     (or (plist-get layer :radius) 0)
                     ;; A glow pushed off centre reaches that much
                     ;; further than its radius, and one that rises as
                     ;; it plays reaches as far as its furthest phase.
                     (let ((offs (append (plist-get layer :offsets) nil))
                           (off (plist-get layer :offset))
                           (far 0))
                       (dolist (o (if off (cons off offs) offs))
                         (setq far (max far (abs (car o)) (abs (cdr o)))))
                       far))))
        (setq x0 (min x0 (- (aref r 0) pad))
              y0 (min y0 (- (aref r 1) pad))
              x1 (max x1 (+ (aref r 0) (aref r 2) pad))
              y1 (max y1 (+ (aref r 1) (aref r 3) pad)))))
    (if (< x0 x1)
        (list (floor x0) (floor y0) (ceiling (- x1 x0)) (ceiling (- y1 y0)))
      (list 0 0 0 0))))

(defun smear-cursor--layer-offset (layer u)
  "Return how far LAYER is drawn from its head at normalized time U.

A layer with `:offsets' carries one for each phase of its life and
moves to the next as it plays, which is how an ember rises.  A plain
`:offset' does not move."
  (let ((offs (plist-get layer :offsets)))
    (or (and offs (> (length offs) 0)
             (aref offs (min (1- (length offs))
                             (max 0 (floor (* u (length offs)))))))
        (plist-get layer :offset))))

(defun smear-cursor--layer-head (layer head rect &optional u)
  "Return HEAD for LAYER over RECT at normalized time U.

Moved by the layer\='s offset for that moment, and dropped to the foot
of RECT when it is anchored to `bottom', so a glow sits where the
shapes that share its anchor do."
  (let ((off (smear-cursor--layer-offset layer (or u 0.0)))
        (y (if (eq (plist-get layer :anchor) 'bottom)
               (smear-cursor--layer-anchor layer rect)
             (aref head 1))))
    (vector (+ (aref head 0) (if off (car off) 0.0))
            (+ y (if off (cdr off) 0.0)))))

(defun smear-cursor--effect-head (rects)
  "Return the center for a radial effect over RECTS."
  (let ((r (car rects)))
    (if r
        (vector (+ (aref r 0) (/ (aref r 2) 2.0))
                (+ (aref r 1) (/ (aref r 3) 2.0)))
      (vector 0.0 0.0))))

;;; Built-in effects

(smear-cursor-define-effect 'region-arrive
  :doc "A region arriving: what was just pasted.

Green, the colour a diff gives to text that has appeared -- which is
what a paste is, and is read that way without having to be learned.
The deletion beside it is red for the same reason, and the copy, which
changes nothing, is neither."
  :duration 0.30
  :color [120 235 160]
  :shape 'region
  :layers '((:shape quad :alpha 0.5 :grow 1 :blur 4
             :envelope ((0.0 . 0.0) (0.12 . 1.0) (0.45 . 0.85) (1.0 . 0.0)))))

(defcustom smear-cursor-pulse-duration 0.4
  "Set a line pulse's duration in seconds at the ordinary trail speed.

`smear-cursor--trail-linger' scales this duration for the current trail."
  :type 'number
  :group 'smear-cursor)

(defun smear-cursor--trail-linger ()
  "Return the current trail's duration multiplier around one.

Use tail stiffness and bound the result in both directions."
  (let ((tail (max 0.01 (smear-cursor--spring
                         :tail smear-cursor-stiffness-tail))))
    (min 1.5 (max 0.5 (/ 0.2 tail)))))

(defcustom smear-cursor-pulse-strength 0.45
  "Set a line pulse's peak opacity as a fraction of the trail's peak.

Turn it up to make the pulse say where the cursor is rather than tint
the line it is on.  It stops short of solid whatever this says: the
point of compositing over the text rather than replacing its face is
that the text stays readable, and an opaque wash gives that away for
nothing."
  :type 'number
  :group 'smear-cursor)

(defun smear-cursor--trail-strongest ()
  "Return the current trail's maximum opacity, from 0 to 1.

Read each layer's stops through `smear-cursor--layer-stops'."
  (let ((layers (plist-get (smear-cursor-trail smear-cursor-trail-style)
                           :layers))
        (best 0.0))
    (dolist (layer layers best)
      (dolist (stop (smear-cursor--layer-stops layer))
        (setq best (max best (cdr stop)))))))

(defun smear-cursor--trail-blur ()
  "Return the largest blur radius in the current trail, in pixels."
  (let ((layers (plist-get (smear-cursor-trail smear-cursor-trail-style)
                           :layers))
        (blur 0))
    (dolist (layer layers blur)
      (setq blur (max blur (or (plist-get layer :blur) 0))))))

(defun smear-cursor--trail-core-color ()
  "Return the current trail's brightest layer color, or nil.

Use only colors assigned to individual layers.  Return nil if none has its
own color."
  (let ((layers (plist-get (smear-cursor-trail smear-cursor-trail-style)
                           :layers))
        (best 0.0) (core nil))
    (dolist (layer layers core)
      (let ((own (plist-get layer :color))
            (peak (apply #'max (mapcar #'cdr
                                       (smear-cursor--layer-stops layer)))))
        (when (and own (> peak best))
          (setq best peak core own))))))

(defcustom smear-cursor-pulse-flashes 1
  "How many times a line pulse flashes before it goes out.

One is a single wash over the line.  More turns it into a blink, at
whatever rate `smear-cursor-pulse-duration' divides into: three over
four tenths of a second is about seven a second, which is quick enough
to read as an alarm rather than as a highlight.

Worth a thought above three, both because the eye stops resolving
separate flashes and because rapid flashing is worth being sparing
with in something that runs unbidden."
  :type 'integer
  :group 'smear-cursor)

(defun smear-cursor--pulse-core-inset ()
  "Return the pulse core's inset from the line edge in pixels.

Scale the inset with line height."
  (max 2 (round (* 0.3 (if (fboundp 'default-line-height)
                           (default-line-height)
                         (frame-char-height))))))

(defun smear-cursor--pulse-flash-count ()
  "Return how many times a pulse can flash and be seen.

A flash needs a frame lit and a frame dark, so half the frames a pulse
is drawn in is the most it can show.  Asked for more, the extra fall
between one frame and the next and are never drawn: what reaches the
screen is not a faster blink but a shimmer of whichever happened to
land on a frame."
  (let* ((rate (smear-cursor--effect-rate))
         (frames (max 2 (round (* smear-cursor-pulse-duration
                                  (smear-cursor--trail-linger) rate)))))
    (max 1 (min (max 1 smear-cursor-pulse-flashes) (/ frames 2)))))

(defun smear-cursor--pulse-envelope (flashes)
  "Return the opacity envelope for a pulse of FLASHES flashes.

Each flash rises early, holds most of its height, and falls away
before the next, so what is drawn is a blink rather than a wobble.
They dim as they go: a pulse is one thing settling, not a light left
switched on.

A single flash holds its peak for longer.  Without the hold only three
frames of thirty are near full brightness and the whole thing looks
faint."
  (if (<= flashes 1)
      '((0.0 . 0.0) (0.10 . 1.0) (0.45 . 0.9) (1.0 . 0.0))
    (append
     (cl-loop
      for i below flashes
      append (let* ((span (/ 1.0 flashes))
                    (from (* i span))
                    (peak (- 1.0 (* 0.5 (/ (float i) flashes)))))
               (list (cons from 0.0)
                     (cons (+ from (* span 0.30)) peak)
                     (cons (+ from (* span 0.55)) (* peak 0.9)))))
     '((1.0 . 0.0)))))

(defun smear-cursor--line-pulse ()
  "Build the line pulse effect using the current trail's color and speed.

Blend over the line without changing text faces.  Read options when the
effect plays."
  (list :doc "A wash over the line, in the trail's colour and pace."
        :duration (* smear-cursor-pulse-duration
                     (smear-cursor--trail-linger))
        :color 'trail
        :shape 'region
        :layers
        (let* ((env (smear-cursor--pulse-envelope
                     (smear-cursor--pulse-flash-count)))
               (alpha (min 0.85 (* (smear-cursor--trail-strongest)
                                   (max 0.0 smear-cursor-pulse-strength))))
               (blur (smear-cursor--trail-blur))
               (core (smear-cursor--trail-core-color))
               (beam `(:shape quad :grow 1 :alpha ,alpha :blur ,blur
                       :envelope ,env)))
          (if (not core)
              (list beam)
            ;; Draw a brighter inset band in the style's centre colour.
            ;; Negative `:grow' shrinks all sides, narrowing the band and slightly
            ;; shortening its ends.
            (list beam
                  `(:shape quad
                    :grow ,(- (smear-cursor--pulse-core-inset))
                    :color ,core
                    :alpha ,(min 0.8 (* alpha 1.5))
                    :blur ,(max 1 (/ blur 3))
                    :envelope ,env))))))

(smear-cursor-define-effect 'line-pulse #'smear-cursor--line-pulse)


(smear-cursor-define-effect 'region-flash
  :doc "Briefly highlight the copied region.  The three region effects follow
diff colours: green for added text, red for deleted text, and blue for
copying, which changes nothing."
  :duration 0.26
  :color [120 190 255]
  :shape 'region
  :layers '((:shape quad :alpha 0.5 :grow 1 :blur 4
             :envelope ((0.0 . 0.0) (0.10 . 1.0) (0.40 . 0.8) (1.0 . 0.0)))))

(smear-cursor-define-effect 'region-fade
  :doc "A region going out: what was just deleted.
Held a moment at full and then let go, which reads as the text being
taken away rather than as something arriving."
  :duration 0.28
  :color [255 110 80]
  :shape 'region
  :layers '((:shape quad :alpha 0.58 :grow 3 :blur 6
             :envelope ((0.0 . 1.0) (0.30 . 0.9) (1.0 . 0.0)))))

(smear-cursor-define-effect 'spark-dot
  :doc "A dot at the character just typed, gone almost at once."
  :duration 0.16
  :color [255 240 232]
  :shape 'point
  :layers '((:shape radial :alpha 0.85 :radius 9
             :envelope ((0.0 . 1.0) (1.0 . 0.0)))))

(defcustom smear-cursor-type-blink-duration 0.22
  "Set how many seconds one typing blink lasts.

Longer durations allow several blinks to appear at once while typing."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-type-blink-strength 0.75
  "Set the typing blink's peak opacity from 0 to 1.

`smear-cursor-effect-strength' scales this along with other effects."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-type-blink-radius 13
  "Set the typing blink's radius in pixels."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-type-blink-color nil
  "Set the typing blink color, or use nil for the trail color."
  :type '(choice (const :tag "the trail's own" nil) color)
  :group 'smear-cursor)

(defcustom smear-cursor-type-blink-noise 0.45
  "Break the typing blink up, from 0 to 1.

Zero is one round glow.  Above that, smaller glows are pushed off
centre by up to this much of the radius, so the outline is no longer
a circle.  `smear-cursor-noise-seed' picks the arrangement."
  :type 'number
  :group 'smear-cursor)

(defun smear-cursor--type-blink ()
  "Build the typing blink effect from its current options."
  (list :doc "A soft blink at the character just typed."
        :duration smear-cursor-type-blink-duration
        :color (or smear-cursor-type-blink-color 'trail)
        :shape 'point
        :layers
        (smear-cursor--radial-cluster
         smear-cursor-type-blink-radius
         smear-cursor-type-blink-strength
         smear-cursor-type-blink-noise
         smear-cursor-noise-seed
         ;; Hold peak brightness.  Without a hold, only one of nine frames is
         ;; fully bright and the effect looks faint.  Fade in to avoid a sharp
         ;; flash on every keystroke.
         '((0.0 . 0.0) (0.12 . 1.0) (0.55 . 0.9) (1.0 . 0.0)))))

(smear-cursor-define-effect 'type-blink #'smear-cursor--type-blink)

;; The glow and the breath are the same shape: four bands around the
;; cursor cell, widening as they fade.  They differ in what sets them
;; off and in how long they take.

(defcustom smear-cursor-glow-duration 0.4
  "Set how many seconds one typing glow lasts."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-glow-reach 7.0
  "Set how far the typing glow travels from the cursor, in pixels."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-glow-thickness 2.0
  "Set the width of the ring in pixels.

A ring thicker than half the cursor closes over the character, so the
width is held to half the cell whatever this says."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-glow-blur 4
  "Set how far the ring is softened, in pixels.

Zero draws four hard bands.  Larger values spread the light further
out and are what make it read as a glow rather than a box."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-glow-strength 0.8
  "Set the typing glow's peak opacity from 0 to 1."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-glow-color nil
  "Set the colour of the ring, or use nil for the trail colour."
  :type '(choice (const :tag "the trail's own" nil) color)
  :group 'smear-cursor)

(defun smear-cursor--ring-layers (grow opacity alpha thickness blur)
  "Return the four sides of a ring as effect layers.

GROW is the growth envelope the sides share and OPACITY their opacity
envelope.  ALPHA is the peak opacity, THICKNESS the width of a side in
pixels, and BLUR how far it is softened."
  (mapcar (lambda (edge)
            `(:shape quad :edge ,edge :thickness ,thickness
              :alpha ,alpha :blur ,blur
              :grow-envelope ,grow :envelope ,opacity))
          '(top bottom left right)))

(defun smear-cursor--cursor-glow ()
  "Build the typing glow from its current options."
  (list :doc "A ring that widens out of the cursor and fades."
        :duration smear-cursor-glow-duration
        :color (or smear-cursor-glow-color 'trail)
        :shape 'point
        :layers (smear-cursor--ring-layers
                 `((0.0 . 0.0) (1.0 . ,smear-cursor-glow-reach))
                 '((0.0 . 0.0) (0.15 . 1.0) (0.5 . 0.75) (1.0 . 0.0))
                 smear-cursor-glow-strength
                 smear-cursor-glow-thickness
                 smear-cursor-glow-blur)))

(smear-cursor-define-effect 'cursor-glow #'smear-cursor--cursor-glow)

(defcustom smear-cursor-breathe-duration 1.1
  "Set how many seconds one resting breath lasts.

Keep this under `smear-cursor-idle-delay' so one breath finishes
before the next begins."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-breathe-reach 4.0
  "Set how far the resting breath travels from the cursor, in pixels."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-breathe-strength 0.45
  "Set the resting breath's peak opacity from 0 to 1.

Lower than the typing glow.  This one repeats for as long as the
cursor is left alone, and is meant to be noticed only if looked at."
  :type 'number
  :group 'smear-cursor)

(defun smear-cursor--cursor-breathe ()
  "Build the resting breath from its current options."
  (list :doc "A ring that widens and comes back while the cursor rests."
        :duration smear-cursor-breathe-duration
        ;; Half the frames of a trail.  This one plays while the machine
        ;; is otherwise idle, and a slow fade looks the same at thirty.
        :fps 30
        :color (or smear-cursor-glow-color 'trail)
        :shape 'point
        :layers (smear-cursor--ring-layers
                 `((0.0 . 0.0) (0.5 . ,smear-cursor-breathe-reach) (1.0 . 0.0))
                 '((0.0 . 0.0) (0.5 . 1.0) (1.0 . 0.0))
                 smear-cursor-breathe-strength
                 smear-cursor-glow-thickness
                 smear-cursor-glow-blur)))

(smear-cursor-define-effect 'cursor-breathe #'smear-cursor--cursor-breathe)

(defcustom smear-cursor-rest-duration 1.2
  "Set how many seconds one turn of the resting glow lasts."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-rest-strength 1.0
  "Scale the resting glow, as a multiple of the trail style\='s own rings.

One is the strength the style draws its head at, which is what the
cursor should be wearing: scaled down it read as a dimmer relative of
the trail rather than as the trail standing still.  Lower it for
something quieter to sit behind."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-rest-size 1.0
  "Scale the resting glow, as a multiple of the trail style\='s own rings."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-rest-dip 0.72
  "How far the resting glow falls between one breath and the next.

A share of its own strength: one holds still and nought goes out
altogether.  With `smear-cursor-rest-duration\=' this is the whole of the
breathing -- how deep, and how long a turn takes."
  :type 'number
  :group 'smear-cursor)

(defun smear-cursor--rest-envelope ()
  "Return the opacity the resting glow breathes through over one turn.

One envelope for every ring of it, because a still effect is one the
module renders once and stamps again at each opacity, and that asks
for a single opacity between the layers."
  (let ((low (max 0.0 (min 1.0 smear-cursor-rest-dip))))
    `((0.0 . ,low) (0.5 . 1.0) (1.0 . ,low))))

(defun smear-cursor--rest-layers ()
  "Return the resting glow\='s layers, taken from the trail style in use.

The rings a style draws at the head of a flight are what the cursor
should wear when the flight has stopped: the laser style is a hot
near-white dot inside a red bloom, and a glow invented separately is a
different thing that happens to sit in the same place.  A style with no
rings of its own gets one, sized to the line."
  (let* ((style (gethash smear-cursor-trail-style smear-cursor--trails))
         (rings (seq-filter (lambda (layer)
                              (and (eq 'radial (plist-get layer :shape))
                                   (plist-get layer :radius)))
                            (plist-get style :layers))))
    (mapcar (lambda (layer)
              (append (list :envelope (smear-cursor--rest-envelope)
                            :radius (* smear-cursor-rest-size
                                       (plist-get layer :radius))
                            :alpha (* smear-cursor-rest-strength
                                      (or (plist-get layer :alpha) 1.0)))
                      layer))
            (or rings
                `((:shape radial
                   :radius ,(+ (/ (frame-char-height) 2.0) 4.0)
                   :alpha 0.6))))))

(defun smear-cursor--cursor-rest ()
  "Build the resting glow from its current options."
  (list :doc "A glow the cursor carries about with it."
        :duration smear-cursor-rest-duration
        ;; Slow: it never stops, and every frame is work the display
        ;; does for something that is barely changing.
        :fps 20
        :color (or smear-cursor-glow-color 'trail)
        :shape 'point
        :layers (smear-cursor--rest-layers)))

(smear-cursor-define-effect 'cursor-rest #'smear-cursor--cursor-rest)

;;; Lightning

(defcustom smear-cursor-lightning-lines 3.0
  "How far above the cursor a bolt starts, in text lines.

The symbol `window' reaches the top of the window instead, so the bolt
comes the whole way down however far the cursor has scrolled, and
`frame' reaches the top of the frame and crosses any windows above.
Either costs about a tenth more than a short bolt: the shape is drawn
once and stamped again for each frame, so most of the cost is the
strike rather than its size."
  :type '(choice (const :tag "from the top of the window" window)
                 (const :tag "from the top of the frame" frame)
                 number)
  :group 'smear-cursor)

(defcustom smear-cursor-lightning-width 2.5
  "How wide a bolt is at the top, in pixels.

It thins towards the tip, so this is the widest it gets."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-lightning-noise 0.28
  "How far a bolt wanders sideways, as a share of its height.

Zero draws a bar.  The wandering is widest at the top and closes to
nothing at the cursor, so the bolt lands on the character whatever
this is set to.  A bolt reaching the top of the window wanders this
much of a much greater height, so lower it there: a tenth is about
right for a bolt the height of a window."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-lightning-vary t
  "Strike a new shape every time.

Lightning that struck the same shape on every keystroke would read as
a picture of a bolt.  Turn this off to hold one shape, which
`smear-cursor-noise-seed' then chooses."
  :type 'boolean
  :group 'smear-cursor)

(defcustom smear-cursor-lightning-steps 6
  "How many lengths a bolt is drawn in.

More lengths bend more sharply.  The module holds eight layers for one
effect and the flash takes one of them, so this is held to seven."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-lightning-strength 0.9
  "Set a bolt's peak opacity from 0 to 1."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-lightning-duration 0.26
  "Set how many seconds a strike lasts."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-lightning-color "#cfe2ff"
  "Set the colour of a bolt, or use nil for the trail colour."
  :type '(choice (const :tag "the trail's own" nil) color)
  :group 'smear-cursor)

(defvar smear-cursor--strike 0
  "Counts strikes, so no two bolts are the same shape.")

(defvar smear-cursor--last-effect nil
  "The last effect played, as (NAME . EFFECT).

An effect worked out at playback time may carry state on from one play
to the next, so building one again to read a number off it would move
that state without drawing anything.")

(defvar smear-cursor--effect-window nil
  "The window an effect is being built for, while it is being built.

An effect worked out at playback time may need to measure the window
it is about to be drawn in.  The preview panel plays in a window of
its own, so the selected window is not always the right one.")

(defun smear-cursor--bolt-height ()
  "Return how tall a bolt should be, in pixels.

`smear-cursor-lightning-lines' gives a number of text lines, the
symbol `window' to reach the top of the window the strike plays in,
or `frame' to reach the top of the frame and cross any windows above."
  (if (not (memq smear-cursor-lightning-lines '(window frame)))
      (* smear-cursor-lightning-lines (frame-char-height))
    (let* ((win (or smear-cursor--effect-window (selected-window)))
           (r (and (window-live-p win) (smear-cursor--point-rect win)))
           ;; Frame pixels are the module's job, so fall back to the
           ;; window when it is not loaded rather than failing here.
           (r (if (and r (eq smear-cursor-lightning-lines 'frame)
                       (fboundp 'smear-cursor-x11--frame-xy))
                  (smear-cursor--rect-in-frame win r)
                r)))
      ;; Down to the middle of the cursor's own line, so the bolt spans
      ;; the window and lands on the character rather than above it.
      (if r
          (max (float (frame-char-height))
               (+ (aref r 1) (/ (aref r 3) 2.0)))
        (* 3.0 (frame-char-height))))))

(defun smear-cursor--bolt-quads (height width steps seed noise)
  "Return STEPS quadrilaterals making a bolt HEIGHT pixels tall.

The bolt comes down from HEIGHT above the cursor and lands on it.
WIDTH is how wide it is at the top and it thins towards the tip.
NOISE is how far it wanders sideways, as a share of HEIGHT.  SEED
chooses the shape.  Coordinates are relative to the middle of the
cursor, and the corners of each quadrilateral run clockwise from the
top left, as they do everywhere else here."
  (let ((wander (lambda (k)
                  ;; Loosest at the top and straight where it lands, so
                  ;; a bolt always arrives on the character.
                  (* noise height
                     (smear-cursor--noise seed k)
                     (- 1.0 (/ (float k) steps)))))
        (out nil))
    (dotimes (i steps)
      (let* ((ta (/ (float i) steps))
             (tb (/ (float (1+ i)) steps))
             (ya (* height (- ta 1.0)))
             (yb (* height (- tb 1.0)))
             (xa (funcall wander i))
             (xb (funcall wander (1+ i)))
             (wa (* width (- 1.0 (* 0.55 ta))))
             (wb (* width (- 1.0 (* 0.55 tb)))))
        (push (vector (- xa wa) ya (+ xa wa) ya
                      (+ xb wb) yb (- xb wb) yb)
              out)))
    (nreverse out)))

(defun smear-cursor--lightning ()
  "Build a bolt from the current options.

A new shape for each strike: lightning that struck the same way every
time would read as a picture of a bolt rather than a strike."
  (let* ((seed (+ smear-cursor-noise-seed
                  (if smear-cursor-lightning-vary
                      (cl-incf smear-cursor--strike)
                    0)))
         (height (smear-cursor--bolt-height))
         (steps (max 1 (min (1- (smear-cursor--max-layers))
                            smear-cursor-lightning-steps)))
         ;; Struck, gone, struck again, out.  One flash reads as a
         ;; flicker rather than a lamp being switched on.
         (env '((0.0 . 1.0) (0.16 . 0.2) (0.3 . 1.0) (0.55 . 0.5) (1.0 . 0.0))))
    (list :doc "A bolt down onto the character just typed."
          :duration smear-cursor-lightning-duration
          :color (or smear-cursor-lightning-color 'trail)
          :shape 'point
          :layers
          (append
           (mapcar (lambda (quad)
                     `(:shape quad :quad ,quad
                       :alpha ,smear-cursor-lightning-strength
                       :blur 2
                       :envelope ,env))
                   (smear-cursor--bolt-quads
                    height smear-cursor-lightning-width steps seed
                    smear-cursor-lightning-noise))
           ;; The flash where it lands, which is what makes it a strike
           ;; rather than a line drawn down the window.
           (list `(:shape radial
                   :alpha ,(* smear-cursor-lightning-strength 0.8)
                   :radius ,(max 6.0 (* 0.7 (frame-char-height)))
                   :envelope ,env))))))

(smear-cursor-define-effect 'lightning #'smear-cursor--lightning)

;;; Sparks

(defcustom smear-cursor-plasma-arcs 3
  "How many sparks leave the cursor at once.

Up to three are drawn bent, in two lengths each.  Ask for more and
they straighten to one length each so they fit what the module draws
in one effect, up to seven."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-plasma-reach 1.0
  "How far a spark reaches from the cursor, in text lines."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-plasma-width 1.3
  "How wide a spark is where it leaves the cursor, in pixels."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-plasma-noise 0.5
  "How far a spark bends, as a share of its reach.

The bend grows with distance, so sparks leave the cursor straight and
wander further out."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-plasma-phases 4
  "How many times the sparks jump to a new shape while they play.

One holds a single shape and only fades.  Three or four crackle."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-plasma-strength 0.9
  "Set the sparks' peak opacity from 0 to 1."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-plasma-duration 0.22
  "Set how many seconds the sparks last."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-plasma-color "#a9d6ff"
  "Set the colour of the sparks, or use nil for the trail colour."
  :type '(choice (const :tag "the trail's own" nil) color)
  :group 'smear-cursor)

(defcustom smear-cursor-plasma-vary t
  "Spark a new shape every time.

Turn this off to hold one arrangement, which `smear-cursor-noise-seed'
then chooses."
  :type 'boolean
  :group 'smear-cursor)

(defvar smear-cursor--spark 0
  "Counts sparks, so no two are the same shape.")

(defun smear-cursor--arc-kink (joint steps reach seed noise)
  "Return how far across the arc JOINT of STEPS is pushed.

Sides alternate, so the arc zigzags, and the push grows with distance
from the cursor, so it leaves straight.  REACH is the length of the
arc, SEED chooses the sizes and NOISE scales them."
  (* noise reach (/ (float joint) steps)
     (if (cl-evenp joint) 1.0 -1.0)
     (abs (smear-cursor--noise seed joint))))

(defun smear-cursor--arc-quads (angle reach width steps seed noise &optional origin)
  "Return STEPS quadrilaterals for one arc leaving the cursor at ANGLE.

REACH is how far it goes in pixels and WIDTH how wide it is at the
start, thinning towards the tip.  NOISE bends it, by more the further
out it gets, so the arc leaves straight.  SEED chooses the bend.
ORIGIN is where it starts as a cons of pixels, defaulting to the
cursor itself; flames start above it.  Coordinates are relative to the
middle of the cursor."
  (let* ((dx (cos angle)) (dy (sin angle))
         ;; Across the arc, for the bend and for the width.
         (px (- dy)) (py dx)
         (ox (if origin (car origin) 0.0))
         (oy (if origin (cdr origin) 0.0))
         (out nil))
    (dotimes (i steps)
      (let* ((ta (/ (float i) steps))
             (tb (/ (float (1+ i)) steps))
             ;; Joints fall on alternate sides.  A walk that took its
             ;; direction from the noise alone would wander to one side
             ;; and back, which draws a curve, and a tapered curve
             ;; reads as a limb rather than as a spark.
             (ja (smear-cursor--arc-kink i steps reach seed noise))
             (jb (smear-cursor--arc-kink (1+ i) steps reach seed noise))
             (xa (+ ox (* dx reach ta) (* px ja)))
             (ya (+ oy (* dy reach ta) (* py ja)))
             (xb (+ ox (* dx reach tb) (* px jb)))
             (yb (+ oy (* dy reach tb) (* py jb)))
             (wa (* width (- 1.0 (* 0.6 ta))))
             (wb (* width (- 1.0 (* 0.6 tb)))))
        (push (vector (- xa (* px wa)) (- ya (* py wa))
                      (+ xa (* px wa)) (+ ya (* py wa))
                      (+ xb (* px wb)) (+ yb (* py wb))
                      (- xb (* px wb)) (- yb (* py wb)))
              out)))
    (nreverse out)))

(defun smear-cursor--arc-budget (want)
  "Return how to draw WANT arcs as a cons of arcs and lengths each.

The module holds sixteen layers for one effect and the core takes one
of them, so three arcs of five lengths is what fits: enough lengths to
be jagged.  Asking for more arcs spends those layers on arcs instead,
and they straighten out as the count goes up."
  (let* ((room (1- (smear-cursor--max-layers)))
         (want (max 1 want))
         (steps (cond ((<= (* 5 want) room) 5)
                      ((<= (* 3 want) room) 3)
                      ((<= (* 2 want) room) 2)
                      (t 1)))
         (arcs (max 1 (min want (/ room steps)))))
    (cons arcs steps)))

(defun smear-cursor--arc-angle (index count seed phase)
  "Return the angle of arc INDEX of COUNT for SEED in PHASE.

Spread around the cursor and then pushed off the even spacing, so the
sparks do not come out as a star."
  (+ (/ (* 2 float-pi index) count)
     (* 0.8 (smear-cursor--noise (+ seed phase) (+ 20 index)))))

(defun smear-cursor--arc-layer (index steps step reach seed phases noise)
  "Return the layer drawing length STEP of arc INDEX, of STEPS in all.

The layer holds the shape that length takes in each of PHASES, so the
spark jumps to a new one as it plays.  REACH, SEED and NOISE are as in
`smear-cursor--arc-quads'."
  `(:shape quad
    :alpha ,smear-cursor-plasma-strength
    :blur 1
    :envelope ((0.0 . 1.0) (0.5 . 0.9) (1.0 . 0.0))
    :quads ,(vconcat
             (cl-loop
              for p below phases
              collect (nth step
                           (smear-cursor--arc-quads
                            (smear-cursor--arc-angle index steps seed p)
                            ;; A little longer or shorter each phase, so
                            ;; the spark reaches about rather than
                            ;; redrawing at one length.
                            (* reach (+ 0.65 (* 0.35 (abs (smear-cursor--noise
                                                           (+ seed p)
                                                           (+ 40 index))))))
                            smear-cursor-plasma-width steps
                            (+ seed (* 10 p) index) noise))))))

(defun smear-cursor--plasma ()
  "Build crackling sparks from the current options."
  (let* ((seed (+ smear-cursor-noise-seed
                  (if smear-cursor-plasma-vary (cl-incf smear-cursor--spark) 0)))
         (budget (smear-cursor--arc-budget smear-cursor-plasma-arcs))
         (arcs (car budget))
         (steps (cdr budget))
         (phases (max 1 smear-cursor-plasma-phases))
         (reach (* smear-cursor-plasma-reach (frame-char-height))))
    (list :doc "Sparks crackling out of the cursor."
          :duration smear-cursor-plasma-duration
          :color (or smear-cursor-plasma-color 'trail)
          :shape 'point
          :layers
          (append
           (cl-loop for a below arcs
                    append (cl-loop for i below steps
                                    collect (smear-cursor--arc-layer
                                             a steps i reach seed phases
                                             smear-cursor-plasma-noise)))
           (list `(:shape radial
                   :alpha ,(* smear-cursor-plasma-strength 0.7)
                   :radius ,(max 4.0 (* 0.45 (frame-char-height)))
                   :envelope ((0.0 . 1.0) (0.5 . 0.9) (1.0 . 0.0))))))))

(smear-cursor-define-effect 'plasma #'smear-cursor--plasma)

;;; Fire

;; Embers rather than tongues.  A tongue is one long tapering shape,
;; and a few of them read as limbs however they are bent.  A fire is
;; better made of many small round things going up and dying out.

(defcustom smear-cursor-fire-embers 6
  "How many embers rise from the cursor at once.

The module holds eight layers for one effect and the hot base takes
one, so seven is the most that will be drawn."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-fire-height 1.4
  "How far the embers rise above the cursor, in text lines."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-fire-spread 0.5
  "How far the embers drift sideways, in character widths."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-fire-phases 4
  "How many places an ember takes as it goes up.

More phases move it more smoothly.  Each phase is a step further up
and a little to one side."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-fire-strength 0.8
  "Set the fire\='s peak opacity from 0 to 1."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-fire-duration 0.5
  "Set how many seconds the fire lasts."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-fire-color "#c0361a"
  "Set the colour of the embers as they cool, higher up the fire."
  :type 'color
  :group 'smear-cursor)

(defcustom smear-cursor-fire-core-color "#ff8c2b"
  "Set the colour at the base of the fire, where it is hottest.

A fire is not one colour throughout, and the base is the bright part."
  :type 'color
  :group 'smear-cursor)

(defcustom smear-cursor-fire-vary t
  "Burn a new shape every time.

Turn this off to hold one arrangement, which `smear-cursor-noise-seed'
then chooses."
  :type 'boolean
  :group 'smear-cursor)

(defvar smear-cursor--flame 0
  "Counts fires, so no two burn the same way.")

(defun smear-cursor--ember-layer (index count height phases seed)
  "Return the layer for ember INDEX of COUNT in a fire HEIGHT pixels tall.

Embers further up the fire are smaller, cooler and fainter, and each
takes PHASES places on the way up, drifting sideways as it goes.  SEED
chooses the drift."
  (let* ((f (/ (float index) (max 1 count)))    ; 0 at the base, 1 at the top
         (start (* height 0.3 f))
         (peak (min 0.55 (+ 0.05 (* 0.45 f))))
         (drift (* (frame-char-width) smear-cursor-fire-spread)))
    `(:shape radial
      :anchor bottom
      :color ,(if (< f 0.45)
                  smear-cursor-fire-core-color
                smear-cursor-fire-color)
      :alpha ,(* smear-cursor-fire-strength (- 1.0 (* 0.4 f)))
      :radius ,(max 2.5 (* height 0.3 (- 1.0 (* 0.5 f))))
      ;; Later embers come up later, so the fire keeps going for its
      ;; whole life rather than arriving all at once.
      :envelope ((0.0 . 0.0) (,peak . 1.0)
                 (,(min 0.95 (+ peak 0.3)) . 0.55) (1.0 . 0.0))
      :offsets ,(vconcat
                 (cl-loop
                  for p below phases
                  collect
                  (cons (* drift (smear-cursor--noise (+ seed p) (+ 70 index)))
                        (- (+ start (* height 0.6
                                       (/ (float p) (max 1 phases)))))))))))

(defun smear-cursor--fire ()
  "Build a fire from the current options."
  (let* ((seed (+ smear-cursor-noise-seed
                  (if smear-cursor-fire-vary (cl-incf smear-cursor--flame) 0)))
         ;; The hot base takes one of the module's layers.
         (embers (max 1 (min (1- (smear-cursor--max-layers))
                             smear-cursor-fire-embers)))
         (phases (max 1 smear-cursor-fire-phases))
         (height (* smear-cursor-fire-height (frame-char-height))))
    (list :doc "A fire burning off the character just typed."
          :duration smear-cursor-fire-duration
          :color smear-cursor-fire-color
          :shape 'point
          :layers
          (append
           (cl-loop for i below embers
                    collect (smear-cursor--ember-layer i embers height phases seed))
           ;; The hot base, sitting on the character itself.
           (list `(:shape radial
                   :anchor bottom
                   :color ,smear-cursor-fire-core-color
                   :alpha ,smear-cursor-fire-strength
                   :radius ,(max 3.0 (* 0.45 (frame-char-height)))
                   :envelope ((0.0 . 0.6) (0.15 . 1.0) (0.5 . 0.7)
                              (1.0 . 0.0))))))))

(smear-cursor-define-effect 'fire #'smear-cursor--fire)

;;; Sprites

;; A sprite is a small picture drawn as text: one character a cell, a
;; space for nothing, and a palette saying what colour each character
;; is.  Several of them in a row make an animation.
;;
;; The module draws sixteen layers for a whole effect, so a figure
;; cannot be a layer a pixel.  Cells of one colour are merged into the
;; largest rectangles that will hold them, and each rectangle is a
;; layer carrying one quad for every phase of the flight.  A drawing
;; of a hundred cells comes out as ten or so layers, which is what
;; makes a figure affordable at all.

(defvar smear-cursor--sprites (make-hash-table :test 'eq)
  "Sprites by name.  See `smear-cursor-define-sprite'.")

(defun smear-cursor-define-sprite (name &rest plist)
  "Define sprite NAME from PLIST.

PLIST accepts these keys:

`:frames'   A list of frames, each a list of equal-length strings, one
            a cell row.  A space draws nothing.  Frames play in turn.
`:palette'  An alist of (CHARACTER . COLOR).  Its order is the drawing
            order, so a character later in it is drawn over one
            earlier: put outlines and faces last.
`:hold'     How many phases each frame is held for.  One is a frame
            every phase, which at the usual rate is far too fast for a
            walk; four is a step a sixth of a second.

Draw the sprite facing left.  It is mirrored when it travels right, so
one drawing faces both ways.

Every frame must use only characters the palette names, and every row
of a frame must be the same length as the others."
  (let ((sprite (copy-sequence plist)))
    (smear-cursor--sprite-check sprite)
    (puthash name sprite smear-cursor--sprites)
    name))

(defun smear-cursor-sprite (name)
  "Return the sprite called NAME, or nil."
  (gethash name smear-cursor--sprites))

(defun smear-cursor--sprite-check (sprite)
  "Signal if SPRITE is not drawable.

Caught here rather than at the far end: a ragged row or a character
with no colour comes out as a figure with a piece missing, which is a
hard thing to read back to its cause."
  (let ((palette (plist-get sprite :palette))
        (frames (plist-get sprite :frames)))
    (unless frames (error "smear-cursor: sprite has no frames"))
    (unless palette (error "smear-cursor: sprite has no palette"))
    (dolist (frame frames)
      (let ((wide (length (car frame))))
        (dolist (row frame)
          (unless (= (length row) wide)
            (error "smear-cursor: sprite row %S is not %d cells" row wide))
          (dotimes (i (length row))
            (let ((ch (aref row i)))
              (unless (or (eq ch ?\s) (assq ch palette))
                (error "smear-cursor: sprite uses %c, which the palette \
does not name" ch)))))))))

(defun smear-cursor--sprite-runs (grid row ch)
  "Return every run of CH in GRID\='s ROW, as a list of (LEFT . RIGHT)."
  (let* ((line (aref grid row))
         (wide (length line))
         (out nil)
         (col 0))
    (while (< col wide)
      (if (not (eq (aref line col) ch))
          (setq col (1+ col))
        (let ((start col))
          (while (and (< col wide) (eq (aref line col) ch))
            (setq col (1+ col)))
          (push (cons start col) out))))
    (nreverse out)))

(defun smear-cursor--sprite-run-under (grid row ch span)
  "Return the run of CH in GRID\='s ROW lying under SPAN, or nil.

Under means overlapping it.  A shape carries on downwards only into
cells its own bottom row touches, or a figure would join up with
whatever happened to be below it."
  (cl-find-if (lambda (run) (and (< (car run) (cdr span))
                                 (> (cdr run) (car span))))
              (smear-cursor--sprite-runs grid row ch)))

(defun smear-cursor--sprite-runs-over (grid row ch run)
  "Return the runs of CH in GRID\='s ROW that overlap RUN."
  (seq-filter (lambda (r) (and (< (car r) (cdr run)) (> (cdr r) (car run))))
              (smear-cursor--sprite-runs grid row ch)))

(defun smear-cursor--sprite-run-below (grid whole row ch span)
  "Return the run of CH under SPAN on the row below ROW in GRID, or nil.

Nil where the drawing joins or parts there: two runs meeting one below
them, or one becoming two, as the point of Pacman\='s mouth does.  A
shape carried across a join takes a step of half the figure in a
single row, and having set its slope from that it splays across the
gap it should leave, while the rows beneath the other side of the join
go to nobody.  Facing up that is a bar sticking out to his right with
a strip missing beside it.

Counted in WHOLE, the drawing as it came, rather than in GRID, whose
claimed cells are blanked as shapes are taken: with one side of a join
already claimed, the other cannot see there was ever a join there."
  (let ((below (smear-cursor--sprite-run-under grid (1+ row) ch span)))
    (when (and below
               (= 1 (length (smear-cursor--sprite-runs-over
                             whole (1+ row) ch span)))
               (= 1 (length (smear-cursor--sprite-runs-over
                             whole row ch below))))
      below)))

(defun smear-cursor--sprite-foot (edge step want into)
  "Return where a side ends below EDGE, given its STEP and WANT below it.

WANT is where the same side sits on the row under the shape, and INTO
says which way is into the shape: 1 for a left side, -1 for a right
one.

A side that steps inward carries its step one row further whatever is
below.  Inward it can only eat into its own last row, rounding a
corner off, so it is always safe -- and it is what keeps a long edge
straight to its end.  Each side of Pacman\='s mouth runs to a point
where the cells stop advancing, and held back there it bent.

Outward it may only reach the run below, and then only when that run
is exactly one step on, so the shape below is carrying this side\='s
line.  Any further and it puts colour where the figure is not: a spike
a step wide on every turn of a curve.

With nothing below there is no join to meet, and reaching past the
last row would only cut the shape short."
  (let ((ext (+ edge step)))
    (cond ((null want) edge)
          ((> (* step into) 0) ext)
          ((= want ext) want)
          (t edge))))

(defun smear-cursor--sprite-slope (grid whole row ch span)
  "Return the rows of CH under SPAN in GRID from ROW down, and its foot.

A list of (SPANS FOOT): the run taken from each row, and where the
shape\='s bottom edge should sit, as (LEFT . RIGHT).

Rows join one shape while both edges keep stepping by the same amount
as before.  That is a straight side, which is what a quad has, and it
is what a diagonal in a drawing is made of: taken a step at a time a
round figure costs a layer a row.

The foot is the run on the row below the shape, not the last row of
the shape itself.  A side has to end exactly where the side of the
shape below it starts, or every join shows as a step -- each side of
Pacman\='s mouth is three shapes on one straight line.  Where there is
no row below, the shape\='s own last row is its foot, so a side that
nothing carries on does not run past the figure."
  (let* ((h (length grid))
         (spans (list span))
         (dl nil) (dr nil)
         (next (1+ row))
         (going t))
    (while (and going (< next h))
      (let* ((last-span (car (last spans)))
             (run (smear-cursor--sprite-run-below grid whole (1- next)
                                                  ch last-span)))
        (cond
         ((null run) (setq going nil))
         ;; The first row below sets the slope; the rest keep to it.
         ((or (null dl)
              (and (= (car run) (+ (car last-span) dl))
                   (= (cdr run) (+ (cdr last-span) dr))))
          (unless dl
            (setq dl (- (car run) (car last-span))
                  dr (- (cdr run) (cdr last-span))))
          (setq spans (append spans (list run))
                next (1+ next)))
         (t (setq going nil)))))
    (let* ((last-span (car (last spans)))
           (below (and (< next h)
                       (smear-cursor--sprite-run-below grid whole (1- next)
                                                       ch last-span))))
      (list spans
            (cons (smear-cursor--sprite-foot (car last-span) (or dl 0)
                                             (and below (car below)) 1)
                  (smear-cursor--sprite-foot (cdr last-span) (or dr 0)
                                             (and below (cdr below)) -1))))))

(defun smear-cursor--sprite-rects (rows)
  "Return the shapes ROWS are drawn from, as (CHAR COL ROW W H BL BR).

ROWS is a list of equal-length strings, a space meaning nothing is
drawn.  COL ROW W H is the shape\='s top row, and BL BR is where its
bottom edge sits, so a shape is a trapezium and an upright box is the
case where the two agree.

Cells are taken left to right and top to bottom: each one not yet
claimed is widened along its row, and the shape then runs down for as
long as both of its edges keep stepping by the same amount.

Shapes rather than cells because each one costs a layer, and there are
sixteen of those for a whole effect."
  (let* ((h (length rows))
         (w (if rows (length (car rows)) 0))
         (grid (apply #'vector (mapcar #'copy-sequence rows)))
         ;; The drawing as it came, for the questions that are about
         ;; its shape rather than about what is left to claim.
         (whole (apply #'vector (mapcar #'copy-sequence rows)))
         (out nil))
    (dotimes (row h)
      (dotimes (col w)
        (let ((ch (aref (aref grid row) col)))
          (unless (eq ch ?\s)
            (let* ((run (smear-cursor--sprite-run-under
                         grid row ch (cons col (1+ col))))
                   (span (cons col (cdr run)))
                   (slope (smear-cursor--sprite-slope grid whole row ch span))
                   (spans (nth 0 slope))
                   (foot (nth 1 slope)))
              (push (list ch col row (- (cdr span) col) (length spans)
                          (car foot) (cdr foot))
                    out)
              ;; Taken, so the cells below and to the right start from
              ;; what is left rather than covering this again.
              (cl-loop for one in spans
                       for j from 0
                       do (cl-loop for i from (car one) below (cdr one)
                                   do (aset (aref grid (+ row j)) i ?\s))))))))
    (nreverse out)))

(defun smear-cursor--sprite-plan (frames)
  "Return how many layers each character of FRAMES needs.

An alist of (CHAR . COUNT), COUNT being the most rectangles that
character takes in any one frame.  A layer holds its colour for the
whole flight, so the allotment is made once from the busiest frame and
every frame afterwards draws into it."
  (let ((plan nil))
    (dolist (frame frames)
      (let ((seen nil))
        (dolist (r (smear-cursor--sprite-rects frame))
          (setf (alist-get (car r) seen 0) (1+ (alist-get (car r) seen 0))))
        (dolist (pair seen)
          (setf (alist-get (car pair) plan 0)
                (max (alist-get (car pair) plan 0) (cdr pair))))))
    (nreverse plan)))

(defun smear-cursor--sprite-plan-for (sprite)
  "Return SPRITE's plan in its palette's order.

The order is the drawing order, so it is also the order detail is
given up in when there is not room for all of it."
  (let ((plan (smear-cursor--sprite-plan
               (apply #'append (mapcar #'cdr (smear-cursor--sprite-sets sprite))))))
    (cl-loop for (ch . _) in (plist-get sprite :palette)
             for n = (alist-get ch plan 0)
             when (> n 0) collect (cons ch n))))

(defun smear-cursor--sprite-fit (plan budget)
  "Return PLAN reduced to BUDGET layers.

Detail goes before shape.  A character's later rectangles are the
smaller pieces of it -- the peak of a cap, the second boot -- so those
are given up first, from whichever character has the most.  Only when
every character is down to one block are whole characters dropped, and
then from the end of the palette, where what is drawn last and on top
is the fine detail."
  (let ((out (copy-alist plan)))
    (while (and (> (cl-loop for (_ . n) in out sum n) budget)
                (cl-loop for (_ . n) in out thereis (> n 1)))
      (let ((worst (car (cl-sort (copy-sequence out) #'> :key #'cdr))))
        (setf (alist-get (car worst) out) (1- (cdr worst)))))
    (while (and out (> (cl-loop for (_ . n) in out sum n) budget))
      (setq out (butlast out)))
    out))

(defun smear-cursor--sprite-size (sprite)
  "Return SPRITE\\='s size in cells, as (COLS . ROWS)."
  (let ((frame (car (plist-get sprite :frames))))
    (cons (length (car frame)) (length frame))))

(defun smear-cursor--sprite-pixel (sprite lines lh)
  "Return the size in pixels of one cell of SPRITE drawn LINES tall.

LH is the line height.  A whole number of pixels, because a cell
landing on a fraction is resolved differently at each of its edges,
which rounds the corners off a drawing whose whole point is that it
has corners.  Never below one, which is where a figure of many cells
drawn small ends up: Pacman is thirty-two cells and about that many
pixels."
  (max 1 (round (/ (* lines lh) (cdr (smear-cursor--sprite-size sprite))))))

(defconst smear-cursor--sprite-bleed 0.5
  "How far each block of a sprite is grown, in pixels.

Every quad is drawn with a soft half-pixel edge, so two that meet
exactly each cover their shared line about halfway.  One composited
over the other comes to about three quarters rather than one, and the
join shows as a darker line across the figure: a drawing of stacked
bands came out stripey.  Grown by half a pixel they overlap by one,
and the seam is covered twice in the same colour, which is that
colour.")

(defun smear-cursor--sprite-quad (rect base px cols rows flip)
  "Return the quad RECT covers with the sprite standing at BASE.

PX is a cell\\='s size in pixels and COLS by ROWS the sprite\\='s size in
cells.  Non-nil FLIP mirrors it, which is how one drawing faces both
ways.

BASE is where the sprite\\='s feet go, not its middle: it walks along the
text, so the line is the floor.  It is rounded to whole pixels first: a
cell landing on a fraction is resolved differently at each of its
edges, which rounds the corners off a drawing whose whole point is
that it has corners."
  (let* ((col (nth 1 rect)) (row (nth 2 rect))
         (w (nth 3 rect)) (h (nth 4 rect))
         (dl (or (nth 5 rect) col)) (dr (or (nth 6 rect) (+ col w)))
         (bleed smear-cursor--sprite-bleed)
         ;; Top edge from this shape's first row, bottom edge from the
         ;; row below it -- which is where the next shape starts, so
         ;; the two meet exactly.  Ending at this shape's own last row
         ;; leaves a jump at every join; carrying the slope on a step
         ;; past it assumes the shape below leans the same way, which
         ;; on a curve it does not, and the figure grows spikes.
         (lt col)
         (lb dl)
         (rt (+ col w))
         (rb dr)
         ;; Mirrored, the left edge is the right one measured back
         ;; from the far side.
         (l0 (if flip (- cols rt) lt))
         (l1 (if flip (- cols rb) lb))
         (r0 (if flip (- cols lt) rt))
         (r1 (if flip (- cols lb) rb))
         (bx (fround (car base)))
         (by (fround (cdr base)))
         (x (lambda (c) (+ bx (* px (- c (/ cols 2.0))))))
         (y0 (+ by (* px (- row rows))))
         (y1 (+ by (* px (- (+ row h) rows))))
         (ytop (- y0 bleed))
         (ybot (+ y1 bleed))
         ;; Grown along its own slope, not straight up and down.  A
         ;; sloped side stretched vertically ends off the line it
         ;; belongs to, and two shapes stacked on one straight edge --
         ;; which is what each side of Pacman's mouth is made of --
         ;; then meet with a notch at every join.
         (lean (lambda (top bottom)
                 (let ((m (/ (- bottom top) (max 1.0 (- y1 y0)))))
                   (cons (- top (* m bleed)) (+ bottom (* m bleed)))))))
    (let ((left (funcall lean (funcall x l0) (funcall x l1)))
          (right (funcall lean (funcall x r0) (funcall x r1))))
      (vector (- (car left) bleed) ytop
              (+ (car right) bleed) ytop
              (+ (cdr right) bleed) ybot
              (- (cdr left) bleed) ybot))))

(defun smear-cursor--sprite-slot (rects ch slot)
  "Return the SLOT-th rectangle of CH in RECTS, or the last of them.

A frame with fewer rectangles than the busiest one has slots with
nothing to draw.  They repeat one it does have instead of being left
empty: a quad of no area still covers its own pixel faintly, while the
same rectangle drawn twice in one colour shows nothing at all."
  (let ((mine (cl-remove-if-not (lambda (r) (eq (car r) ch)) rects)))
    (when mine
      (nth (min slot (1- (length mine))) mine))))

(defun smear-cursor--sprite-heading (walk index)
  "Return which way WALK is going at INDEX.

One of `left\=', `right\=', `up\=' or `down\=', whichever it last actually
moved in, so a figure that has stopped keeps facing the way it was
going rather than snapping back."
  (let ((heading 'left))
    (cl-loop
     for i from 1 to index
     do (let ((dx (- (car (nth i walk)) (car (nth (1- i) walk))))
              (dy (- (cdr (nth i walk)) (cdr (nth (1- i) walk)))))
          (cond ((and (> (abs dy) (abs dx)) (> (abs dy) 0.01))
                 (setq heading (if (> dy 0) 'down 'up)))
                ((> (abs dx) 0.01)
                 (setq heading (if (> dx 0) 'right 'left))))))
    heading))

(defun smear-cursor--sprite-flip-p (walk index)
  "Return non-nil when WALK is heading right at INDEX.

Sprites are drawn facing left, so this is when to mirror one."
  (eq 'right (smear-cursor--sprite-heading walk index)))

(defun smear-cursor--sprite-sets (sprite)
  "Return SPRITE\='s frames by heading, as an alist of (HEADING . FRAMES).

`side\=' is what it is drawn as, facing left and mirrored when it
travels right.  `up\=' and `down\=' are optional: worth a set of frames
for Pacman, whose mouth has to point where he is going, and worth
nothing for a man with a mop."
  (let ((out (list (cons 'side (plist-get sprite :frames)))))
    (dolist (way '((up . :frames-up) (down . :frames-down)))
      (when (plist-get sprite (cdr way))
        (push (cons (car way) (plist-get sprite (cdr way))) out)))
    (nreverse out)))

(defun smear-cursor--sprite-layer (sprite ch color slot walk px lh sets)
  "Return the layer drawing SLOT of CH in COLOR for SPRITE along WALK.

PX is a cell\='s size in pixels, LH the line height, and SETS the
shapes of each frame of each heading, worked out once for all the
layers."
  (let* ((size (smear-cursor--sprite-size sprite))
         (hold (max 1 (or (plist-get sprite :hold) 1))))
    `(:shape quad
      :part sprite
      :color ,color
      :alpha 1.0
      ;; Full throughout, rather than fading in and out.  The blocks
      ;; are grown so their seams are covered twice in the same colour
      ;; -- `smear-cursor--sprite-bleed' -- and at anything short of
      ;; full opacity twice is not that colour: every overlap comes out
      ;; denser and the figure is striped.  Fading also made him blink
      ;; between one play and the next, which are back to back.
      :envelope ((0.0 . 1.0) (1.0 . 1.0))
      :quads
      ,(vconcat
        (cl-loop
         for place in walk
         for i from 0
         collect
         (let* ((heading (smear-cursor--sprite-heading walk i))
                ;; Sideways, and anything the sprite has no frames
                ;; for, is drawn from the side ones.
                (way (if (assq heading sets) heading 'side))
                (rects (alist-get way sets))
                (frame (nth (mod (/ i hold) (length rects)) rects))
                (rect (smear-cursor--sprite-slot frame ch slot))
                ;; Its feet are on the line it walks, and the walk
                ;; gives the middle of the cell.
                (base (cons (car place) (+ (cdr place) (/ lh 2.0)))))
           (if rect
               (smear-cursor--sprite-quad
                rect base px (car size) (cdr size)
                (and (eq way 'side) (eq heading 'right)))
             ;; Nothing of this colour in this frame at all.
             (vector (car base) (cdr base) (car base) (cdr base)
                     (car base) (cdr base) (car base) (cdr base)))))))))

(defun smear-cursor--sprite-layers (sprite walk px lh &optional budget)
  "Return the layers drawing SPRITE along WALK.

PX is a cell's size in pixels and LH the line height.  One layer a
rectangle, in the palette's order, so a character later in the palette
is drawn over one earlier.

BUDGET caps how many layers are used, giving up detail to fit.  Built
over it instead, the figure would be cut down by the trim, which keeps
the layers built last and so takes away whatever is drawn underneath:
overalls before a moustache."
  (let* ((palette (plist-get sprite :palette))
         (plan (smear-cursor--sprite-plan-for sprite))
         (sets (mapcar (lambda (set)
                         (cons (car set)
                               (mapcar #'smear-cursor--sprite-rects (cdr set))))
                       (smear-cursor--sprite-sets sprite))))
    (when budget (setq plan (smear-cursor--sprite-fit plan budget)))
    (cl-loop
     for (ch . color) in palette
     append (cl-loop for slot below (alist-get ch plan 0)
                     collect (smear-cursor--sprite-layer
                              sprite ch color slot walk px lh sets)))))

(defun smear-cursor--sprite-layer-count (sprite)
  "Return how many layers SPRITE takes.

Across every heading it is drawn from, which is what it is allotted:
counting only the side-on frames said Pacman cost twelve when he cost
nineteen, because a mouth opening upwards breaks into more shapes than
one opening along the row."
  (cl-loop for (_ . n) in (smear-cursor--sprite-plan-for sprite) sum n))

;;; Pacman

;; He eats nothing.  The overlay draws over the window, so a quad in
;; the buffer's background colour hides the text under it and takes it
;; back when the effect ends.  The buffer is never touched.

(defcustom smear-cursor-pacman-duration 3.0
  "Set how many seconds Pacman takes to cross the text."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-pacman-eat-text t
  "Hide the text Pacman passes over, as though he had eaten it.

The text is covered rather than changed, and stays covered until the
cursor is touched: what he has eaten is eaten for as long as he is on
screen.  Turn it off to leave it alone and let him eat only the
pellets, as he does in the arcade, where he eats the dots and not the
maze."
  :type 'boolean
  :group 'smear-cursor)

(defcustom smear-cursor-pacman-rows 6
  "How many rows above and below the cursor Pacman may roam.

He turns back at the edge of the window whatever this says.

Rows are the cheaper of the two directions to give him: he goes back
over a row on the way past, so its stretches join, where reaching
further along a line makes separate ones.  Six costs about three
stretches a minute against `smear-cursor-roam-columns' at forty; the
whole window, twenty-four of them, costs ten."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-pacman-pellets 0
  "How many pellets are laid out ahead of him.

None by default: they are small, they stay put, and they end up being
the only thing anyone sees.  Held to what is left of the module\='s
layers once he and his ghosts have theirs."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-pacman-color "#ffd23f"
  "Set Pacman\='s colour."
  :type 'color
  :group 'smear-cursor)

(defcustom smear-cursor-pacman-power 4
  "Every so many plays, a power pellet turns the chase blue.

Nought for never.  While the chase is blue he eats the ghosts he runs
into, which is often: they follow the route he has just walked and he
doubles back along it.  The arcade\='s other rule, that a ghost he
touches is the end of him, would be the end of most plays."
  :type 'integer
  :group 'smear-cursor)

(defconst smear-cursor--pacman-blue "#3f6fff"
  "What the ghosts turn while he has the run of them.")

(defconst smear-cursor--pacman-clash-color "#ffffff"
  "What a ghost bouncing off him flashes.")

(defcustom smear-cursor-pacman-berserk '("vim")
  "Words that set Pacman off when he crosses one.

He knows where he is in pixels, and a pixel in a window is a buffer
position away from being a word, so he reads what he is about to eat.
One of these under his route and he goes for it: red, and taking
bigger bites for the rest of that play.  Nil for nothing to look for.

Matched whole and without regard to case, against the word under a
handful of places along the route rather than every one of them: this
runs while the effect is built, and a display query a frame is a
different proposition from eight of them a play."
  :type '(repeat string)
  :group 'smear-cursor)

(defconst smear-cursor--pacman-berserk-color "#ff2d2d"
  "What he turns when he finds a word he cannot let pass.")

(defconst smear-cursor--pacman-berserk-bite 1.6
  "How much wider he bites when he is set off, as a share of his reach.")

(defconst smear-cursor--pacman-berserk-notice 0.3
  "How far into a play he leaves off wandering and goes for the word.

A share of the play.  Straight away and the wander never reads as one;
late and there is no play left to go for anything in.")

(defconst smear-cursor--pacman-berserk-dash 2.0
  "How much faster than his wander he runs at a word, as a multiple.

A pace rather than a share of the play: given a share, a word close by
had him crossing the gap slowly and a word far off had him streaking,
which is the wrong way round for something he has just gone for.  What
is left of the play once he arrives he spends on the word.")

(defun smear-cursor--pacman-pace (walk)
  "Return the mean distance between one place of WALK and the next."
  (let ((total 0.0) (n 0))
    (cl-loop for (a b) on walk while b do
             (setq total (+ total (abs (- (car b) (car a)))
                            (abs (- (cdr b) (cdr a))))
                   n (1+ n)))
    (if (> n 0) (/ total n) 1.0)))

(defconst smear-cursor--pacman-mark-color "#ffe066"
  "What the word he has gone for is marked in.")

(defun smear-cursor--pacman-mark (walk at target wide lh)
  "Return the layer marking the word he has gone for.

WALK is the route, AT where he notices, TARGET where the word starts,
WIDE how far it runs and LH the line height.  On from the moment he
notices and off once he has crossed it: a mark that outlasts the
eating is a highlight, and this is a word being singled out."
  (let* ((n (max 1 (1- (length walk))))
         (on (/ (float at) n))
         (off (min 1.0 (+ on 0.5)))
         (top (- (cdr target) (/ lh 2.0)))
         (bottom (+ top lh)))
    `(:shape quad
      :part word
      :color ,smear-cursor--pacman-mark-color
      :alpha 0.4
      :quads ,(vector (vector (car target) top (+ (car target) wide) top
                              (+ (car target) wide) bottom (car target) bottom))
      :envelope ((0.0 . 0.0) (,(max 0.0 (- on 0.02)) . 0.0) (,on . 1.0)
                 (,(max 0.0 (- off 0.05)) . 1.0) (,off . 0.0) (1.0 . 0.0)))))

(defconst smear-cursor--pacman-stare-color "#1a0000"
  "What the eyes he turns on the word are drawn in.")

(defun smear-cursor--pacman-stare (walk at target r)
  "Return the eyes he opens on the word he has gone for.

WALK is the route, AT the place he leaves off wandering at, TARGET
where the word is and R his radius.  Two layers, shut until he lands
and open after: he is drawn as a mouth in profile, and another set of
frames for facing out costs nineteen layers where a pair of eyes costs
two and says the same thing."
  (let* ((n (max 1 (1- (length walk))))
         (landed (min 0.99 (/ (float (+ at (round (* 0.3 (- (length walk) at)))))
                              n))))
    (mapcar (lambda (side)
              `(:shape radial
                :part pacman-eye
                :color ,smear-cursor--pacman-stare-color
                :alpha 1.0
                :radius ,(* r 0.22)
                :stops ((0.0 . 1.0) (0.7 . 1.0) (1.0 . 0.0))
                :offset ,(cons (+ (car target) (* side r 0.42)) (cdr target))
                :envelope ((0.0 . 0.0) (,landed . 0.0)
                           (,(min 1.0 (+ landed 0.04)) . 1.0) (1.0 . 1.0))))
            '(-1 1))))

(defconst smear-cursor--pacman-jumps 3
  "How many times he jumps on the word once he has eaten it.")

(defconst smear-cursor--pacman-jump-high 0.7
  "How high he jumps, as a share of a line.")

(defun smear-cursor--pacman-run-in (from target steps)
  "Return STEPS places running from FROM to TARGET.

A count rather than a pace: what is left of the play is what he has to
get there in, so how fast he goes is whatever the distance and the
count between them ask for.  Given a pace instead, the run was cut to
the places left and he stopped partway, a few characters short of the
word."
  (cl-loop for i from 1 to steps
           collect (let ((f (/ (float i) steps)))
                     (cons (+ (car from) (* f (- (car target) (car from))))
                           (+ (cdr from) (* f (- (cdr target) (cdr from))))))))

(defun smear-cursor--pacman-hop (place lh n)
  "Return N places bobbing over PLACE, LH being the line height.

As many jumps as there is room for, three places to a jump at least so
that he comes down between them: read off a shorter arc he never
touched the line and the jumping was a hover."
  (let* ((jumps (max 1 (min smear-cursor--pacman-jumps (floor (/ n 3.0)))))
         (each (max 3.0 (/ (float n) jumps)))
         (high (* lh smear-cursor--pacman-jump-high)))
    (cl-loop for i below n
             collect (cons (car place)
                           (- (cdr place)
                              (* high (abs (sin (* float-pi (/ i each))))))))))

(defun smear-cursor--pacman-charge (walk at target wide &optional lh)
  "Return WALK with everything from AT replaced by going for the word.

TARGET is where the word starts, WIDE how far it runs and LH the line
height, for the height of a jump.  He runs at it faster than he
wanders, crosses the whole of it, and jumps over the place it was in
until the play runs out.

Crosses it, because landing on the first letter and staying there ate
the two characters his mouth is wide: eating a word means going over
all of it."
  (let* ((from (nth at walk))
         (left (max 3 (- (length walk) at)))
         (lh (or lh 18.0))
         (pace (smear-cursor--pacman-pace walk))
         ;; The play is divided between getting there, going over it
         ;; and jumping on it, so that all three happen however far off
         ;; the word is.  Crossing keeps to his eating pace; the run
         ;; takes whatever speed is left over, which is the point.
         (over-n (max 1 (min (round (/ left 3.0))
                             (ceiling (/ wide (max 0.1 pace))))))
         (hops-n (max 2 (round (/ left 4.0))))
         (run-n (max 1 (- left over-n hops-n)))
         (run (smear-cursor--pacman-run-in from target run-n))
         (over (smear-cursor--pacman-run-in
                target (cons (+ (car target) wide) (cdr target)) over-n)))
    (append (seq-take walk at) run over
            (smear-cursor--pacman-hop
             (cons (+ (car target) (/ wide 2.0)) (cdr target)) lh
             (max 0 (- (- (length walk) at) run-n over-n))))))

(defcustom smear-cursor-pacman-immune nil
  "Whether a ghost that catches Pacman up is shrugged off.

Nil is the arcade\='s rule: he goes out where they touched and the chase
carries on without him until the next play sets him off again.  Non-nil
leaves him to walk through them, which is what he did before there was
a rule, and which suits a route where the ghosts follow his own
footsteps and meeting one is the queue behind him catching up rather
than a chase he is losing.

Either way the meeting flashes, and either way a play he has a power
pellet in ends with him eating them instead."
  :type 'boolean
  :group 'smear-cursor)

(defconst smear-cursor--pacman-touching 1.8
  "How near a ghost has to be to have met him, in his radii.

The two radii added: his own, and the eight tenths of it a ghost is
drawn at.  Measured at his radius alone the pair had to be all but
concentric before it counted, and a meeting came up once in a dozen
plays.")

(defconst smear-cursor--pacman-most-ghosts 3
  "The most ghosts that chase Pacman.

Three of the arcade\='s four.  Each costs four of the module\='s layers,
and what he is eating is served out of those first: asked for the
whole set, the fourth had nothing left to be drawn with and the menu
offered a number that never arrived.")

(defcustom smear-cursor-pacman-ghosts 0
  "How many ghosts chase Pacman, up to `smear-cursor--pacman-most-ghosts\='.

None by default, which leaves him the text to himself.  Each takes
four of the module\='s layers -- a dome, a body and two eyes -- and what
he is eating is served out of those layers first, so the last ghost
asked for is the first to go without.  What there is no room for is
not drawn."
  :type 'integer
  :group 'smear-cursor)

(defun smear-cursor--pacman-ghosts ()
  "Return how many ghosts to chase him with, inside the cap."
  (max 0 (min smear-cursor-pacman-ghosts smear-cursor--pacman-most-ghosts)))

(defcustom smear-cursor-pacman-ghost-colors
  '("#ff4d4d" "#ffb8de" "#5bd8ff" "#ffb852")
  "Colours for the ghosts, used in turn."
  :type '(repeat color)
  :group 'smear-cursor)

(defcustom smear-cursor-pacman-pellet-color "#ffe9a8"
  "Set the colour of the pellets he eats."
  :type 'color
  :group 'smear-cursor)

(defvar smear-cursor--pacman-run 0
  "Counts runs, so he takes a new route each time.")

(defvar smear-cursor--roam-from '(0.0 . 0.0)
  "Where the next roaming play picks up, relative to the cursor.

A play is a fixed length of animation, and roaming is longer than any
one of them: each play starts where the last ended so that they read
as one wander rather than the same three seconds over and over.")

(defvar smear-cursor--pacman-layers nil
  "The layers of the play being built.

Built before the tail is moved on, since the ghosts and the eaten
bands are both worked out from where the last play left off.")

(defvar smear-cursor--roam-tail nil
  "Where he was at the end of the last play.

Carried over so the hole behind him is not filled in at the handover
from one play to the next.")

(defcustom smear-cursor-roam-columns 40
  "How far along a line a roamer strays from the cursor, in characters.

The other side of the per-effect row settings.  Unbounded, a roamer
walks off across the frame: on every row it eats a short piece, leaves
and comes back somewhere else, so what it has taken is many short
stretches rather than a few long ones -- and there are only so many
layers to hold them, so the oldest is handed back within seconds.
Held near the cursor it goes back over its own ground and the
stretches join.

Forty either side is as wide as it goes without that starting again:
over three minutes it takes ten stretches and gives none of them up.
Fifty-six loses two in the same time, and eighty loses one every few
seconds."
  :type 'integer
  :group 'smear-cursor)

(defun smear-cursor--roam-route (rows phases seed)
  "Return the places a roamer takes for one play.

ROWS is how many lines above and below the cursor it may visit,
PHASES how many places it takes, and SEED which way it wanders.

The route picks up where the last play left off rather than at the
cursor, so a spell of them reads as one wander instead of the same few
seconds over and over."
  (let* ((win (or smear-cursor--effect-window (selected-window)))
         (lh (float (frame-char-height)))
         (cw (float (frame-char-width)))
         (rect (and (window-live-p win)
                    (ignore-errors (smear-cursor--point-rect win))))
         (x (if rect (aref rect 0) 0.0))
         (y (if rect (aref rect 1) 0.0))
         (width (if (window-live-p win)
                    (float (window-body-width win t)) 400.0))
         (height (if (window-live-p win)
                     (float (window-body-height win t)) 300.0))
         (from smear-cursor--roam-from)
         ;; How far from the cursor's own row it has already strayed.
         ;; ROWS is the room either side of the cursor, not the room
         ;; for one play: measured per play the roamer drifts, a row
         ;; or two at a time, and after half a minute it is a dozen
         ;; rows out and pinned against the side of the frame.
         ;;
         ;; It is also what keeps the eating.  A row visited once and
         ;; left is a stretch of its own, and wandering across more
         ;; rows than there are layers churns through them, handing
         ;; text back a few seconds after taking it.
         (strayed (round (/ (cdr from) lh)))
         (along (/ (car from) cw))
         (span (* cw smear-cursor-roam-columns)))
    (smear-cursor--roam-on
     (smear-cursor--roam-walk
      cw lh
      (max 0.0 (min (* cw (+ smear-cursor-roam-columns along))
                    (+ x (car from))))
      (max 0.0 (min (- span (car from)) (- width x (car from))))
      (max 0 (min (+ rows strayed) (floor (/ (+ y (cdr from)) lh))))
      (max 0 (min (- rows strayed) (floor (/ (- height y (cdr from) lh) lh))))
      phases seed)
     from)))

(defun smear-cursor--roam-phases (duration fps)
  "Return how many places a roamer takes over DURATION at FPS."
  (max 2 (round (* duration fps))))

(defvar smear-cursor--eaten-spans nil
  "What has been taken, as a list of (ROW LEFT . RIGHT).

One entry a stretch rather than a row.  A roamer that leaves a row and
comes back to it further along has taken two stretches of it, and the
text between them is still there: kept as one stretch from the
leftmost thing taken to the rightmost, coming back to a row swallowed
everything he had walked past on the way.

Stretches of a row are joined when they meet.  It grows while a roamer
works and is forgotten when the spell ends, which is what makes what
has gone stay gone until the cursor is touched.  See
`smear-cursor--roam-reset\='.")

(defun smear-cursor--roam-advance (walk keep)
  "Record that WALK has been taken, keeping its last KEEP places.

The next play starts where this one ended, and the places kept are
what stops the gap behind a roamer being filled in at the handover
from one play to the next."
  (setq smear-cursor--roam-from (car (last walk))
        smear-cursor--roam-tail (last walk (max 1 keep))))

(defvar smear-cursor--pacman-berserk-left 0
  "Plays still to go before a word can set him off again.")

(defun smear-cursor--roam-reset ()
  "Forget where a roaming effect had got to, and what it had eaten.

A spell of them is over, so the word he last went for is fair game
again when the next one starts."
  (setq smear-cursor--roam-from '(0.0 . 0.0)
        smear-cursor--roam-tail nil
        smear-cursor--pacman-berserk-left 0
        smear-cursor--eaten-spans nil))

(defconst smear-cursor--pacman-fps 24
  "Frames a second for Pacman.

He takes a place for every frame, so this is also how many places he
takes on the way across.")

(defun smear-cursor--background-color ()
  "Return the buffer\='s background colour as a string.

What the eaten text is covered with, so it has to be the colour the
text sits on rather than a guess at it."
  (let ((bg (face-attribute 'default :background nil t)))
    (if (stringp bg) bg (or (frame-parameter nil 'background-color) "black"))))

(defun smear-cursor--foreground-color ()
  "Return the buffer's foreground colour as a string.

What a character being carried off is drawn in, so it has to be the
colour the text is rather than a guess at it."
  (let ((fg (face-attribute 'default :foreground nil t)))
    (if (stringp fg) fg (or (frame-parameter nil 'foreground-color) "white"))))

(defun smear-cursor--roam-step (want leftward room-left room-right)
  "Return how far along a row to step, WANT pixels where there is room.

LEFTWARD is the way this leg would rather go.  It turns when that side
is used up, and where neither side has WANT it takes whatever the
roomier side has left.

Which way to go is the seed\='s to decide rather than the room\='s: a
walk that goes right whenever it can is not a wander but a march, and
it ends against the far edge of its range within a few plays."
  (cond ((and leftward (>= room-left want)) (- want))
        ((and (not leftward) (>= room-right want)) want)
        ((>= room-right want) want)
        ((>= room-left want) (- want))
        ((> room-right room-left) room-right)
        (t (- room-left))))

(defun smear-cursor--roam-legs (cw lh left right up down seed)
  "Return the legs of a roaming walk as (DX . DY) steps in pixels.

Each leg runs along a row or between rows, turning at the end of it,
so he covers text the way the text is laid out.  CW and LH are the
character size, LEFT, RIGHT, UP and DOWN the room around the cursor
in pixels and rows, and SEED chooses the route."
  (let ((x 0.0) (y 0.0) (out nil) (horizontal t))
    (dotimes (i 6)
      (if horizontal
          ;; Along a row, three to seven characters, towards whichever
          ;; side has the room for it.
          (let* ((n (+ 3 (mod (abs (round (* 100 (smear-cursor--noise seed i)))) 5)))
                 (want (* n cw))
                 (dx (smear-cursor--roam-step
                      want (< (smear-cursor--noise seed (+ 10 i)) 0)
                      (+ left x) (- right x))))
            (setq x (+ x dx))
            (push (cons dx 0.0) out))
        ;; Between rows, one row at a time, whichever way there is room.
        (let* ((rows (if (> (smear-cursor--noise seed (+ 20 i)) 0) -1 1))
               (dy (* rows lh))
               (limit (if (< rows 0) (* up lh) (* down lh))))
          (when (> (abs (+ y dy)) limit) (setq dy (- dy)))
          (if (> (abs (+ y dy)) limit)
              (push (cons 0.0 0.0) out)
            (setq y (+ y dy))
            (push (cons 0.0 dy) out))))
      (setq horizontal (not horizontal)))
    (nreverse out)))

(defun smear-cursor--roam-walk (cw lh left right up down phases seed)
  "Return PHASES places along a roaming walk, as (X . Y) from the cursor.

Whoever is roaming: Pacman eats along it and the janitor sweeps it.

CW and LH are the character size, LEFT, RIGHT, UP and DOWN the room
around the cursor, and SEED chooses the route.  The places are spread
over the legs by length, so he moves at one speed throughout."
  (let* ((legs (seq-filter (lambda (leg) (or (/= 0.0 (car leg))
                                             (/= 0.0 (cdr leg))))
                           (smear-cursor--roam-legs cw lh left right
                                                      up down seed)))
         (lengths (mapcar (lambda (leg) (+ (abs (car leg)) (abs (cdr leg)))) legs))
         (total (apply #'+ (or lengths '(1.0))))
         (out nil) (x 0.0) (y 0.0) (done 0.0) (rest legs) (lens lengths))
    (dotimes (p phases)
      (let ((want (* total (/ (float p) (max 1 (1- phases))))))
        ;; Walk the legs until the distance asked for falls inside one.
        (while (and rest (> want (+ done (car lens))))
          (setq x (+ x (car (car rest)))
                y (+ y (cdr (car rest)))
                done (+ done (car lens))
                rest (cdr rest)
                lens (cdr lens)))
        (if (null rest)
            (push (cons x y) out)
          (let ((f (/ (- want done) (max 1e-6 (car lens)))))
            (push (cons (+ x (* f (car (car rest))))
                        (+ y (* f (cdr (car rest)))))
                  out)))))
    (nreverse out)))

(defun smear-cursor--roam-on (walk from)
  "Return WALK moved to start at FROM."
  (mapcar (lambda (p) (cons (+ (car p) (car from)) (+ (cdr p) (cdr from))))
          walk))

(defun smear-cursor--eaten-reach (place half)
  "Return the stretch PLACE empties, as (LEFT . RIGHT).

HALF is how far either side of PLACE the text goes.  It is whatever
does the taking rather than how big the taker is: Pacman\='s mouth, the
head of the janitor\='s mop, the foot of the saucer\='s beam.  Pacman\='s is
short of his middle, so a character goes as he closes on it rather
than once he is past."
  (cons (- (car place) half) (+ (car place) half)))

(defun smear-cursor--eaten-stride (walk)
  "Return the furthest one place of WALK is from the one before it.

Two stretches are one when the gap between them is no wider than this.
A roamer takes the text a place at a time and the places are however
far apart the route made them, so judged on the reach alone a reach
shorter than the stride would start a stretch at every place and spend
every layer in a moment."
  (let ((most 0.0) (prev nil))
    (dolist (place walk)
      (when prev (setq most (max most (abs (- (car place) (car prev))))))
      (setq prev place))
    most))

(defun smear-cursor--eaten-touch-p (span reach slack)
  "Return non-nil when REACH comes within SLACK of SPAN.

Near enough is one stretch: the text between two places of a route is
text the roamer went over."
  (and (<= (- (cadr span) slack) (cdr reach))
       (>= (+ (cddr span) slack) (car reach))))

(defun smear-cursor--eaten-merge (spans slack)
  "Return SPANS with stretches of the same row that come within SLACK joined.

Eating the gap between two of them makes them one, and leaving them
apart would spend a layer saying twice what one layer says."
  (let ((out nil))
    (dolist (span spans)
      (let ((hit (cl-find-if
                  (lambda (o) (and (= (car o) (car span))
                                   (smear-cursor--eaten-touch-p
                                    o (cons (cadr span) (cddr span)) slack)))
                  out)))
        (if hit
            (setcdr hit (cons (min (cadr hit) (cadr span))
                              (max (cddr hit) (cddr span))))
          (push (cons (car span) (cons (cadr span) (cddr span))) out))))
    (nreverse out)))

(defun smear-cursor--eaten-fold (spans walk lh half limit)
  "Return SPANS with WALK folded into them, LIMIT stretches at most.

LH is the line height and HALF the reach of whatever is taking it.  A
place joins the stretch it meets and starts one of its own where it
meets none, so a row crossed twice with a gap between keeps the gap.

With every layer spent, the stretch eaten from longest ago is let go
to make room.  Refusing the new one instead stopped the eating dead:
the stretches filled the layers within two plays and nothing was taken
for the rest of the spell, however far the roamer walked."
  (let ((out (mapcar (lambda (s) (cons (car s) (cons (cadr s) (cddr s))))
                     spans))
        (slack (max half (smear-cursor--eaten-stride walk))))
    (dolist (place walk)
      (let* ((row (round (/ (cdr place) lh)))
             (reach (smear-cursor--eaten-reach place half))
             (hit (cl-find-if (lambda (s)
                                (and (= (car s) row)
                                     (smear-cursor--eaten-touch-p
                                      s reach slack)))
                              out)))
        (cond
         (hit (setcdr hit (cons (min (cadr hit) (car reach))
                                (max (cddr hit) (cdr reach))))
              ;; Last in the list is the last eaten from, and so the
              ;; last of them to be given up.
              (setq out (smear-cursor--eaten-merge
                         (append (delq hit out) (list hit)) slack)))
         ((< (length out) limit)
          (setq out (append out (list (cons row reach)))))
         ;; Full.  The stretch eaten from longest ago is let go and
         ;; the text under it comes back: it is the furthest from
         ;; where he is now, so it is the least of them to lose.
         ;;
         ;; Joining two stretches of a row to make room instead costs
         ;; the gap between them, which is text he never ate -- and
         ;; the pair with least between them is the pair on the row he
         ;; has just come back to, so the gap collapsed exactly where
         ;; it was meant to be kept.  Better to forget something eaten
         ;; than to eat something that was not.
         ;; Unless there are no layers for this at all, in which case
         ;; there is nothing to give up and nothing to take.
         (out (setq out (append (cdr out) (list (cons row reach))))))))
    out))

(defun smear-cursor--eaten-remember (walk lh half limit)
  "Record that WALK has been taken, in LIMIT stretches at most.

LH is the line height and HALF the reach of whatever is taking it."
  (setq smear-cursor--eaten-spans
        (smear-cursor--eaten-fold smear-cursor--eaten-spans
                                  walk lh half limit)))

(defun smear-cursor--eaten-bg-at (win pos)
  "Return the background [R G B] behind POS in WIN.

The face\='s own background where the text carries one, and the region\='s
where POS is inside an active one.  The region is painted by redisplay
rather than carried on the text, so nothing at POS says it is there and
the face lookup alone comes back with the buffer\='s own."
  (with-current-buffer (window-buffer win)
    (or (and (region-active-p)
             (>= pos (region-beginning))
             (< pos (region-end))
             (smear-cursor--color-rgb
              (smear-cursor--face-attr 'region :background)))
        (smear-cursor--bg-at pos))))

(defun smear-cursor--eaten-color (span lh)
  "Return the colour to cover SPAN with, LH being the line height.

The background where the stretch actually lies, sampled at its middle.
Covered in the `default\=' face\='s background instead, a stretch over a
highlighted line, an active region or any face with a background of its
own reads as a block dropped on the text rather than as text gone.

One colour for the stretch, since a layer keeps its colour for the whole
flight.  A stretch that runs off the end of a highlight takes the colour
of its middle, which is the best a single layer can do."
  (let* ((win (or smear-cursor--effect-window (selected-window)))
         (rect (and (window-live-p win)
                    (ignore-errors (smear-cursor--point-rect win))))
         (pos (and rect
                   (ignore-errors
                     (posn-point
                      (posn-at-x-y
                       (max 0 (round (+ (aref rect 0)
                                        (/ (+ (cadr span) (cddr span)) 2.0))))
                       (max 0 (round (+ (aref rect 1) (* (car span) lh))))
                       win))))))
    (if pos
        (smear-cursor--eaten-bg-at win pos)
      (smear-cursor--background-color))))

(defun smear-cursor--eaten-span-band (span before walk lh half)
  "Return the quads covering SPAN as WALK fills it in.

BEFORE is what earlier plays had taken, LH the line height and HALF
the reach of whatever is taking it.  One quad a phase: the stretch
opens with whatever of BEFORE lies inside it and widens as this play
reaches further along.  A place counts towards the stretch it ends up
inside, so a row eaten in two places fills them in separately."
  (let* ((row (car span))
         (deep (/ lh 2.0))
         (y (* row lh))
         (lo nil) (hi nil)
         (inside (lambda (l r) (and (>= l (cadr span)) (<= r (cddr span)))))
         (quads nil))
    (dolist (was before)
      (when (and (= (car was) row) (funcall inside (cadr was) (cddr was)))
        (setq lo (min (or lo (cadr was)) (cadr was))
              hi (max (or hi (cddr was)) (cddr was)))))
    (dolist (place walk)
      (when (= (round (/ (cdr place) lh)) row)
        (let ((reach (smear-cursor--eaten-reach place half)))
          (when (funcall inside (car reach) (cdr reach))
            (setq lo (min (or lo (car reach)) (car reach))
                  hi (max (or hi (cdr reach)) (cdr reach))))))
      (push (if lo
                (vector lo (- y deep) hi (- y deep)
                        hi (+ y deep) lo (+ y deep))
              ;; Nothing of this stretch taken yet.
              (vector 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))
            quads))
    (vconcat (nreverse quads))))

(defun smear-cursor--eaten-bands (walk lh limit half)
  "Return the layers covering what has been taken, LIMIT stretches at most.

Pacman eats it, the Grinch bags it and the saucer beams it up; what
they have in common is a stretch of a row that is not there any more,
covered in the background colour so it reads as gone rather than as
something drawn over it.

LH is the line height and HALF the reach of whatever is taking it.
One layer a stretch, worked out from what WALK will have taken by the
end of it.  Read before `smear-cursor--eaten-remember\=' folds this
route in, so a stretch opens where the last play left it."
  (let ((before smear-cursor--eaten-spans))
    (cl-loop
     for span in (smear-cursor--eaten-fold before walk lh half limit)
     collect `(:shape quad
               :part eaten
               :color ,(smear-cursor--eaten-color span lh)
               :alpha 1.0
               ;; It does not fade out at the end.  The next play
               ;; takes over just before this one finishes, and a band
               ;; going pale at the handover is the text flickering
               ;; back for a frame.
               :envelope ((0.0 . 1.0) (1.0 . 1.0))
               :quads ,(smear-cursor--eaten-span-band
                        span before walk lh half)))))

(defun smear-cursor--pacman-facing (walk index)
  "Return the angle Pacman faces at INDEX of WALK.

The way he last moved, so he keeps facing that way while he is still."
  (let ((angle 0.0))
    (cl-loop for i from 1 to index
             do (let ((dx (- (car (nth i walk)) (car (nth (1- i) walk))))
                      (dy (- (cdr (nth i walk)) (cdr (nth (1- i) walk)))))
                  (when (or (> (abs dx) 0.01) (> (abs dy) 0.01))
                    (setq angle (atan dy dx)))))
    angle))

(defun smear-cursor--roam-trail (walk delay &optional before drift)
  "Return WALK put back by DELAY places.

BEFORE is where he was at the end of the last play, so a chase carries
on across the handover from one play to the next.  Without it the
whole chase would gather at the start of every play and set off again,
which is what a pile-up every few seconds looks like.  With nothing
behind him they do wait at the start, which is how the first play of
an idle spell begins.

DRIFT is how many further places back it has fallen by the end, which
is a follower losing ground: the arcade slows its ghosts while they
are blue, and one that cannot flee along a route it did not choose can
at least drop behind on it."
  (let* ((lead (length before))
         (n (length walk))
         (history (append before walk nil)))
    (cl-loop for i below n
             collect (nth (max 0 (- (+ lead i)
                                    (round (+ delay
                                              (* (or drift 0)
                                                 (/ (float i) (max 1 n)))))))
                          history))))

(defun smear-cursor--pacman-eye (trail r env side)
  "Return one eye of a ghost following TRAIL, SIDE being -1 left or 1 right.

R is Pacman\='s radius and ENV the envelope the ghost fades on.  The
eyes lean the way the ghost is travelling, as the arcade ones do, and
they are most of what makes a coloured blob read as a ghost."
  `(:shape radial
    :part ghost-eye
    :color "#ffffff"
    :alpha 1.0
    :radius ,(* r 0.2)
    :stops ((0.0 . 1.0) (0.85 . 1.0) (1.0 . 0.0))
    :envelope ,env
    :offsets ,(vconcat
               (cl-loop
                for i from 0
                for p in trail
                collect (let ((look (smear-cursor--pacman-facing trail i))
                              (lean (* r 0.12)))
                          (cons (+ (car p) (* side r 0.3) (* lean (cos look)))
                                (+ (cdr p) (* r -0.25) (* lean (sin look)))))))))

(defconst smear-cursor--pacman-berserk-looks 24
  "The most matches weighed up when looking for a word worth minding.

A page of code can hold a great many of one word, and each one that
gets as far as being measured costs a question to the display.")

(defun smear-cursor--pacman-near-p (here pos rows cols)
  "Return non-nil when POS is within ROWS and COLS of where he is.

HERE is the cursor.  Where he is, is HERE plus how far he has roamed
from it, because a spell of plays carries him a long way and it is he
who does the eating: measured against the cursor, a word was out of
reach exactly when he was standing next to it.

Counted in lines and columns rather than measured in pixels: measuring
is a question for the display, and this is asked of every match on the
screen where the answer is wanted for one of them.

HERE is passed in rather than read from the window, because the search
that finds POS moves point, and the point of the selected window is
the buffer\='s: read inside the loop it followed the match, and every
match came out near."
  (let ((rows-out (round (/ (cdr smear-cursor--roam-from)
                            (float (frame-char-height)))))
        (cols-out (round (/ (car smear-cursor--roam-from)
                            (float (frame-char-width))))))
    (and (<= (abs (- (line-number-at-pos pos)
                     (+ (line-number-at-pos here) rows-out)))
             rows)
         (<= (abs (- (save-excursion (goto-char pos) (current-column))
                     (+ (save-excursion (goto-char here) (current-column))
                        cols-out)))
             cols))))

(defun smear-cursor--pacman-word-place (win rect)
  "Return where a word worth minding sits in WIN, or nil.

Measured from RECT, the cursor\='s rectangle, as the places of a route
are.  Searched for in the text on screen rather than read off the
route a pixel at a time: a route covers a few dozen columns of a few
rows, a word is three characters of one of them, and samples along the
way went over the top of it every time.

The near ones are weighed up in lines and columns, which is arithmetic,
and only the one he settles on is measured in pixels, which is a
question for the display."
  (when (and smear-cursor-pacman-berserk rect (window-live-p win)
             (not (minibufferp (window-buffer win))))
    (with-current-buffer (window-buffer win)
      (save-excursion
        (let ((case-fold-search t)
              (re (concat "\\_<" (regexp-opt smear-cursor-pacman-berserk) "\\_>"))
              (to (window-end win t))
              (here (window-point win))
              (looks smear-cursor--pacman-berserk-looks)
              (rows smear-cursor-pacman-rows)
              (cols smear-cursor-roam-columns))
          (goto-char (window-start win))
          (cl-loop while (and (> looks 0) (re-search-forward re to t))
                   do (setq looks (1- looks))
                   when (smear-cursor--pacman-near-p here (match-beginning 0)
                                                     rows cols)
                   thereis (let ((at (smear-cursor--pos-rect
                                      win (match-beginning 0)))
                                 (end (smear-cursor--pos-rect
                                       win (1- (match-end 0)))))
                             (and at end
                                  (list (cons (- (aref at 0) (aref rect 0))
                                              (- (aref at 1) (aref rect 1)))
                                        (- (+ (aref end 0) (aref end 2))
                                           (aref at 0)))))))))))

(defconst smear-cursor--pacman-berserk-rest 4
  "How many plays he leaves a word alone for, having gone for it.

He covers the text he eats rather than changing it, so the word is
still there the next time he looks.  Without this he found it again
every play and stayed red, which is not going berserk but a colour
scheme.")

(defun smear-cursor--pacman-berserk-forget ()
  "Let him be set off by a word again."
  (setq smear-cursor--pacman-berserk-left 0))

(defun smear-cursor--pacman-berserk-at (win rect walk)
  "Return what WALK is left for, as a plist, or nil.

`:at\=' is the place on the route he leaves off wandering at, far enough
in that the wander reads as a wander first; `:place\=' is where the word
is and `:wide\=' how far it runs, because eating a word means crossing
it.  Nil when there is no such word within reach, and nil while he is
still over the last one."
  (if (> smear-cursor--pacman-berserk-left 0)
      (progn (setq smear-cursor--pacman-berserk-left
                   (1- smear-cursor--pacman-berserk-left))
             nil)
    (let ((found (smear-cursor--pacman-word-place win rect)))
      (when found
        (setq smear-cursor--pacman-berserk-left smear-cursor--pacman-berserk-rest)
        (list :at (min (max 1 (- (length walk) 2))
                       (round (* (length walk)
                                 smear-cursor--pacman-berserk-notice)))
              :place (nth 0 found)
              :wide (nth 1 found))))))

(defun smear-cursor--pacman-powered-p ()
  "Return non-nil when this play is one he has the run of them in."
  (and (integerp smear-cursor-pacman-power)
       (> smear-cursor-pacman-power 0)
       (zerop (mod smear-cursor--pacman-run smear-cursor-pacman-power))))

(defun smear-cursor--pacman-apart-p (him it r)
  "Return non-nil when HIM and IT are more than R apart."
  (>= (+ (expt (- (car him) (car it)) 2)
         (expt (- (cdr him) (cdr it)) 2))
      (* r r)))

(defun smear-cursor--pacman-caught (walk trail r)
  "Return the phase of WALK at which TRAIL comes within R of him, or nil.

A crossing rather than a state: they have to have been apart the phase
before.  Until there is a tail behind him the trail falls back to where
he is standing, so the two start on top of each other, and read as a
state that is a meeting on the first frame of every play -- at the
place he set off from, gone before anything has moved."
  (cl-loop for him in (cdr walk)
           for it in (cdr trail)
           for was in walk
           for was-it in trail
           for i from 1
           when (and (smear-cursor--pacman-apart-p was was-it r)
                     (not (smear-cursor--pacman-apart-p him it r)))
           return i))

(defun smear-cursor--pacman-gone-at (env walk caught)
  "Return ENV ending at the CAUGHT phase of WALK, or ENV when nil.

One frame to go out in.  A ghost that vanished between one frame and
the next would be a ghost nobody saw go."
  (if (not caught)
      env
    (let ((at (/ (float caught) (max 1 (1- (length walk))))))
      (append (seq-take-while (lambda (stop) (< (car stop) at)) env)
              `((,at . 1.0) (,(min 1.0 (+ at 0.03)) . 0.0) (1.0 . 0.0))))))

(defun smear-cursor--pacman-clash (walk at r)
  "Return the flash where a ghost met him at phase AT of WALK, or nil.

R is his radius.  He is immune: they follow the route he has just
walked, so meeting one is the queue behind him catching up rather than
a chase he is losing, and the arcade\='s answer of dying there would end
most plays.  One layer, one frame of it, at the place they touched --
enough to say something happened and cheap enough to be spared from a
budget the eating has first call on."
  (when at
    (let ((place (nth at walk))
          (when-at (/ (float at) (max 1 (1- (length walk))))))
      `(:shape radial
        :part clash
        :at ,at
        :color ,smear-cursor--pacman-clash-color
        :alpha 1.0
        :radius ,(* r 2.2)
        ;; Solid most of the way out, like the dome of a ghost.  Dropped
        ;; to a third by two thirds of the radius, as this did, the only
        ;; part of it with any weight is a dot ten pixels across and the
        ;; rest is a haze nobody notices.
        :stops ((0.0 . 1.0) (0.55 . 1.0) (0.82 . 0.5) (1.0 . 0.0))
        :offset ,place
        ;; In on one frame and out over half a dozen: a hit that
        ;; arrives slowly is not a hit, and one that leaves as fast as
        ;; it came is over before the eye finds it.
        :envelope ((0.0 . 0.0)
                   (,(max 0.0 (- when-at 0.015)) . 0.0)
                   (,when-at . 1.0)
                   (,(min 1.0 (+ when-at 0.1)) . 0.0)
                   (1.0 . 0.0))))))

(defun smear-cursor--pacman-ghost (index color walk r &optional powered)
  "Return the layers for ghost INDEX in COLOR, chasing along WALK.

R is Pacman\='s radius.  A ghost is a round top on a straight body with
a pair of eyes that look the way it is going.  Each ghost trails
further back than the one before it and joins the chase a little
later.

Non-nil POWERED is a play he has the run of them in: they are drawn in
`smear-cursor--pacman-blue\=' and the one he catches is eaten there and
then."
  ;; Far enough back to read as a chase: five places was less than his
  ;; own radius, which sat them on top of him.
  (let* ((trail (smear-cursor--roam-trail walk (* 20 (1+ index))
                                          smear-cursor--roam-tail
                                          (and powered 12)))
         (start (min 0.6 (* 0.12 (1+ index))))
         ;; Nothing to chase along yet, so they come out one at a
         ;; time: with no tail behind him the trail falls back to where
         ;; he is standing, and they would arrive sitting on top of
         ;; him.  Once there is a tail they are already behind him and
         ;; there is nothing to hide, so they play full throughout.
         ;;
         ;; Full throughout matters twice over: a ghost is four shapes
         ;; drawn over each other, and a partial alpha composites the
         ;; overlaps denser than the rest; and the plays run back to
         ;; back, so an envelope that faded in and out at every one of
         ;; them was a chase that faded away every few seconds.
         (env (smear-cursor--pacman-gone-at
               (if smear-cursor--roam-tail
                   '((0.0 . 1.0) (1.0 . 1.0))
                 `((0.0 . 0.0) (,start . 1.0) (1.0 . 1.0)))
               walk
               (and powered
                    (smear-cursor--pacman-caught
                     walk trail (* r smear-cursor--pacman-touching)))))
         (color (if powered smear-cursor--pacman-blue color)))
    (list
     `(:shape radial
       :part ghost
       :color ,color
       :alpha 1.0
       :radius ,(* r 0.8)
       :stops ((0.0 . 1.0) (0.86 . 0.98) (1.0 . 0.0))
       :envelope ,env
       :offsets ,(vconcat (mapcar (lambda (p)
                                    (cons (car p) (- (cdr p) (* r 0.2))))
                                  trail)))
     `(:shape quad
       :part ghost
       :color ,color
       :alpha 1.0
       :envelope ,env
       :quads ,(vconcat
                (mapcar (lambda (p)
                          (let ((x (car p)) (y (cdr p)))
                            ;; Straight sides, drawn in a little at the
                            ;; foot, where the wavy skirt would be.
                            (vector (- x (* r 0.78)) (- y (* r 0.2))
                                    (+ x (* r 0.78)) (- y (* r 0.2))
                                    (+ x (* r 0.62)) (+ y (* r 0.85))
                                    (- x (* r 0.62)) (+ y (* r 0.85)))))
                        trail)))
     ;; Its eyes, after its body so they sit on it rather than under it.
     (smear-cursor--pacman-eye trail r env -1.0)
     (smear-cursor--pacman-eye trail r env 1.0))))

(defun smear-cursor--pacman-pellet (index count walk r)
  "Return the layer for pellet INDEX of COUNT, laid on the route WALK.

R is Pacman\='s radius.  Each sits further along than the last and goes
out as he reaches it, so they wink out in order."
  (let* ((at (/ (float (1+ index)) (1+ count)))
         (place (nth (min (1- (length walk))
                          (floor (* at (length walk))))
                     walk))
         (gone (min 0.98 (max 0.02 at))))
    `(:shape radial
      :part pellet
      :color ,smear-cursor-pacman-pellet-color
      :alpha 0.5
      :radius ,(max 1.2 (* r 0.13))
      :offset ,place
      ;; Laid down just before he gets there and gone as he passes.
      ;; All of them at once would put every frame's box around the
      ;; whole route, which is what an overlay charges for.
      :envelope ((0.0 . 0.0) (,(max 0.01 (- gone 0.22)) . 0.0)
                 (,(max 0.02 (- gone 0.16)) . 1.0) (,gone . 1.0)
                 (,(min 1.0 (+ gone 0.02)) . 0.0) (1.0 . 0.0)))))

(defcustom smear-cursor-pacman-size 1.2
  "How many lines tall Pacman is drawn, and the ghosts with him.

Two lines made him wider than the words he was eating.  A cell is a
whole number of pixels and the grid follows the size, so this moves in
steps of a pixel rather than smoothly, and it stops at
`smear-cursor--pacman-most-cells\=': each shape the drawing breaks into
costs a layer and a frame has a fixed number of those."
  :type 'number
  :group 'smear-cursor)

(defconst smear-cursor--pacman-most-cells 32
  "The most cells across Pacman is drawn.

A cap on the drawing rather than a size: each shape it breaks into
costs a layer, and a frame has a fixed number of those.

A pixel a cell, rather than two cells to the pixel.  A shape\='s sloped
side is a straight line laid over a staircase of cells, and it can sit
a step outside the staircase or a step inside it but not exactly on
it.  The step is the cell, so halving the cell halves every edge that
is not quite right: at two pixels those showed as spikes.")

(defconst smear-cursor--pacman-least-bands 6
  "The fewest eaten stretches kept back from the ghosts.

A stretch is a layer, and so is a quarter of a ghost.  Given the
choice the chase is worth less than the eating: four ghosts took every
layer that was left and he crossed the text without a mark on it,
which is the one thing the effect is for.")

(defconst smear-cursor--pacman-least-cells 12
  "The fewest cells across Pacman is drawn.

Below this the mouth is a notch of two or three cells and the chomp
cannot be read, which is the whole of what says he is eating.")

(defun smear-cursor--pacman-cells (lh)
  "Return how many cells across to draw Pacman on a line LH pixels tall.

A cell is a whole pixel, so the grid is the size: asked for a figure a
line and a half tall on an eighteen pixel line, he is drawn twenty
seven cells across and comes out twenty seven pixels.  Rounding the
cell instead left every size from 0.8 to 2.5 lines at one pixel a cell
on a thirty-two cell drawing, which is to say the setting did
nothing."
  (max smear-cursor--pacman-least-cells
       (min smear-cursor--pacman-most-cells
            (round (* smear-cursor-pacman-size lh)))))

(defconst smear-cursor--pacman-chomp '(0.0 0.5 1.0 0.5)
  "How far his mouth is open through one chomp.

Shut, half, wide, half, so the cycle ends where it began.  The number
is the slope of the mouth\='s edge, and one is a right angle.")

(defun smear-cursor--pacman-art (n open way)
  "Return Pacman as N rows of N cells, mouth OPEN, facing WAY.

WAY is `side\=' for facing left, or `up\=' or `down\='.

OPEN is the slope of the mouth\='s edge: nought is shut and one is a
right angle, wide open.  A slope rather than an angle because the cell
grid holds a slope exactly -- one cell across per cell down -- and so
each edge of the mouth comes out as one straight line.  Cut at an
angle the grid cannot hold, the edge steps one cell and then two, and
no straight line can be laid along it.

He is two half-circles turned apart, which is what the arcade does:
the outline stays a circle and the mouth is a slice out of it, with
its point at the middle."
  (let ((r (/ n 2.0)))
    (cl-loop
     for row below n
     collect
     (apply #'string
            (cl-loop
             for col below n
             collect
             (let* ((x (- (+ col 0.5) r))
                    (y (- (+ row 0.5) r))
                    ;; Turned so the mouth always opens along -A.
                    (a (pcase way ('up y) ('down (- y)) (_ x)))
                    (b (pcase way ((or 'up 'down) x) (_ y))))
               (if (and (<= (+ (* x x) (* y y)) (* r r))
                        (not (and (> open 0) (< a 0)
                                  (<= (abs b) (* open (- a))))))
                   ?o
                 ?\s)))))))

(defun smear-cursor--pacman-frames (cells way)
  "Return one chomp of Pacman facing WAY, CELLS cells across."
  (mapcar (lambda (open) (smear-cursor--pacman-art cells open way))
          smear-cursor--pacman-chomp))

(defvar smear-cursor--pacman-drawn nil
  "How many cells across the `pacman\=' sprite is drawn at, or nil.")

(defun smear-cursor--pacman-redraw (cells)
  "Draw the `pacman\=' sprite CELLS cells across, unless it already is.

Twelve frames of art each time, so it is worth not doing again for a
size that has not changed: the size is read every play and changes
when somebody sets it or moves to a frame with a different line
height."
  (unless (eq cells smear-cursor--pacman-drawn)
    (setq smear-cursor--pacman-drawn cells)
    (smear-cursor-define-sprite 'pacman
      :hold 2
      :palette '((?o . "#ffd23f"))
      :frames (smear-cursor--pacman-frames cells 'side)
      :frames-up (smear-cursor--pacman-frames cells 'up)
      :frames-down (smear-cursor--pacman-frames cells 'down))))

(smear-cursor--pacman-redraw smear-cursor--pacman-most-cells)

(defconst smear-cursor--pacman-reach 0.25
  "How wide Pacman eats, as a share of his width either side of him.

A share rather than a count of cells: the grid follows his size, and a
count that was a quarter of a thirty-two cell drawing would be his
whole head on a small one.

The part of the mouth that has closed, rather than how big he is.
Measured at his middle -- a reach of nothing -- a character went only
once he sat on top of it, and the whole of the open wedge hung over
text it was not eating.  Measured at his leading edge instead, the
background-coloured band filled the wedge and his mouth read as a
black hole.  Half his radius eats what the jaw has shut on and leaves
the outer wedge showing what is about to go.")

(defcustom smear-cursor-pacman-sprite 'pacman
  "Which sprite Pacman is drawn from.

Define your own with `smear-cursor-define-sprite' and name it here."
  :type 'symbol
  :group 'smear-cursor)

(defun smear-cursor--pacman ()
  "Build Pacman roaming the text beside the cursor and eating it.

He covers what he passes rather than removing it: the overlay is drawn
over the window, and what is covered comes back when the effect ends."
  (let* ((lh (float (frame-char-height)))
         (seed (+ smear-cursor-noise-seed (cl-incf smear-cursor--pacman-run)))
         (walk (smear-cursor--roam-route
                smear-cursor-pacman-rows
                (smear-cursor--roam-phases smear-cursor-pacman-duration
                                           smear-cursor--pacman-fps)
                seed))
         ;; Before anything is built from the route, because everything
         ;; is: the bands he eats, the ghosts behind him and the
         ;; drawing itself all follow it.
         (seen (smear-cursor--pacman-berserk-at
                (or smear-cursor--effect-window (selected-window))
                (ignore-errors
                  (smear-cursor--point-rect
                   (or smear-cursor--effect-window (selected-window))))
                walk))
         (berserk (and seen t))
         (walk (if seen
                   (smear-cursor--pacman-charge walk (plist-get seen :at)
                                                (plist-get seen :place)
                                                (plist-get seen :wide) lh)
                 walk))
         ;; A row above, a row below and the one he started on: he can
         ;; visit all of them, and a row without a band is a row where
         ;; he eats nothing.  Only when he is eating the text at all,
         ;; which he does not by default.
         ;; Drawn to the size asked for on this frame's line height,
         ;; which is what a cell being a whole pixel means: the grid is
         ;; the size.  Only the sprite this package draws; one somebody
         ;; else supplied is left as they wrote it.
         (_ (when (eq smear-cursor-pacman-sprite 'pacman)
              (smear-cursor--pacman-redraw (smear-cursor--pacman-cells lh))))
         (sprite (or (smear-cursor-sprite smear-cursor-pacman-sprite)
                     (error "smear-cursor: no sprite called %s"
                            smear-cursor-pacman-sprite)))
         (px (smear-cursor--sprite-pixel sprite smear-cursor-pacman-size lh))
         (across (car (smear-cursor--sprite-size sprite)))
         ;; Half his height: the ghosts and the pellets are measured
         ;; against him rather than against the line, because they are
         ;; chasing him.  Taken from the line height they stayed the
         ;; size they were when he was made smaller.
         (r (/ (* px across) 2.0))
         ;; He is served first.  The ghosts are chasing somebody, and
         ;; the text he has eaten says nothing without him over it.
         (figure (smear-cursor--sprite-layers
                  sprite walk px lh (- (smear-cursor--max-layers) 4)))
         (room (- (smear-cursor--max-layers) (length figure)))
         ;; The ghosts asked for are kept back, four layers each, but
         ;; never all of what is left: eating is what the effect is,
         ;; and a full chase took every layer and had him crossing the
         ;; text without a mark on it.  A ghost that will not fit
         ;; beside the eating is the one to go without.
         ;;
         ;; Counted in stretches rather than rows: a row he leaves and
         ;; comes back to further along is eaten in two of them, and
         ;; capping this at the rows he can reach had him giving a
         ;; stretch up to take another while layers went spare.
         (band-room (min room
                         (max smear-cursor--pacman-least-bands
                              (- room (* 4 (smear-cursor--pacman-ghosts))))))
         (reach (* px across smear-cursor--pacman-reach
                   (if berserk smear-cursor--pacman-berserk-bite 1.0)))
         (bands (when smear-cursor-pacman-eat-text
                  (smear-cursor--eaten-bands walk lh band-room reach)))
         ;; Four layers a ghost: a dome, a body and two eyes.  He
         ;; keeps whichever of them there is room for, so a short
         ;; round with little eaten gets the whole chase.
         ;; None at all while he is going for a word: they scatter,
         ;; and their dozen layers go to the eating and to the pair of
         ;; eyes he needs to face out with.  A chase and a charge at
         ;; once is two things happening where there is room for one.
         (ghosts (if seen 0
                   (max 0 (min (smear-cursor--pacman-ghosts)
                               (/ (- room (length bands)) 4)))))
         ;; The first ghost to catch him up, when this is not a play
         ;; he can eat them in.  One layer, and it comes before the
         ;; pellets: a ghost bouncing off him is something happening,
         ;; and a pellet is trimming.
         (clash (and (> ghosts 0)
                     (not (smear-cursor--pacman-powered-p))
                     (< (+ (* 4 ghosts) (length bands)) room)
                     (smear-cursor--pacman-clash
                      walk
                      (cl-loop for i below ghosts
                               thereis (smear-cursor--pacman-caught
                                        walk
                                        (smear-cursor--roam-trail
                                         walk (* 20 (1+ i))
                                         smear-cursor--roam-tail)
                                        (* r smear-cursor--pacman-touching)))
                      r)))
         (pellets (max 0 (min (- room (* 4 ghosts) (length bands)
                                 (if clash 1 0))
                              smear-cursor-pacman-pellets)))
         ;; Going out costs no layers of its own: it is an envelope
         ;; that ends early on the ones he is already drawn from, and
         ;; going red is the same again with the colour.
         (figure (if (and clash (not smear-cursor-pacman-immune)
                          (not berserk))
                     (mapcar (lambda (layer)
                               (plist-put (copy-sequence layer) :envelope
                                          (smear-cursor--pacman-gone-at
                                           (plist-get layer :envelope)
                                           walk (plist-get clash :at))))
                             figure)
                   figure))
         (figure (if berserk
                     (mapcar (lambda (layer)
                               (plist-put (copy-sequence layer) :color
                                          smear-cursor--pacman-berserk-color))
                             figure)
                   figure))
         ;; Two more layers, and only if they are going spare: the
         ;; stare is the best part of it, but it is not worth a row of
         ;; text going uneaten.
         (stare (and seen
                     (< (+ (* 4 ghosts) (length bands) (if clash 1 0) 2) room)
                     (smear-cursor--pacman-stare walk (plist-get seen :at)
                                                 (plist-get seen :place) r)))
         ;; The word itself, marked from the moment he notices it.
         (mark (and seen
                    (< (+ (length bands) (if clash 1 0) (if stare 2 0) 1) room)
                    (smear-cursor--pacman-mark walk (plist-get seen :at)
                                               (plist-get seen :place)
                                               (plist-get seen :wide) lh))))
    ;; The layers first, while the tail still says where the last play
    ;; left off: the bands and the ghosts behind him are both built
    ;; from it, and overwriting it first would have them chase this
    ;; play's own route and come out ahead of him.
    (setq smear-cursor--pacman-layers
          (append bands
                  ;; Under everything: it marks the text, and he goes
                  ;; over the top of it.
                  (and mark (list mark))
                  (cl-loop for i below pellets
                           collect (smear-cursor--pacman-pellet i pellets walk r))
                  (cl-loop for i below ghosts
                           append (smear-cursor--pacman-ghost
                                   i (nth (mod i (length
                                                  smear-cursor-pacman-ghost-colors))
                                          smear-cursor-pacman-ghost-colors)
                                   walk r (smear-cursor--pacman-powered-p)))
                  figure
                  ;; On his face, so after the figure.
                  stare
                  ;; Last, so it is on top of the two of them.  Under
                  ;; the ghost and under him it is a white disc behind
                  ;; whatever is bouncing off what, which is to say
                  ;; nothing anybody sees.
                  (and clash (list clash))))
    ;; Read the bands before folding this route into what has gone:
    ;; a band has to open where the last play left it.
    (when smear-cursor-pacman-eat-text
      (smear-cursor--eaten-remember walk lh reach band-room))
    ;; Far enough back for the ghosts behind him.
    (smear-cursor--roam-advance walk (* 20 (1+ ghosts)))
    (list :doc "Pacman roaming the text beside the cursor and eating it."
          :duration smear-cursor-pacman-duration
          :continuous t
          :fps smear-cursor--pacman-fps
          :color smear-cursor-pacman-color
          :shape 'point
          ;; Ghosts before him, so he passes in front of the one on his
          ;; heels rather than under it.
          :layers smear-cursor--pacman-layers)))

(smear-cursor-define-effect 'pacman #'smear-cursor--pacman)

;;; The janitor

;; He roams the same way Pacman does and leaves the text alone: a
;; faint streak of shine behind him, a broom that swings, and dust
;; going up off the line he has just done.

(defcustom smear-cursor-janitor-duration 3.0
  "Set how many seconds the janitor works for."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-janitor-rows 2
  "How many rows above and below the cursor the janitor covers."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-janitor-dust 3
  "How many puffs of dust he raises behind him."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-janitor-color "#cfd8e3"
  "Set the janitor\='s colour."
  :type 'color
  :group 'smear-cursor)

(defcustom smear-cursor-janitor-shine-color "#ffffff"
  "Set the colour of the streak he leaves behind."
  :type 'color
  :group 'smear-cursor)

(defcustom smear-cursor-janitor-dust-color "#b5a68d"
  "Set the colour of the dust he raises."
  :type 'color
  :group 'smear-cursor)

(defconst smear-cursor--janitor-fps 24
  "Frames a second for the janitor, and so places along his round.")

(defvar smear-cursor--janitor-round 0
  "Counts rounds, so he takes a new route each time.")

(defun smear-cursor--janitor-wipe (walk r lh)
  "Return the streak of shine the janitor leaves along WALK.

R is his size and LH the line height.  A couple of characters wide,
just behind him, and faint: the characters have to stay readable under
a wipe, which is the whole difference between cleaning them and
covering them."
  `(:shape quad
    :part wipe
    :color ,smear-cursor-janitor-shine-color
    :alpha 0.32
    :blur 3
    :envelope ((0.0 . 0.0) (0.08 . 1.0) (0.9 . 1.0) (1.0 . 0.0))
    :quads ,(vconcat
             (mapcar (lambda (p)
                       (let ((x (car p)) (y (cdr p))
                             (w (* r 2.2)) (h (/ lh 2.2)))
                         (vector (- x w) (- y h) (+ x (* r 0.4)) (- y h)
                                 (+ x (* r 0.4)) (+ y h) (- x w) (+ y h))))
                     walk))))

;; The palette is the Commodore 64's, which is where a figure this
;; size and this blocky comes from: sixteen fixed colours, and a
;; sprite twenty-odd cells tall built out of them.
(smear-cursor-define-sprite 'janitor
  :hold 4
  :palette '((?o . "#6C5EB5")      ; overalls, the C64's light blue
             (?h . "#959595")      ; shirt, light grey
             (?s . "#9A6759")      ; face and hands, light red
             (?c . "#444444")      ; cap, dark grey
             (?m . "#000000"))     ; moustache and boots
  ;; Facing left, and mirrored when he turns.  Two frames: a stride
  ;; and a step closed, which is a walk at four phases a frame.
  :frames '(("   cccc   "
             "  ccccc   "
             "  csss    "
             "   sss    "
             "  hhhhh   "
             " hhhhhh   "
             "   oooo   "
             "   oooo   "
             "   oooo   "
             "   oo oo  "
             "   oo oo  "
             "  mm   mm ")
            ("   cccc   "
             "  ccccc   "
             "  csss    "
             "   sss    "
             "  hhhhh   "
             " hhhhhh   "
             "   oooo   "
             "   oooo   "
             "   oooo   "
             "   oooo   "
             "   oo oo  "
             "   mm mm  ")))

(defcustom smear-cursor-janitor-size 2.0
  "How many lines tall the janitor is drawn.

A person the height of a line is a smudge; two lines is somebody
sweeping.  A cell is a whole number of pixels, so this moves in steps
rather than smoothly."
  :type 'number
  :group 'smear-cursor)

(defconst smear-cursor--janitor-reach 2.8
  "How wide the janitor cleans, in sprite cells either side of him.

The head of the mop and the swing of it: what the mop covers, rather
than how big he is.")

(defcustom smear-cursor-janitor-clean-text t
  "Whether the janitor takes the text he goes over.

He is cleaning, so it goes.  Nil leaves it there and he only walks the
rows with a shine behind him."
  :type 'boolean
  :group 'smear-cursor)

(defcustom smear-cursor-janitor-sprite 'janitor
  "Which sprite the janitor is drawn from.

Define your own with `smear-cursor-define-sprite' and name it here to
put somebody else on the round."
  :type 'symbol
  :group 'smear-cursor)

(defcustom smear-cursor-janitor-mop-color "#B8C76F"
  "Set the colour of the mop handle."
  :type 'color
  :group 'smear-cursor)

(defconst smear-cursor--janitor-hand '(-3.5 . -6.5)
  "Where the mop meets his hands, in sprite cells from his feet.")

(defun smear-cursor--janitor-mop-ends (walk i px)
  "Return the mop\\='s two ends at place I of WALK, PX pixels a cell.

A cons of (HAND-X . FOOT-X), in pixels either side of where he
stands.  The foot of it swings as he goes, which is the difference
between mopping and carrying a mop."
  (let* ((flip (smear-cursor--sprite-flip-p walk i))
         (sign (if flip -1.0 1.0))
         (swing (* 1.2 (sin (* i 0.55)))))
    (cons (* sign px (car smear-cursor--janitor-hand))
          (* sign px (+ -4.2 swing)))))

(defun smear-cursor--janitor-mop (walk px lh)
  "Return the mop the janitor pushes along WALK.

PX is a sprite cell in pixels and LH the line height.  A handle, drawn
as one slanted quad rather than a staircase of cells, and a head on
the floor at the end of it."
  (let ((env '((0.0 . 0.0) (0.06 . 1.0) (0.92 . 1.0) (1.0 . 0.0))))
    (list
     `(:shape quad
       :part mop
       :color ,smear-cursor-janitor-mop-color
       :alpha 1.0
       :envelope ,env
       :quads
       ,(vconcat
         (cl-loop
          for place in walk
          for i from 0
          collect
          (let* ((ends (smear-cursor--janitor-mop-ends walk i px))
                 (bx (car place)) (by (+ (cdr place) (/ lh 2.0)))
                 (hx (+ bx (car ends)))
                 (hy (+ by (* px (cdr smear-cursor--janitor-hand))))
                 (fx (+ bx (cdr ends)))
                 (w (* px 0.45)))
            (vector (- hx w) hy (+ hx w) hy
                    (+ fx w) by (- fx w) by)))))
     `(:shape quad
       :part mop-head
       :color ,smear-cursor-janitor-shine-color
       :alpha 0.92
       :envelope ,env
       :quads
       ,(vconcat
         (cl-loop
          for place in walk
          for i from 0
          collect
          (let* ((ends (smear-cursor--janitor-mop-ends walk i px))
                 (bx (+ (car place) (cdr ends)))
                 (by (+ (cdr place) (/ lh 2.0)))
                 (w (* px 1.6)))
            (vector (- bx w) (- by px) (+ bx w) (- by px)
                    (+ bx w) by (- bx w) by))))))))

(defun smear-cursor--janitor-dust (index count walk r)
  "Return one puff of dust, INDEX of COUNT, raised along WALK.

R is the janitor\='s size.  Each puff starts where he was a moment
earlier and goes up as it thins out."
  (let* ((at (/ (float (1+ index)) (1+ count)))
         (n (length walk))
         (from (max 0 (min (1- n) (floor (* at n)))))
         (place (nth from walk))
         (gone (min 0.98 (+ at 0.28))))
    `(:shape radial
      :part dust
      :color ,smear-cursor-janitor-dust-color
      :alpha 0.7
      :radius ,(* r 0.55)
      :envelope ((0.0 . 0.0) (,(max 0.01 (- at 0.02)) . 0.0)
                 (,at . 0.8) (,gone . 0.0) (1.0 . 0.0))
      ;; Up and a little back, thinning as it goes.
      :offsets ,(vconcat
                 (cl-loop for p below n
                          collect (let ((rise (* r 1.4 (/ (float p) n))))
                                    (cons (- (car place) (* r 0.3))
                                          (- (cdr place) rise))))))))

(defun smear-cursor--janitor ()
  "Build the janitor going over the text beside the cursor.

He leaves it exactly as he found it: a faint streak of shine behind
him and some dust in the air, and nothing covered over."
  (let* ((lh (float (frame-char-height)))
         (r (* lh 0.62))
         (seed (+ smear-cursor-noise-seed (cl-incf smear-cursor--janitor-round)))
         (walk (smear-cursor--roam-route
                smear-cursor-janitor-rows
                (smear-cursor--roam-phases smear-cursor-janitor-duration
                                           smear-cursor--janitor-fps)
                seed))
         (sprite (or (smear-cursor-sprite smear-cursor-janitor-sprite)
                     (error "smear-cursor: no sprite called %s"
                            smear-cursor-janitor-sprite)))
         (px (smear-cursor--sprite-pixel sprite smear-cursor-janitor-size lh))
         ;; He and his mop come first, then the floor he has cleared.
         ;; A module that cannot hold a whole figure gets a plainer
         ;; one rather than a butchered one.
         (figure (smear-cursor--sprite-layers
                  sprite walk px lh (- (smear-cursor--max-layers) 6)))
         (mop (smear-cursor--janitor-mop walk px lh))
         (reach (* px smear-cursor--janitor-reach))
         (band-room (max 0 (- (smear-cursor--max-layers)
                              (length figure) (length mop) 1)))
         (bands (when smear-cursor-janitor-clean-text
                  (smear-cursor--eaten-bands walk lh band-room reach)))
         ;; The shine only when he is not clearing the line.  It is
         ;; there to say a stretch has been cleaned without changing
         ;; it, and once the stretch is actually gone it is saying
         ;; nothing, so the layer goes to the dust instead.
         (shine (unless smear-cursor-janitor-clean-text
                  (list (smear-cursor--janitor-wipe walk r lh))))
         (dust (max 0 (min smear-cursor-janitor-dust
                           (- (smear-cursor--max-layers) (length figure)
                              (length mop) (length bands) (length shine))))))
    ;; Read the bands before folding this route into what has gone.
    (when smear-cursor-janitor-clean-text
      (smear-cursor--eaten-remember walk lh reach band-room))
    (smear-cursor--roam-advance walk 1)
    (list :doc "A janitor going over the text beside the cursor."
          :duration smear-cursor-janitor-duration
          :continuous t
          :fps smear-cursor--janitor-fps
          :color smear-cursor-janitor-color
          :shape 'point
          :layers
          (append
           bands
           shine
           (cl-loop for i below dust
                    collect (smear-cursor--janitor-dust i dust walk r))
           figure
           mop))))

(smear-cursor-define-effect 'janitor #'smear-cursor--janitor)

;;; The saucer

;; It hovers over a line, puts a beam down and lifts the characters up
;; into itself.  What it has taken is covered the way Pacman's meals
;; are: the same machinery, a different excuse for it.

(defcustom smear-cursor-ufo-duration 3.0
  "Set how many seconds the saucer works a line for."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-ufo-rows 2
  "How many rows above and below the cursor the saucer visits."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-ufo-size 1.6
  "How many lines tall the saucer is drawn.

It flies above the line it is working, so it is drawn smaller than the
figures that stand on one.  A cell is a whole number of pixels, so
this moves in steps rather than smoothly."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-ufo-sprite 'ufo
  "Which sprite the saucer is drawn from.

Define your own with `smear-cursor-define-sprite' and name it here."
  :type 'symbol
  :group 'smear-cursor)

(defcustom smear-cursor-ufo-color "#9fb3c8"
  "Set the saucer\='s colour, used for anything the palette leaves out."
  :type 'color
  :group 'smear-cursor)

(defcustom smear-cursor-ufo-beam-color "#9ef7cf"
  "Set the colour of the beam the saucer puts down."
  :type 'color
  :group 'smear-cursor)

(defcustom smear-cursor-ufo-take-text t
  "Whether the saucer takes the characters it beams up.

Nil leaves the text alone and it only shines a light on it."
  :type 'boolean
  :group 'smear-cursor)

(defconst smear-cursor--ufo-fps 24
  "Frames a second for the saucer.")

(defconst smear-cursor--ufo-beam-foot 2.4
  "How wide the beam lands, in characters either side of the saucer.

The band it empties is measured from this rather than from the size of
the ship: it is the beam that does the taking, and text left standing
inside the light is text the light is not taking.")

(defconst smear-cursor--ufo-lift 18
  "Phases one character takes to travel up the beam.")

(defvar smear-cursor--ufo-run 0
  "Counts runs, so the saucer takes a new route each time.")

(smear-cursor-define-sprite 'ufo
  :hold 3
  :palette '((?h . "#9fb3c8")      ; hull, light grey-blue
             (?d . "#70A4B2")      ; dome, the C64's cyan
             (?l . "#EEEE77"))     ; lights, its yellow
  ;; Two frames, differing only in the lights, which is the whole of
  ;; the animation a hovering saucer needs.
  :frames '(("    dddd    "
             "    dddd    "
             " hhhhhhhhhh "
             " hhhhhhhhhh "
             "hhhhhhhhhhhh"
             "  ll    ll  ")
            ("    dddd    "
             "    dddd    "
             " hhhhhhhhhh "
             " hhhhhhhhhh "
             "hhhhhhhhhhhh"
             " ll      ll ")))

(defun smear-cursor--ufo-sky (walk lift)
  "Return WALK raised by LIFT pixels, which is where the saucer flies.

It works a line without standing on it: the beam is the part that
reaches the text."
  (mapcar (lambda (p) (cons (car p) (- (cdr p) lift))) walk))

(defun smear-cursor--ufo-beam (walk lift lh cw)
  "Return the beam the saucer puts down along WALK.

LIFT is how high it flies, LH the line height and CW a character\='s
width.  Narrow where it leaves the hull and wide where it lands, so it
reads as a cone rather than a post."
  `(:shape quad
    :part beam
    :color ,smear-cursor-ufo-beam-color
    :alpha 0.3
    :blur 2
    :envelope ((0.0 . 0.0) (0.1 . 1.0) (0.9 . 1.0) (1.0 . 0.0))
    :quads ,(vconcat
             (mapcar
              (lambda (p)
                (let* ((x (car p))
                       (foot (+ (cdr p) (/ lh 2.0)))
                       (top (- foot lift))
                       (wide (* cw smear-cursor--ufo-beam-foot)))
                  (vector (- x cw) top (+ x cw) top
                          (+ x wide) foot (- x wide) foot)))
              walk))))

(defun smear-cursor--ufo-catch (walk lift lh cw)
  "Return the character going up the beam along WALK.

LIFT is how high the saucer flies, LH the line height and CW a
character's width.  One after another: it rises from the line to the
hull, and the next starts as soon as it is in.  Drawn in the
foreground colour, because what is going up is a piece of the text."
  `(:shape quad
    :part caught
    :color ,(smear-cursor--foreground-color)
    :alpha 0.85
    :envelope ((0.0 . 0.0) (0.12 . 1.0) (0.88 . 1.0) (1.0 . 0.0))
    :quads ,(vconcat
             (cl-loop
              for p in walk
              for i from 0
              collect
              (let* ((x (car p))
                     (foot (+ (cdr p) (/ lh 2.0)))
                     ;; How far up this one has got, and a new one
                     ;; behind it once it is home.
                     (u (/ (float (mod i smear-cursor--ufo-lift))
                           smear-cursor--ufo-lift))
                     (y (- foot (* u lift)))
                     (w (* cw 0.4))
                     (h (/ lh 2.6)))
                (vector (- x w) (- y h) (+ x w) (- y h)
                        (+ x w) (+ y h) (- x w) (+ y h)))))))

(defun smear-cursor--ufo ()
  "Build a saucer working the text beside the cursor.

It hovers a couple of lines up, puts a beam down and takes what is
under it."
  (let* ((lh (float (frame-char-height)))
         (cw (float (frame-char-width)))
         (lift (* lh 2.2))
         (seed (+ smear-cursor-noise-seed (cl-incf smear-cursor--ufo-run)))
         (walk (smear-cursor--roam-route
                smear-cursor-ufo-rows
                (smear-cursor--roam-phases smear-cursor-ufo-duration
                                           smear-cursor--ufo-fps)
                seed))
         (sprite (or (smear-cursor-sprite smear-cursor-ufo-sprite)
                     (error "smear-cursor: no sprite called %s"
                            smear-cursor-ufo-sprite)))
         (px (smear-cursor--sprite-pixel sprite smear-cursor-ufo-size lh))
         ;; The beam and what is going up it take two, and the ship
         ;; itself comes before the text it has emptied.
         (figure (smear-cursor--sprite-layers
                  sprite (smear-cursor--ufo-sky walk lift) px lh
                  (- (smear-cursor--max-layers) 3)))
         (band-room (max 0 (- (smear-cursor--max-layers) (length figure) 2)))
         (bands (when smear-cursor-ufo-take-text
                  (smear-cursor--eaten-bands
                   walk lh band-room (* cw smear-cursor--ufo-beam-foot)))))
    (when smear-cursor-ufo-take-text
      (smear-cursor--eaten-remember
       walk lh (* cw smear-cursor--ufo-beam-foot) band-room))
    (smear-cursor--roam-advance walk 1)
    (list :doc "A saucer beaming the text beside the cursor up into itself."
          :duration smear-cursor-ufo-duration
          :continuous t
          :fps smear-cursor--ufo-fps
          :color smear-cursor-ufo-color
          :shape 'point
          :layers (append bands
                          (list (smear-cursor--ufo-beam walk lift lh cw)
                                (smear-cursor--ufo-catch walk lift lh cw))
                          figure))))

(smear-cursor-define-effect 'ufo #'smear-cursor--ufo)

;;; The Grinch

;; He goes along the line taking the characters and putting them in
;; his sack.  Same round as the janitor, opposite intent: the janitor
;; leaves the text as he found it and the Grinch leaves a gap.

(defcustom smear-cursor-grinch-duration 3.0
  "Set how many seconds the Grinch works a line for."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-grinch-rows 2
  "How many rows above and below the cursor the Grinch visits."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-grinch-size 2.0
  "How many lines tall the Grinch is drawn.

A cell is a whole number of pixels, so this moves in steps rather than
smoothly."
  :type 'number
  :group 'smear-cursor)

(defconst smear-cursor--grinch-reach 4.0
  "How wide the Grinch takes, in sprite cells either side of him.

His body rather than the grid he is drawn on: the sack beside him
takes up half of that and he is not carrying it through the text.")

(defcustom smear-cursor-grinch-sprite 'grinch
  "Which sprite the Grinch is drawn from.

Define your own with `smear-cursor-define-sprite' and name it here."
  :type 'symbol
  :group 'smear-cursor)

(defcustom smear-cursor-grinch-color "#588D43"
  "Set the Grinch\='s colour, used for anything the palette leaves out."
  :type 'color
  :group 'smear-cursor)

(defcustom smear-cursor-grinch-take-text t
  "Whether the Grinch takes the characters he passes.

Nil leaves the text alone and he only walks past it."
  :type 'boolean
  :group 'smear-cursor)

(defconst smear-cursor--grinch-fps 24
  "Frames a second for the Grinch.")

(defvar smear-cursor--grinch-run 0
  "Counts runs, so he takes a new route each time.")

(smear-cursor-define-sprite 'grinch
  :hold 4
  :palette '((?b . "#664400")      ; the sack, the C64's brown
             (?g . "#588D43")      ; him, its green
             (?r . "#68372B")      ; the hat, its red
             (?w . "#FFFFFF")      ; the hat's trim
             (?m . "#000000"))     ; his feet
  ;; Facing left, with the sack over the shoulder behind him, and two
  ;; frames of a walk.
  :frames '(("   rrrr       "
             "   wwww       "
             "   gggg bbbb  "
             "   ggggbbbbbb "
             "  ggggggbbbbb "
             "  ggggggbbbbb "
             "  gggggg bbb  "
             "  gggggg      "
             "   gg gg      "
             "   gg gg      "
             "  mm   mm     ")
            ("   rrrr       "
             "   wwww       "
             "   gggg bbbb  "
             "   ggggbbbbbb "
             "  ggggggbbbbb "
             "  ggggggbbbbb "
             "  gggggg bbb  "
             "  gggggg      "
             "   gggg       "
             "   gg gg      "
             "  mm    mm    ")))

(defun smear-cursor--grinch ()
  "Build the Grinch working the text beside the cursor.

He walks the rows and the characters he passes go into the sack."
  (let* ((lh (float (frame-char-height)))
         (seed (+ smear-cursor-noise-seed (cl-incf smear-cursor--grinch-run)))
         (walk (smear-cursor--roam-route
                smear-cursor-grinch-rows
                (smear-cursor--roam-phases smear-cursor-grinch-duration
                                           smear-cursor--grinch-fps)
                seed))
         (sprite (or (smear-cursor-sprite smear-cursor-grinch-sprite)
                     (error "smear-cursor: no sprite called %s"
                            smear-cursor-grinch-sprite)))
         (px (smear-cursor--sprite-pixel sprite smear-cursor-grinch-size lh))
         ;; Him first: the gap in the text is the joke only while
         ;; there is somebody standing next to it holding a sack.
         (figure (smear-cursor--sprite-layers
                  sprite walk px lh (- (smear-cursor--max-layers) 1)))
         (grab (* px smear-cursor--grinch-reach))
         (band-room (max 0 (- (smear-cursor--max-layers) (length figure))))
         (bands (when smear-cursor-grinch-take-text
                  (smear-cursor--eaten-bands walk lh band-room grab))))
    (when smear-cursor-grinch-take-text
      (smear-cursor--eaten-remember walk lh grab band-room))
    (smear-cursor--roam-advance walk 1)
    (list :doc "The Grinch putting the text beside the cursor in his sack."
          :duration smear-cursor-grinch-duration
          :continuous t
          :fps smear-cursor--grinch-fps
          :color smear-cursor-grinch-color
          :shape 'point
          :layers (append bands figure))))

(smear-cursor-define-effect 'grinch #'smear-cursor--grinch)


;;;; Effect triggers

(defconst smear-cursor--redisplay-tries 3
  "Limit how often a measurement can be deferred for redisplay.")

(defun smear-cursor--again-soon (fn tries)
  "Schedule FN one frame later with one fewer TRIES."
  (run-at-time (/ 1.0 (max 1 smear-cursor-fps)) nil fn (1- tries)))

(defcustom smear-cursor-effects
  '((pulse   . line-pulse)
    (copy    . region-flash)
    (delete  . region-fade)
    (yank    . region-arrive)
    (insert  . nil)
    (newline . nil))
  "Map each occasion to an effect with (OCCASION . EFFECT) entries.

Occasions are pulse (landing after a jump), copy, delete, yank, insert
and newline.  A nil or undefined effect disables that occasion.  Insert
and newline are disabled by default.

Newline is the return key: a bigger thing than a letter, since the line
ends, the cursor drops and everything below it moves, where insert is
written to be quiet enough to fire on every keystroke.  With nothing set
for it a newline is marked as an insert like any other character.

These effects require the `x11' backend.  See `smear-cursor-define-effect'
to define effects."
  :type '(alist :key-type symbol :value-type (choice symbol (const nil)))
  :group 'smear-cursor)

(defcustom smear-cursor-pulse-min-rows 25
  "Pulse the landing line after any movement of at least this many rows.

Use nil to pulse only after commands in `smear-cursor-pulse-commands'."
  :type '(choice (const :tag "only the listed commands" nil) number)
  :group 'smear-cursor)

(defcustom smear-cursor-pulse-commands
  '(xref-find-definitions xref-go-back xref-pop-marker-stack
    avy-goto-char-timer avy-goto-line ace-window
    recenter-top-bottom scroll-up-command scroll-down-command
    beginning-of-buffer end-of-buffer
    isearch-repeat-forward isearch-repeat-backward)
  "List commands after which the landing line is pulsed."
  :type '(repeat function)
  :group 'smear-cursor)

(defun smear-cursor--effect-layer-color (layer default)
  "Return LAYER's own color, or DEFAULT."
  (let ((c (plist-get layer :color)))
    (cond ((vectorp c) c)
          ((stringp c) (or (smear-cursor--color-rgb c) default))
          (t default))))

(defun smear-cursor--effect-color (effect)
  "Return EFFECT's color as [R G B].

Prefer `smear-cursor-effect-color' over the effect's own color.  Either
may be `trail'; unrecognized colors also use the trail color."
  (let ((c (or smear-cursor-effect-color (plist-get effect :color))))
    (cond ((eq c 'trail) (smear-cursor--trail-color))
          ((vectorp c) c)
          ((stringp c) (or (smear-cursor--color-rgb c)
                           (smear-cursor--trail-color)))
          (t (smear-cursor--trail-color)))))

(defun smear-cursor--effect-stage (win)
  "Return a stage with a running player for WIN's effects, or nil."
  (when (and (eq smear-cursor-backend 'x11)
             (smear-cursor--x11-available-p)
             (fboundp 'smear-cursor-x11-stage))
    (let ((stage (ignore-errors (smear-cursor-x11-stage (window-frame win)))))
      (and stage
           (not (smear-cursor-x11--trouble stage))
           (smear-cursor--x11-player stage)
           stage))))

(defun smear-cursor--rect-in-frame (win rect)
  "Convert RECT from WIN's text-area coordinates to frame pixels."
  (let ((xy (smear-cursor-x11--frame-xy win (aref rect 0) (aref rect 1))))
    (vector (float (car xy)) (float (cdr xy))
            (float (aref rect 2)) (float (aref rect 3)))))

(defun smear-cursor--pos-row (win pos)
  "Return the screen row of POS in WIN using display row numbering."
  (let ((p (ignore-errors (posn-at-point pos win))))
    (and p (cdr (posn-actual-col-row p)))))

(defun smear-cursor--pos-rect (win pos)
  "Return the glyph rectangle [X Y W H] at POS in WIN, or nil if hidden.

Use the screen line's height and y coordinate when available.  Call
`pos-visible-in-window-p' with PARTIALLY nil."
  (let ((xy (and (>= pos (point-min)) (<= pos (point-max))
                 (pos-visible-in-window-p pos win t))))
    (when (consp xy)
      (let* ((row (smear-cursor--pos-row win pos))
             (box (and row (window-line-height row win)))
             (h (or (car-safe box)
                    (with-selected-window win
                      (save-excursion (goto-char pos) (line-pixel-height)))))
             (y (if (car-safe box) (nth 2 box) (nth 1 xy))))
        (when (and h y (> h 0))
          (vector (float (nth 0 xy)) (float y)
                  (float (frame-char-width (window-frame win)))
                  (float h)))))))

(defun smear-cursor--line-rect (win)
  "Return the rectangle of point's screen line in WIN's text area."
  (let ((r (smear-cursor--point-rect win)))
    (when r
      (vector 0.0 (aref r 1) (float (window-text-width win t)) (aref r 3)))))

(defun smear-cursor--region-last (beg end)
  "Return the position whose glyph ends the region BEG to END.

A region that ends where a line begins does not reach into that line:
it ends with the newline before it, and a newline is drawn at the end
of the line above.  `kill-whole-line\=' and the slick-cut idiom both hand
over a region like that, and taken at face value the flash covered the
line and one character of the next."
  (if (and (> end beg) (save-excursion (goto-char end) (bolp)))
      (1- end)
    end))

(defun smear-cursor--region-rects (win beg end)
  "Return rectangles covering BEG to END in WIN.

Return nil if neither endpoint is visible."
  (let ((b (smear-cursor--pos-rect win (min beg end)))
        (e (smear-cursor--pos-rect
            win (smear-cursor--region-last (min beg end) (max beg end)))))
    (when (and b e)
      (smear-cursor--rects-between b e 0.0
                                   (float (window-text-width win t))))))

(defcustom smear-cursor-effect-fps nil
  "Set effect frames per second, or use nil for `smear-cursor-fps'.

The rate is capped at `smear-cursor-fps'.  Effects with a shared opacity
envelope need only one rendering and upload."
  :type '(choice (const :tag "the frame rate" nil) number)
  :group 'smear-cursor)

(defun smear-cursor--effect-rate (&optional effect)
  "Return the effect frame rate in frames per second.

EFFECT may ask for a rate of its own with `:fps', which a rate set in
`smear-cursor-effect-fps' overrides.  No effect outruns the trail."
  (let ((want (or smear-cursor-effect-fps
                  (and effect (plist-get effect :fps))
                  smear-cursor-fps)))
    (float (max 1 (min (max 1 smear-cursor-fps) (max 1 want))))))

(defconst smear-cursor--frozen-share 0.25
  "How much of a flight plays over its photograph of the text.

A deletion is marked from `before-change-functions', so the flash is
built while the text is still there and drawn once it has gone: the
photograph is what the first frames stand on.  Kept for the whole
flight it also keeps the deleted line on the screen for as long as the
effect runs, and the edit reads as having lagged by that much.  A
quarter is long enough to see the flash land on the text and short
enough that the line goes when the key was pressed.")

(defun smear-cursor--frozen-frames (n)
  "Return how many of a flight\='s N frames play over the photograph."
  (max 1 (round (* n smear-cursor--frozen-share))))

(defun smear-cursor--play-effect (win name rects track &optional frozen)
  "Play effect NAME over RECTS in WIN on TRACK.

Non-nil FROZEN captures pixels before playback and restores them under
each frame, for deleted text.  Skip freezing if unsupported.  Missing
display support, modules, or visible rectangles produce no effect."
  ;; The stage first: opening it is what loads the module, and the
  ;; module is what says how many layers an effect may use.  Built the
  ;; other way round, the first effect of a session is built to a
  ;; guess made before there was anything to ask.
  (let* ((stage (and rects (smear-cursor--effect-stage win)))
         (effect (let ((smear-cursor--effect-window win))
                   (smear-cursor-effect name))))
    (setq smear-cursor--last-effect (cons name effect))
    (smear-cursor--trace "%-28s effect %s on track %d over %d rect(s)%s"
                         (or this-command smear-cursor--last-command "-")
                         name track (length rects)
                         (cond ((not effect) "  -- no such effect")
                               ((not stage) "  -- nowhere to draw it")
                               (t "")))
    (when (plist-get effect :play)
      (funcall (plist-get effect :play) win rects))
    (when (and effect stage (not (plist-get effect :play)))
      (let* ((frame-rects (mapcar (lambda (r) (smear-cursor--rect-in-frame win r))
                                  rects))
             (pairs (smear-cursor--effect-layers effect frame-rects))
             (rate (smear-cursor--effect-rate effect))
             (n (max 2 (round (* (or (plist-get effect :duration) 0.3) rate))))
             (color (smear-cursor--effect-color effect))
             (zeros (make-vector 8 0.0))
             (nohead (vector 0.0 0.0)))
        (when pairs
          (let ((hold (and frozen
                           (fboundp 'smear-cursor-x11--freeze)
                           (ignore-errors
                             (apply #'smear-cursor-x11--freeze stage
                                    (smear-cursor--effect-box pairs)))
                           (smear-cursor--frozen-frames n)))
                (layers (vconcat
                         (mapcar (lambda (pair)
                                   (smear-cursor--layer-vector
                                    (smear-cursor--effect-layer (car pair))
                                    zeros nohead
                                    (smear-cursor--effect-layer-color
                                     (car pair) color)))
                                 pairs)))
                (frames (smear-cursor--effect-frames effect frame-rects n))
                (fps rate))
            ;; Try the longest supported argument list first.  Modules that accept
            ;; only five arguments otherwise fail silently for every effect.
            (let ((still (and (smear-cursor--effect-still-p effect) t)))
              (or (ignore-errors
                    (smear-cursor-x11--play stage track layers frames fps
                                            hold still))
                  (ignore-errors
                    (smear-cursor-x11--play stage track layers frames fps
                                            hold))
                  (ignore-errors
                    (smear-cursor-x11--play stage track layers frames fps)))))
          t)))))

(defcustom smear-cursor-scan-passes 4
  "How many times a line scan throws the trail across the line.

The last is back to the cursor, so an even number ends where it
started and an odd one leaves the trail settling somewhere else."
  :type 'integer
  :group 'smear-cursor)

(defcustom smear-cursor-scan-pace 0.09
  "Seconds between one throw of a line scan and the next.

Short enough that the trail is caught mid-flight and thrown back the
other way, which is what makes it whip rather than travel: retargeted
before it has settled, the smear never shortens."
  :type 'number
  :group 'smear-cursor)

(defun smear-cursor--scan-ends (win)
  "Return the cursor rectangle and the two ends of its line in WIN.

A list of (HERE NEAR FAR).  The ends are the cursor\='s own rectangle
moved to either end of the window\='s text, so what is thrown about is
the cursor rather than a shape the width of the line."
  (let ((here (smear-cursor--point-rect win)))
    (when here
      (let ((wide (float (window-text-width win t))))
        (list here
              (vector 0.0 (aref here 1) (aref here 2) (aref here 3))
              (vector (max 0.0 (- wide (aref here 2)))
                      (aref here 1) (aref here 2) (aref here 3)))))))

;;;###autoload
(defun smear-cursor--scan (win)
  "Throw the trail from one end of WIN\='s current line to the other.

The last throw is back to the cursor, so it settles where the cursor
actually is."
  (let ((ends (smear-cursor--scan-ends win)))
    (when ends
      (let* ((here (nth 0 ends))
             (passes (max 1 smear-cursor-scan-passes))
             (throw-at
              (lambda (i)
                (smear-cursor--start
                 win (or (smear-cursor--origin-rect win (window-buffer win))
                         here)
                 (cond ((= i (1- passes)) here)
                       ((cl-evenp i) (nth 2 ends))
                       (t (nth 1 ends)))))))
        (dotimes (i passes)
          (if (zerop i)
              (funcall throw-at 0)
            (run-at-time (* i smear-cursor-scan-pace) nil throw-at i)))))))

;;;###autoload
(defun smear-cursor-scan-line ()
  "Sweep the cursor\='s trail along the line to say where the cursor is.

What this is meant to look like is what holding down end-of-line and
beginning-of-line looks like, so it is the same thing: the trail,
aimed at one end and then the other before it has settled.  A shape of
its own drawn to imitate it is not close.

The last throw is back to the cursor, so it settles where the cursor
actually is."
  (interactive)
  (when (and smear-cursor-mode (display-graphic-p))
    (smear-cursor--scan (selected-window))))

(smear-cursor-define-effect 'line-scan
  ;; A function rather than a plist: how long a scan runs is the pace
  ;; and the number of passes, both of which are settings, and whatever
  ;; paces a preview should ask now rather than when this file loaded.
  (lambda ()
    (list :doc "Sweep the trail along the line, the way holding end-of-line does."
          :shape 'region
          :duration (* (max 1 smear-cursor-scan-passes) smear-cursor-scan-pace)
          ;; The rectangles say which line; the scan works the rest out
          ;; from the window, because what it throws is the cursor
          ;; rather than a shape the width of the line.
          :play (lambda (win _rects) (smear-cursor--scan win)))))

(defun smear-cursor--effect-for (occasion)
  "Return the effect name configured for OCCASION, or nil."
  (cdr (assq occasion smear-cursor-effects)))

(defun smear-cursor--fire-region-effect (occasion beg end &optional frozen)
  "Mark BEG to END for OCCASION in the selected window.

Use FROZEN for text about to be deleted; see `smear-cursor--play-effect'."
  (let ((name (smear-cursor--effect-for occasion)))
    (when (and name smear-cursor-mode (display-graphic-p))
      (let ((win (selected-window)))
        (smear-cursor--play-effect
         win name (smear-cursor--region-rects win beg end)
         smear-cursor--track-occasion frozen)))))

;;;###autoload
(defun smear-cursor-pulse-line ()
  "Pulse the line at point by blending over its text."
  (interactive)
  (let ((name (smear-cursor--effect-for 'pulse))
        (win (selected-window)))
    (when (and name (display-graphic-p))
      (smear-cursor--play-effect
       win name (let ((r (smear-cursor--line-rect win))) (and r (list r)))
       smear-cursor--track-occasion))))

(defun smear-cursor--maybe-pulse ()
  "Schedule a landing line pulse after a configured movement command.

Run from `post-command-hook' to check `this-command'."
  (when (and smear-cursor-mode
             (memq this-command smear-cursor-pulse-commands)
             (smear-cursor--effect-for 'pulse)
             (not (minibufferp)))
    (smear-cursor--pulse-soon)))

(defvar smear-cursor--pulse-timer nil
  "Store the pending deferred pulse timer to combine consecutive requests.")

(defun smear-cursor--pulse-soon ()
  "Schedule a landing line pulse after redisplay settles."
  (unless smear-cursor--pulse-timer
    (setq smear-cursor--pulse-timer
          (run-at-time 0 nil #'smear-cursor--pulse-try
                       smear-cursor--redisplay-tries))))

(defun smear-cursor--pulse-try (tries)
  "Pulse the landing line, retrying up to TRIES times if it is off screen."
  (setq smear-cursor--pulse-timer nil)
  (unless (or (ignore-errors (smear-cursor-pulse-line)) (<= tries 0))
    (setq smear-cursor--pulse-timer
          (smear-cursor--again-soon #'smear-cursor--pulse-try tries))))

(defvar smear-cursor--copy-last nil
  "Store the last copy's (BEG END) and timestamp to avoid duplicate flashes.")

(defun smear-cursor--effect-names ()
  "Return a list of all defined effect names."
  (let (names)
    (maphash (lambda (k _v) (push k names)) smear-cursor--effects)
    (sort names #'string<)))

;;;###autoload
(defun smear-cursor-flash (effect &optional beg end)
  "Play EFFECT over BEG to END, the active region, or the current line.

Call interactively to choose an effect, or from Lisp to mark text
directly.  See `smear-cursor-define-effect' for effect definitions."
  (interactive
   (list (intern (completing-read
                  "Flash effect: "
                  (mapcar #'symbol-name (smear-cursor--effect-names))
                  nil t))))
  (let ((win (selected-window)))
    (smear-cursor--play-effect
     win effect
     (cond ((and beg end) (smear-cursor--region-rects win (min beg end)
                                                      (max beg end)))
           ((region-active-p)
            (smear-cursor--region-rects win (region-beginning) (region-end)))
           (t (let ((r (smear-cursor--line-rect win))) (and r (list r)))))
     smear-cursor--track-occasion)))

(defun smear-cursor--copy-effect (&optional beg end &rest _)
  "Flash copied text from BEG to END after a copy command.

Use the command's bounds rather than the active region.  Additional
arguments _ are ignored."
  (when (and smear-cursor-mode (numberp beg) (numberp end) (/= beg end))
    (let ((this (list (min beg end) (max beg end)))
          (now (float-time)))
      ;; Suppress duplicate copy effects.  Calls through both `kill-ring-save'
      ;; and `copy-region-as-kill' can be a third of a second apart because
      ;; `indicate-copied-region' sleeps, restarting an already finished flash.
      (unless (and smear-cursor--copy-last
                   (equal this (car smear-cursor--copy-last))
                   (< (- now (cdr smear-cursor--copy-last)) 0.5))
        (setq smear-cursor--copy-last (cons this now))
        (smear-cursor--flourish
          (smear-cursor--fire-region-effect 'copy (car this) (cadr this)))))))

(defcustom smear-cursor-delete-min-chars 2
  "Set the minimum number of deleted characters that triggers a flash.

Use one to mark every deletion, or a higher value to skip smaller changes."
  :type 'integer
  :group 'smear-cursor)

(defun smear-cursor--yank-effect (&rest _)
  "Flash pasted text between mark and point after a yank command.

Advice arguments _ are ignored."
  (when (and smear-cursor-mode
             (not (smear-cursor--minibuffer-p))
             (not (smear-cursor--child-frame-showing-p)))
    (smear-cursor--flourish
      (let ((beg (mark t))
            (end (point)))
        (when (and beg end (/= beg end))
          (smear-cursor--fire-region-effect
           'yank (min beg end) (max beg end)))))))

(defvar smear-cursor--delete-last 0.0
  "When the delete effect last played, in `float-time' seconds.")

(defun smear-cursor--delete-forget ()
  "Forget when the delete effect last played."
  (setq smear-cursor--delete-last 0.0))

(defun smear-cursor--delete-again-p ()
  "Return non-nil when a deletion is worth marking rather than skipping.

While the last fade is still playing there is nothing a second one can
add: it draws in the same colour over text that has already gone.
Held down, `C-k' asks thirty times a second, and each ask is a
photograph of the text, a set of layers and a flight to play -- for a
fade nobody can pick out of the one already running.  What the typist
notices then is the editor rather than the effect."
  (let ((effect (smear-cursor-effect (smear-cursor--effect-for 'delete))))
    (> (float-time)
       (+ smear-cursor--delete-last (or (plist-get effect :duration) 0.3)))))

(defun smear-cursor--delete-fire (beg end)
  "Flash text from BEG to END before deletion.

Run from `before-change-functions'.  Equal bounds indicate an insertion."
  (when (and smear-cursor-mode
             (>= (- end beg) (max 1 smear-cursor-delete-min-chars))
             (smear-cursor--effect-for 'delete)
             (smear-cursor--delete-again-p)
             (smear-cursor--own-edit-p beg end))
    (setq smear-cursor--delete-last (float-time))
    ;; Use a saved image for deletion.  By the frame that draws the flash,
    ;; the text is gone and the line has closed up.
    (smear-cursor--flourish
      (smear-cursor--fire-region-effect 'delete beg end t))))

(defun smear-cursor--newline-alone-p (text)
  "Return non-nil when TEXT is one newline and whatever came with it.

One newline: a return ends a line, and what a mode adds after it --
the indentation of a nested block, a list prefix, a comment leader --
comes with that one.  Two newlines is text arriving rather than a line
ending, which is a paste, and a paste has an occasion of its own.

No length anywhere in this.  A return in a deeply nested block inserts
a newline and forty spaces, which is longer than plenty of pastes, so
any limit on the size of the change turns the effect off in exactly
the code that is most indented."
  (let ((at (string-search "\n" text)))
    (and at (not (string-search "\n" text (1+ at))))))

(defun smear-cursor--newline-fire (beg end len)
  "Mark a newline inserted between BEG and END from `after-change-functions'.

LEN is what was there before, so a plain insertion has none.

Watched as a change rather than as a keystroke, because the return key
is not always a keystroke: `markdown-enter-key' and its like insert the
newline and any list prefix themselves, without going through
`self-insert-command', so `post-self-insert-hook' never runs and the
return was marked in some buffers and not others."
  (let ((name (or (smear-cursor--effect-for 'newline)
                  (smear-cursor--effect-for 'insert))))
    (when (and name smear-cursor-mode (zerop len) (> end beg)
               ;; A hook can be called for a change in a buffer that is
               ;; no longer there, or with positions that have moved on.
               (buffer-live-p (current-buffer))
               (<= (point-min) beg) (<= end (point-max))
               (not (smear-cursor--minibuffer-p))
               (not (smear-cursor--child-frame-showing-p))
               (smear-cursor--newline-alone-p
                (buffer-substring-no-properties beg end))
               (smear-cursor--own-edit-p beg end))
      (smear-cursor--flourish
        (let* ((win (selected-window))
               (r (smear-cursor--pos-rect win beg)))
          (when r
            (smear-cursor--play-effect win name (list r)
                                       smear-cursor--track-typing)))))))

(defun smear-cursor--insert-effect ()
  "Mark the character just typed from `post-self-insert-hook'.

Newlines are left to `smear-cursor--newline-fire\=', which sees them
however they arrive."
  (let ((name (smear-cursor--effect-for 'insert)))
    (when (and name smear-cursor-mode
               (not (smear-cursor--minibuffer-p))
               (not (smear-cursor--child-frame-showing-p))
               (> (point) (point-min))
               (not (eq ?\n (char-before))))
      (smear-cursor--flourish
        (let* ((win (selected-window))
               (r (smear-cursor--pos-rect win (1- (point)))))
          (when r
            (smear-cursor--play-effect win name (list r)
                                       smear-cursor--track-typing)))))))

(defun smear-cursor--set-idle (sym val)
  "Set SYM to VAL and match the idle timer to it.

Which effect plays and how long the quiet has to be are both baked
into the timer when it is armed, so storing either on its own changes
nothing until something happens to restart it."
  (set-default sym val)
  (when (bound-and-true-p smear-cursor-mode)
    (smear-cursor--idle-setup)))

(defcustom smear-cursor-idle-effect nil
  "Play this effect over and over while the cursor is left alone.

An effect name, or nil for none.  `cursor-breathe' is the quiet one.
The rest go wandering off across the text: `pacman' eats it with two
ghosts after him, `janitor' goes over it and leaves it as he found
it, `ufo' beams it up into a flying saucer, and `grinch' puts it in a
sack.  This changes how Emacs looks at rest rather than how it looks
while working, so it is off until asked for.  It needs the `x11'
backend, as every effect does."
  :type '(choice (const :tag "nothing" nil) symbol)
  :set #'smear-cursor--set-idle
  :group 'smear-cursor)

(defcustom smear-cursor-idle-delay 1.5
  "Set the seconds of quiet before the idle effect starts.

This is also the gap between one playing and the next, measured from
the end of the last, so a long animation does not run into itself."
  :type 'number
  :set #'smear-cursor--set-idle
  :group 'smear-cursor)

(defvar smear-cursor--idle-timer nil
  "Idle timer that starts the idle effect, or nil when it is off.")

(defvar smear-cursor--idle-next nil
  "Timer holding the next play, or nil when none is due.")

(defvar smear-cursor--idle-stage nil
  "The stage an idle effect is playing on, or nil when none is.")

(defun smear-cursor--idle-interrupt ()
  "Stop the idle effect as soon as anything else happens.

Run from `post-command-hook'.  Whatever plays while the cursor is left
alone has to be gone the moment it is not: Pacman covers the text he
crosses, and an animation still eating the line being typed into is
worse than no animation at all."
  (smear-cursor--roam-reset)
  (when smear-cursor--idle-stage
    (let ((stage smear-cursor--idle-stage))
      (setq smear-cursor--idle-stage nil)
      (smear-cursor--idle-drop-next)
      (when (fboundp 'smear-cursor-x11--play-stop-track)
        (ignore-errors
          (smear-cursor-x11--play-stop-track stage smear-cursor--track-idle))))))

(defcustom smear-cursor-idle-while-prompting t
  "Whether the idle effect plays while the minibuffer is active.

Reading a prompt is time spent not typing, which is the occasion an
idle effect exists for, and the prompt is what there is to roam over.
It plays on the prompt line itself.  Nothing stays eaten under what is
being typed: the first key interrupts the effect and puts the text
back."
  :type 'boolean
  :group 'smear-cursor)

(defun smear-cursor--idle-window ()
  "Return the window the idle effect should play in.

The selected one, save while the minibuffer is active: point is then
in the prompt, and the prompt is what is being read and so what there
is to roam over.  `smear-cursor--sample-window' looks behind the
prompt instead, because a jump it follows happens back there; an idle
roamer has nothing to follow and stays where the eye is.

The prompt is one line tall, so a roamer keeps to that line and leans
up over the mode line above it."
  (selected-window))

(defun smear-cursor--idle-prompting-p ()
  "Return non-nil when the idle effect would play into a prompt.

Asked of the window rather than the current buffer: the timer runs in
whatever buffer was last current, which during a read need not be the
minibuffer even though the prompt is what is on screen."
  (minibufferp (window-buffer (smear-cursor--idle-window))))

(defun smear-cursor--idle-play ()
  "Play the idle effect at the cursor, then queue the next one.

Emacs runs a repeating idle timer once for each stretch of idleness
and then waits for the next one, so the timer alone would play once
after typing stopped.  Each play queues its successor instead, and the
chain ends by itself once Emacs is no longer idle."
  (smear-cursor--idle-drop-next)
  (when (and smear-cursor-idle-effect smear-cursor-mode (current-idle-time)
             (or smear-cursor-idle-while-prompting
                 (not (smear-cursor--idle-prompting-p))))
    (smear-cursor--flourish
      ;; `window-point' rather than `point': this runs from a timer, so
      ;; the current buffer is whatever was last selected and need not
      ;; be the one on screen in WIN.
      (let* ((win (smear-cursor--idle-window))
             (r (with-current-buffer (window-buffer win)
                  (smear-cursor--pos-rect win (window-point win)))))
        (when r
          (setq smear-cursor--idle-stage (smear-cursor--effect-stage win))
          (smear-cursor--play-effect win smear-cursor-idle-effect (list r)
                                     smear-cursor--track-idle))))
    (setq smear-cursor--idle-next
          (run-at-time (smear-cursor--idle-gap) nil #'smear-cursor--idle-play))))

(defun smear-cursor--idle-gap ()
  "Return the seconds until the idle effect should play again.

Measured from the end of the one just played, so a long animation has
finished before the next begins."
  (let* ((effect (if (eq (car smear-cursor--last-effect) smear-cursor-idle-effect)
                     ;; The one just played, rather than a fresh one: a
                     ;; roaming effect walks its route on when it is
                     ;; built, and this is only asking how long it runs.
                     (cdr smear-cursor--last-effect)
                   (smear-cursor-effect smear-cursor-idle-effect)))
         (duration (or (plist-get effect :duration) 0.0)))
    (if (plist-get effect :continuous)
        ;; Taking over just before the last frame, so one wander runs
        ;; on rather than stopping and starting.
        (max 0.1 (- duration 0.12))
      (+ smear-cursor-idle-delay duration))))

(defun smear-cursor--idle-drop-next ()
  "Cancel the queued play if one is waiting."
  (when smear-cursor--idle-next
    (cancel-timer smear-cursor--idle-next)
    (setq smear-cursor--idle-next nil)))

(defun smear-cursor--idle-stop ()
  "Cancel the idle timers and stop anything still playing."
  (smear-cursor--idle-interrupt)
  (smear-cursor--idle-drop-next)
  (when smear-cursor--idle-timer
    (cancel-timer smear-cursor--idle-timer)
    (setq smear-cursor--idle-timer nil)))

(defvar smear-cursor-rest-effect)

(defvar smear-cursor--rest-timer nil
  "Timer keeping the resting glow up, or nil when there is none.")

(defvar smear-cursor--rest-at nil
  "Where the resting glow was last played, or nil when it is not up.")

(defvar smear-cursor--rest-was nil
  "What the cursor stood on last time the glow looked, or nil.

The window, point, and where the window starts.  All three are free to
read, and measuring where the cursor is on the screen is not: half a
millisecond, after every command.")

(defun smear-cursor--rest-forget ()
  "Forget where the resting glow was last played."
  (setq smear-cursor--rest-at nil
        smear-cursor--rest-was nil))

(defun smear-cursor--rest-moved-p (win)
  "Return non-nil when the cursor may have moved on the screen in WIN.

Point and the window start, which are both free: point moving is the
cursor moving, and the window start moving is the text sliding under
it, which moves the cursor on the screen without moving it in the
buffer."
  (let ((now (list win (window-point win) (window-start win))))
    (unless (equal now smear-cursor--rest-was)
      (setq smear-cursor--rest-was now))))

(defun smear-cursor--rest-wanted-p ()
  "Return non-nil when the cursor should be wearing its glow.

Not in a prompt: where the cursor is while one is up is the prompt,
which is not what the glow is for, and a prompt is where commands come
fastest -- `C-x b' would pay for aiming it again on every letter of
the buffer name."
  (and smear-cursor-mode smear-cursor-rest-effect (display-graphic-p)
       (not (minibufferp (window-buffer (selected-window))))))

(defun smear-cursor--rest-play (&optional rect)
  "Play the resting glow at the cursor, if one is asked for.

RECT is where the cursor is, when the caller has already measured it:
measuring costs half a millisecond and there is no sense in two of
them for one play."
  (when (smear-cursor--rest-wanted-p)
    (smear-cursor--flourish
      (let* ((win (selected-window))
             (rect (or rect (and (window-live-p win)
                                 (smear-cursor--point-rect win)))))
        (when rect
          (setq smear-cursor--rest-at rect)
          (smear-cursor--play-effect win smear-cursor-rest-effect (list rect)
                                     smear-cursor--track-rest))))))

(defun smear-cursor--rest-yield (win)
  "Take the resting glow down so something else can have the overlay.

The overlay is shaped, mapped, and taken down between flights, and the
taking down is what lets the next flight map it with a shape the
compositor latches on to.  A flight that never ends holds it up, so the
glow gets out of the way when a trail starts and its timer brings it
back a turn later."
  (when (and smear-cursor-mode smear-cursor-rest-effect
             (fboundp 'smear-cursor-x11--play-stop-track))
    (smear-cursor--flourish
      (let ((stage (smear-cursor--effect-stage win)))
        (when stage
          (smear-cursor-x11--play-stop-track stage smear-cursor--track-rest)
          (smear-cursor--rest-forget))))))

(defun smear-cursor--rest-follow ()
  "Aim the resting glow at the cursor when it has moved.

Run from `post-command-hook\='.  The timer alone only comes round once a
turn, and a glow a turn behind the cursor is worse than none: what the
cursor wears has to move when the cursor does."
  (when (smear-cursor--rest-wanted-p)
    (let ((win (selected-window)))
      (when (and (window-live-p win) (smear-cursor--rest-moved-p win))
        (let ((rect (ignore-errors (smear-cursor--point-rect win))))
          (when (and rect (not (equal rect smear-cursor--rest-at)))
            (smear-cursor--rest-play rect)))))))

(defun smear-cursor--rest-stop ()
  "Stop keeping the resting glow up."
  (when smear-cursor--rest-timer
    (cancel-timer smear-cursor--rest-timer)
    (setq smear-cursor--rest-timer nil)))

(defun smear-cursor--rest-setup ()
  "Match the resting glow\='s timer to `smear-cursor-rest-effect'.

One turn of the glow at a time rather than a frame at a time: the
module plays a flight of its own accord, and this only has to hand it
the next one before the last runs out."
  (smear-cursor--rest-stop)
  (smear-cursor--rest-forget)
  (when (and smear-cursor-mode smear-cursor-rest-effect)
    (let ((turn (max 0.2 (- (or (plist-get (smear-cursor-effect
                                            smear-cursor-rest-effect)
                                           :duration)
                                1.0)
                            0.1))))
      (setq smear-cursor--rest-timer
            (run-at-time 0 turn #'smear-cursor--rest-play)))))

(defun smear-cursor--set-rest (sym val)
  "Set SYM to VAL and match the resting glow to it."
  (set-default sym val)
  (when (bound-and-true-p smear-cursor-mode)
    (smear-cursor--rest-setup)))

(defcustom smear-cursor-rest-effect nil
  "Effect the cursor carries about with it, or nil for none.

If effects stop appearing while this is on, turn it off: the overlay is
taken down between flights so the next one can map it with a shape the
compositor will latch on to, and something drawn for as long as Emacs
is open is the one thing that can keep it up.  A trail takes it down
first for that reason, but the rest of the effects do not.

`cursor-rest\=' is a glow in the trail\='s own colour, so the cursor looks
like the head of a flight that has come to a stop.

It plays on the trail\='s track: a resting glow is what the trail looks
like when it is not moving, and a movement takes the track back and
gets it again when the flight ends.  Off by default, because it draws
for as long as Emacs is open and that is a cost a package should be
asked for rather than assume."
  :type '(choice (const :tag "nothing" nil) symbol)
  :set #'smear-cursor--set-rest
  :group 'smear-cursor)

(defun smear-cursor--idle-setup ()
  "Match the idle timer to `smear-cursor-idle-effect'."
  (smear-cursor--idle-stop)
  (when smear-cursor-idle-effect
    (setq smear-cursor--idle-timer
          (run-with-idle-timer (max 0.2 smear-cursor-idle-delay) t
                               #'smear-cursor--idle-play))))

;;;; Display support: face colours, cells and EOL padding

(defconst smear-cursor--face-attr-cons
  '((:background . background-color)
    (:foreground . foreground-color))
  "Map face attributes to the keys used in legacy color conses.")

(defun smear-cursor--face-attr (spec attr &optional seen)
  "Return color attribute ATTR from face specification SPEC, or nil.

ATTR is :background or :foreground.  SPEC may be a face, attribute plist,
legacy color cons, or list of these, with the first specified value taking
precedence.  Apply buffer-local face remapping; SEEN tracks remapped names
to avoid recursion.  Call in the buffer being drawn."
  (cond
   ((null spec) nil)
   ((symbolp spec)
    (let ((remap (and (not (memq spec seen))
                      (assq spec face-remapping-alist))))
      (if remap
          (smear-cursor--face-attr (cdr remap) attr (cons spec seen))
        (when (facep spec)
          (let ((v (face-attribute spec attr nil t)))
            (and (stringp v) v))))))
   ((stringp spec)
    (let ((f (intern-soft spec)))
      (and f (smear-cursor--face-attr f attr seen))))
   ((keywordp (car-safe spec))
    (let ((v (plist-get spec attr)))
      (if (and v (not (eq v 'unspecified)))
          v
        (smear-cursor--face-attr (plist-get spec :inherit) attr seen))))
   ((eq (car-safe spec) (cdr (assq attr smear-cursor--face-attr-cons)))
    (cdr spec))
   ((consp spec)
    (let ((l spec) res)
      (while (and (consp l) (not res))
        (setq res (smear-cursor--face-attr (car l) attr seen)
              l (cdr l)))
      res))))

(defun smear-cursor--face-bg (spec &optional seen)
  "Return the background color from face SPEC, or nil.

Pass previously remapped names in SEEN to `smear-cursor--face-attr'."
  (smear-cursor--face-attr spec :background seen))

(defun smear-cursor--color-rgb (color)
  "Convert COLOR to an [R G B] vector from 0 to 255, or return nil."
  (let ((v (and color (color-values color))))
    (and v (vector (ash (nth 0 v) -8) (ash (nth 1 v) -8)
                   (ash (nth 2 v) -8)))))

(defun smear-cursor--color-at (pos attr fallback)
  "Return color ATTR from the highest-priority face at POS.

Use the default face if unspecified, then FALLBACK."
  (or (smear-cursor--color-rgb
       (or (smear-cursor--face-attr (get-char-property pos 'face) attr)
           ;; Resolve remapping for `default' too, since buffers can remap it.
           (smear-cursor--face-attr 'default attr)))
      fallback))

(defun smear-cursor--bg-at (pos)
  "Return the background [R G B] of the highest-priority face at POS."
  (smear-cursor--color-at pos :background [0 0 0]))

(defun smear-cursor--pool-cell (anim w)
  "Return a pooled cell W pixels wide for ANIM, creating one if needed.

Use ANIM's glyph height so canvases do not stretch lines with extra
spacing."
  (let ((ch (smear-cursor--anim-ch anim)))
    (unless (eql smear-cursor--pool-size ch)
      (clrhash smear-cursor--pool)
      (setq smear-cursor--pool-size ch))
    (or (let ((cell (pop (gethash w smear-cursor--pool))))
          ;; A live cell can return to the pool after CH changes.  Buckets use
          ;; width alone, so check height too to avoid painting into a wrong-sized
          ;; canvas.
          (and cell
               (eql (length (smear-cursor--cell-data cell)) (* w ch))
               cell))
        (let* ((data (make-vector (* w ch) 0))
               (image (create-image
                       data 'canvas t
                       :id (gensym "smear-cursor--cell")
                       :data-width w :data-height ch
                       :scale 1 :ascent 'center)))
          (smear-cursor--cell-create
           :data data :image image :overlay nil
           :bg [0 0 0] :bgpix #xFF000000 :stamp -1 :col 0 :row 0
           :x 0 :w w)))))

;;;; Typing highlight

(defvar smear-cursor-mode)              ; defined by the minor mode below

(defvar smear-cursor--highlights nil
  "Store highlights as (OVERLAY BORN DURATION TO-RGB FROM-RGB) entries.")

(defvar smear-cursor--highlight-timer nil)

(defun smear-cursor--highlight-color ()
  "Return the typing highlight color as [R G B]."
  (or (and smear-cursor-typing-highlight-color
           (smear-cursor--color-rgb smear-cursor-typing-highlight-color))
      (smear-cursor--color-rgb (face-background 'cursor nil t))
      [200 200 200]))

(defun smear-cursor--highlight-face (from to f)
  "Return a background face blending FROM toward TO by fraction F."
  (list :background
        (format "#%02x%02x%02x"
                (round (+ (aref from 0) (* f (- (aref to 0) (aref from 0)))))
                (round (+ (aref from 1) (* f (- (aref to 1) (aref from 1)))))
                (round (+ (aref from 2) (* f (- (aref to 2) (aref from 2))))))))

(defun smear-cursor--highlight (beg end &optional ghost)
  "Highlight text from BEG to END and start fading it.

With GHOST, equal bounds display a highlighted space for deleted text."
  (when (and (or ghost (< beg end)) (<= (point-min) beg) (<= end (point-max)))
    ;; Sample before creating the overlay or its own face prevents the
    ;; highlight from fading back to the background.
    (let ((from (smear-cursor--bg-at beg))
          (ov (make-overlay beg end nil t nil)))
      (overlay-put ov 'smear-cursor-highlight t)
      (overlay-put ov 'priority 100)
      (push (list ov (float-time) smear-cursor-typing-highlight-duration
                  (smear-cursor--highlight-color) from ghost)
            smear-cursor--highlights)
      (smear-cursor--fade-highlights)
      (unless smear-cursor--highlight-timer
        (setq smear-cursor--highlight-timer
              (run-at-time 0 (/ 1.0 (max 1 smear-cursor-typing-highlight-fps))
                           #'smear-cursor--fade-highlights))))))

(defun smear-cursor--fade-highlights ()
  "Update all highlights and remove those that have finished fading."
  (let ((now (float-time))
        (live nil))
    (dolist (h smear-cursor--highlights)
      (let* ((ov (nth 0 h))
             (age (- now (nth 1 h)))
             (dur (max 0.01 (nth 2 h))))
        (if (or (null (overlay-buffer ov)) (>= age dur))
            (delete-overlay ov)
          (let ((face (smear-cursor--highlight-face
                       (nth 4 h) (nth 3 h)
                       (* smear-cursor-typing-highlight-strength
                          (- 1.0 (/ age dur))))))
            (if (nth 5 h)
                (overlay-put ov 'after-string (propertize " " 'face face))
              (overlay-put ov 'face face)))
          (push h live))))
    (setq smear-cursor--highlights live)
    (unless live
      (when smear-cursor--highlight-timer
        (cancel-timer smear-cursor--highlight-timer)
        (setq smear-cursor--highlight-timer nil)))))

(defun smear-cursor--clear-highlights ()
  "Remove all highlights and stop their timer."
  (dolist (h smear-cursor--highlights) (delete-overlay (nth 0 h)))
  (setq smear-cursor--highlights nil)
  (when smear-cursor--highlight-timer
    (cancel-timer smear-cursor--highlight-timer)
    (setq smear-cursor--highlight-timer nil)))

(defun smear-cursor--highlight-change (beg end len)
  "Highlight an edit from BEG to END with LEN characters replaced.

Highlight inserted text or flash the position of a small deletion."
  (when (and smear-cursor-typing-highlight
             smear-cursor-mode
             (smear-cursor--own-edit-p beg end))
    (let ((ins (- end beg))
          (cap (max 1 smear-cursor-typing-highlight-max-change)))
      (cond
       ((and (> ins 0) (<= ins cap))
        (smear-cursor--highlight beg end))
       ;; Highlight a nearby character after deletion.  At EOL, highlighting
       ;; the newline produces no visible flash.
       ((and (= ins 0) (> len 0) (<= len cap))
        (let ((after (char-after beg))
              (before (char-before beg)))
          (cond
           ((and after (not (eq after ?\n)))
            (smear-cursor--highlight beg (1+ beg)))
           ((and before (not (eq before ?\n)))
            (smear-cursor--highlight (1- beg) beg))
           ;; An empty line needs a space to display the highlight.
           (t (smear-cursor--highlight beg beg t)))))))))

;;;; Proportional rows: measured glyph boxes

(defconst smear-cursor--gmap-cap 96
  "Limit glyph measurements per proportional row per animation.

Stop painting past the limit to bound layout query cost.")

(cl-defstruct (smear-cursor--gmap (:constructor smear-cursor--gmap-create))
  "Glyph boxes for one screen row whose text is off the character grid.
A proportional face, such as org headings or prose under
`variable-pitch', puts glyphs at arbitrary x.  The arithmetic that
turns a pixel into a grid cell does not hold there, so each glyph's box
has to be measured.  Boxes are measured outward from the first pixel asked for,
and never renumbered, so cells stay keyed by entry index."
  (xs (make-vector smear-cursor--gmap-cap 0))      ; left edge, pixels
  (ws (make-vector smear-cursor--gmap-cap 0))      ; advance, pixels
  (starts (make-vector smear-cursor--gmap-cap 0))  ; first buffer position
  (ends (make-vector smear-cursor--gmap-cap 0))    ; end buffer position
  (n 0)
  bol eol      ; the row's line bounds
  row-y row-y1 ; Pixel y of this row and the next.
  mid-y        ; pixel y of its middle, where the row is probed
  lo lo-x      ; leftmost measured glyph: start position and left edge
  hi hi-x)     ; first position not yet measured, and its left edge

(defun smear-cursor--gmap-new (anim row bol eol)
  "Create an empty glyph map for ANIM's ROW with bounds BOL and EOL."
  (let* ((lh (smear-cursor--anim-lh anim))
         (y (+ (smear-cursor--anim-y-base anim) (* row lh))))
    (smear-cursor--gmap-create :bol bol :eol eol
                               :row-y y :row-y1 (+ y lh)
                               :mid-y (+ y (/ lh 2)))))

(defun smear-cursor--glyph-x (gmap win pos)
  "Return the left pixel x of POS in WIN if it lies on GMAP's row.

Return nil otherwise."
  (let ((xy (pos-visible-in-window-p pos win t)))
    (and xy
         (>= (cadr xy) (smear-cursor--gmap-row-y gmap))
         (< (cadr xy) (smear-cursor--gmap-row-y1 gmap))
         (car xy))))

(defun smear-cursor--gmap-add (gmap x w start end)
  "Append a glyph at X with width W over START to END in GMAP.

END is exclusive.  Return its index, or nil if the map is full."
  (let ((i (smear-cursor--gmap-n gmap)))
    (when (< i smear-cursor--gmap-cap)
      (aset (smear-cursor--gmap-xs gmap) i x)
      (aset (smear-cursor--gmap-ws gmap) i w)
      (aset (smear-cursor--gmap-starts gmap) i start)
      (aset (smear-cursor--gmap-ends gmap) i end)
      (setf (smear-cursor--gmap-n gmap) (1+ i))
      i)))

(defun smear-cursor--gmap-next-right (gmap win start x eol)
  "Find the first position after START displayed right of X in WIN.

Search GMAP's row up to EOL and return (POS . POS-X), or nil.  Use
doubling and bisection for characters sharing a glyph; glyph x coordinates
do not decrease along a row."
  (let ((lo start) (hi nil) (hi-x nil) (dead nil)
        (p (1+ start)) (step 1))
    (while (and (null hi) (not dead) (<= p eol))
      (let ((px (smear-cursor--glyph-x gmap win p)))
        (cond
         ((null px) (setq dead t))      ; No visible position on this row.
         ((> px x) (setq hi p hi-x px))
         ((>= p eol) (setq dead t))
         (t (setq lo p
                  p (min eol (+ p step))
                  step (* 2 step))))))
    (while (and hi (not dead) (> (- hi lo) 1))
      (let* ((mid (+ lo (/ (- hi lo) 2)))
             (px (smear-cursor--glyph-x gmap win mid)))
        (cond ((null px) (setq dead t))
              ((> px x) (setq hi mid hi-x px))
              (t (setq lo mid)))))
    (and hi (not dead) (cons hi hi-x))))

(defun smear-cursor--gmap-glyph-start (gmap win pos px bol)
  "Find the first glyph position at PX between BOL and POS in WIN.

Search GMAP's row by bisection; POS must display at PX.  Return nil if
glyph x coordinates are not monotonic."
  (if (eql (smear-cursor--glyph-x gmap win bol) px)
      bol
    (let ((lo bol) (hi pos) (dead nil))
      (while (and (not dead) (> (- hi lo) 1))
        (let* ((mid (+ lo (/ (- hi lo) 2)))
               (mx (smear-cursor--glyph-x gmap win mid)))
          (cond ((null mx) (setq dead t))
                ((eql mx px) (setq hi mid))
                (t (setq lo mid)))))
      (and (not dead) hi))))

(defun smear-cursor--gmap-grow-right (gmap win)
  "Measure GMAP's next glyph to the right in WIN.

Return non-nil if a glyph was added."
  (let ((start (smear-cursor--gmap-hi gmap))
        (x (smear-cursor--gmap-hi-x gmap))
        (eol (smear-cursor--gmap-eol gmap)))
    (when (and start x (< start eol))
      (let ((next (smear-cursor--gmap-next-right gmap win start x eol)))
        (cond
         ;; Stop searching at the last or clipped glyph so later frames do not
         ;; repeat the failed lookup.
         ((null next) (setf (smear-cursor--gmap-hi gmap) nil) nil)
         ((smear-cursor--gmap-add gmap x (- (cdr next) x) start (car next))
          (setf (smear-cursor--gmap-hi gmap) (car next)
                (smear-cursor--gmap-hi-x gmap) (cdr next))
          t))))))

(defun smear-cursor--gmap-grow-left (gmap win)
  "Measure GMAP's next glyph to the left in WIN.

Return non-nil if a glyph was added."
  (let ((end (smear-cursor--gmap-lo gmap))
        (x (smear-cursor--gmap-lo-x gmap))
        (bol (smear-cursor--gmap-bol gmap)))
    (when (and end x (> end bol))
      (let* ((prev (1- end))
             (px (smear-cursor--glyph-x gmap win prev))
             (p (and px (< px x)
                     (smear-cursor--gmap-glyph-start gmap win prev px bol))))
        (cond
         ((null p) (setf (smear-cursor--gmap-lo gmap) nil) nil)
         ((smear-cursor--gmap-add gmap px (- x px) p end)
          (setf (smear-cursor--gmap-lo gmap) p
                (smear-cursor--gmap-lo-x gmap) px)
          t))))))

(defun smear-cursor--gmap-seed (gmap win ax)
  "Initialize GMAP with the glyph under pixel AX in WIN."
  (let* ((pn (posn-at-x-y (max 0 ax) (smear-cursor--gmap-mid-y gmap) win))
         (pos (and pn (null (posn-area pn)) (posn-point pn)))
         (bol (smear-cursor--gmap-bol gmap))
         (x (and pos (>= pos bol) (smear-cursor--glyph-x gmap win pos)))
         (start (and x (smear-cursor--gmap-glyph-start gmap win pos x bol))))
    (when start
      (setf (smear-cursor--gmap-lo gmap) start
            (smear-cursor--gmap-lo-x gmap) x
            (smear-cursor--gmap-hi gmap) start
            (smear-cursor--gmap-hi-x gmap) x)
      (smear-cursor--gmap-grow-right gmap win))))

(defun smear-cursor--gmap-index (gmap win ax)
  "Return the index in GMAP of the glyph covering pixel AX in WIN.

Measure more glyphs as needed.  Return nil past the text, at the glyph
limit, or when the current frame's measurement time runs out."
  (when (and (zerop (smear-cursor--gmap-n gmap))
             (smear-cursor--may-measure-p))
    (smear-cursor--gmap-seed gmap win ax))
  (when (> (smear-cursor--gmap-n gmap) 0)
    (let ((grew t))
      ;; Check the budget last so completed maps do not count as deferred work.
      (while (and grew
                  (< (smear-cursor--gmap-n gmap) smear-cursor--gmap-cap)
                  (or (< ax (smear-cursor--gmap-lo-x gmap))
                      (>= ax (smear-cursor--gmap-hi-x gmap)))
                  (smear-cursor--may-measure-p))
        (setq grew (if (< ax (smear-cursor--gmap-lo-x gmap))
                       (smear-cursor--gmap-grow-left gmap win)
                     (smear-cursor--gmap-grow-right gmap win)))))
    (let ((xs (smear-cursor--gmap-xs gmap))
          (ws (smear-cursor--gmap-ws gmap))
          (n (smear-cursor--gmap-n gmap))
          (i 0) (hit nil))
      (while (and (null hit) (< i n))
        (when (and (>= ax (aref xs i)) (< ax (+ (aref xs i) (aref ws i))))
          (setq hit i))
        (setq i (1+ i)))
      hit)))

(defun smear-cursor--claim-real-cell (anim idx row x w start end)
  "Assign ANIM a cell W pixels wide at X over text from START to END.

IDX is its index in ROW.  END is exclusive; several characters may share
one glyph."
  (let* ((cell (smear-cursor--pool-cell anim w))
         (ov (smear-cursor--cell-overlay cell))
         (bg (smear-cursor--bg-at start)))
    (if ov
        (move-overlay ov start end (smear-cursor--anim-buffer anim))
      (setq ov (make-overlay start end))
      (setf (smear-cursor--cell-overlay cell) ov))
    (overlay-put ov 'display (smear-cursor--cell-image cell))
    (overlay-put ov 'window (smear-cursor--anim-window anim))
    (overlay-put ov 'priority 1000)
    (overlay-put ov 'smear-cursor t)
    (setf (smear-cursor--cell-bg cell) bg
          (smear-cursor--cell-bgpix cell) (smear-cursor--blend bg bg 0.0)
          (smear-cursor--cell-stamp cell) -1
          (smear-cursor--cell-col cell) idx
          (smear-cursor--cell-row cell) row
          (smear-cursor--cell-x cell) x)
    cell))

(defun smear-cursor--ensure-pad (anim row eol eol-x last-col)
  "Extend ANIM's padding for ROW through LAST-COL.

EOL is the line-end position and EOL-X its pixel x coordinate.  Register
each padding cell in ANIM's cell table."
  (let* ((cw (smear-cursor--anim-cw anim))
         ;; Clamp to visible columns.  Horizontal scrolling can put the line end
         ;; at negative x; padding from that column creates dozens of cells off
         ;; the left edge of the window.
         (first-col (max 0 (/ eol-x cw)))
         (win-w (smear-cursor--anim-win-w anim))
         (last-col (if win-w (min last-col (/ win-w cw)) last-col))
         (ncols (1+ (- last-col first-col)))
         (pads (smear-cursor--anim-pads anim))
         (pad (gethash row pads))
         (have (or (car-safe pad) 0)))
    (when (and (> ncols 0) (> ncols have))
      (let ((str (make-string ncols ?\s))
            ;; Sample at EOL to match highlight backgrounds that extend past the text.
            (bg (smear-cursor--bg-at eol)))
        (dotimes (j ncols)
          (let* ((key (smear-cursor--cell-key (+ first-col j) row))
                 (cell (let ((hit (gethash key
                                           (smear-cursor--anim-cells anim))))
                         (if (smear-cursor--cell-p hit) hit
                           (let ((c (smear-cursor--pool-cell anim cw)))
                             (when (smear-cursor--cell-overlay c)
                               (delete-overlay
                                (smear-cursor--cell-overlay c))
                               (setf (smear-cursor--cell-overlay c) nil))
                             (setf (smear-cursor--cell-bg c) bg
                                   (smear-cursor--cell-bgpix c)
                                   (smear-cursor--blend bg bg 0.0)
                                   (smear-cursor--cell-stamp c) -1
                                   (smear-cursor--cell-col c) (+ first-col j)
                                   (smear-cursor--cell-row c) row
                                   (smear-cursor--cell-x c)
                                   (* (+ first-col j) cw))
                             (puthash key c (smear-cursor--anim-cells anim))
                             c)))))
            (put-text-property j (1+ j) 'display
                               (smear-cursor--cell-image cell) str)))
        (let ((ov (or (cdr-safe pad)
                      ;; Advance both markers so typing at EOL stays before the padding.
                      ;; Otherwise the next character appears after a run of blanks.
                      ;; Advancing only the front of an empty overlay would put its start
                      ;; past its end, preventing it from moving.
                      (make-overlay eol eol
                                    (smear-cursor--anim-buffer anim)
                                    t t))))
          (overlay-put ov 'after-string str)
          (overlay-put ov 'window (smear-cursor--anim-window anim))
          (overlay-put ov 'smear-cursor t)
          (puthash row (cons ncols ov) pads))))))

(defun smear-cursor--row-simple-p (bol end)
  "Return non-nil if text from BOL to END lies on the character grid.

Require printable ASCII without replacement display, composition, or
hidden text.  Check properties by runs."
  (and (not (string-match-p "[^\x20-\x7e]"
                            (buffer-substring-no-properties bol end)))
       (let ((p bol) (ok t))
         (while (and ok (< p end))
           (if (or (get-char-property p 'display)
                   (get-char-property p 'composition)
                   (invisible-p p))
               (setq ok nil)
             (setq p (next-char-property-change p end))))
         ok)))

(defun smear-cursor--row-unwrapped-p (anim info)
  "Return non-nil if ANIM's row described by INFO cannot wrap.

INFO is (BOL EOL BOL-X).  Require a line without prefixes that fits the
available width."
  (let ((bol (nth 0 info)) (eol (nth 1 info)) (bol-x (nth 2 info))
        (win (smear-cursor--anim-window anim)))
    (and (or (null win) (zerop (window-hscroll win)))
         (<= (+ bol-x (* (- eol bol) (smear-cursor--anim-cw anim)))
             (smear-cursor--anim-win-w anim))
         (with-current-buffer (smear-cursor--anim-buffer anim)
           (and (null (get-char-property bol 'line-prefix))
                (null (get-char-property bol 'wrap-prefix)))))))

(defun smear-cursor--grid-holds-p (anim win bol end bol-x cw)
  "Return non-nil if ANIM's text in WIN follows the character grid.

Compare positions BOL through END using starting pixel BOL-X and cell
width CW.  Cache the result for the animation."
  (let ((known (smear-cursor--anim-grid anim)))
    (cond
     ((eq known 'yes) t)
     ((eq known 'no) nil)
     (t (let* ((xy (pos-visible-in-window-p end win t))
               (ok (and xy (car xy) (= (car xy) (+ bol-x (* (- end bol) cw))))))
          (setf (smear-cursor--anim-grid anim) (if ok 'yes 'no))
          (and ok t))))))

(defun smear-cursor--scan-row (anim row bol bol-x)
  "Classify ANIM's screen ROW starting at BOL and pixel BOL-X.

Return (BOL EOL BOL-X) for a simple grid row, a glyph map for measured
text, or `bad' for an unsupported row."
  (let ((win (smear-cursor--anim-window anim))
        (lh (smear-cursor--anim-lh anim))
        (cw (smear-cursor--anim-cw anim)))
    (if (/= (with-selected-window win
              (save-excursion (goto-char bol) (line-pixel-height)))
            lh)
        'bad
      (let* ((eol (save-excursion (goto-char bol) (line-end-position)))
             ;; Scan only visible columns.  Scanning a whole minified JSON or log
             ;; line costs milliseconds per row per animation for text outside the window.
             (scan-end
              (min eol (+ bol 1 (/ (max 0 (- (smear-cursor--anim-win-w anim)
                                             bol-x))
                                   cw))))
             ;; Measure glyphs when widths may differ from one cell: non-ASCII text,
             ;; display properties, compositions and hidden text such as org link URLs.
             (simple (smear-cursor--row-simple-p bol scan-end)))
        (cond
         ((not simple) (smear-cursor--gmap-new anim row bol eol))
         ;; Face remapping and text scaling can change glyph widths.  Check a
         ;; measured glyph position before using the frame's character grid.
         ((and face-remapping-alist
               (not (smear-cursor--grid-holds-p anim win bol scan-end
                                                bol-x cw)))
          (smear-cursor--gmap-new anim row bol eol))
         (t (list bol eol bol-x)))))))

(defun smear-cursor--probe-row (anim row)
  "Resolve ANIM's ROW layout with a fresh `posn-at-x-y' probe.

This handles folds and wrapped lines above ROW and sets an anchor if
needed."
  (let* ((win (smear-cursor--anim-window anim))
         (lh (smear-cursor--anim-lh anim))
         (y-base (smear-cursor--anim-y-base anim))
         (pn (posn-at-x-y 1 (+ y-base (* row lh) (/ lh 2)) win))
         (pos (and pn (null (posn-area pn)) (posn-point pn))))
    (if (null pos)
        'bad
      (with-current-buffer (smear-cursor--anim-buffer anim)
        (let* ((bol (save-excursion (goto-char pos)
                                    (line-beginning-position)))
               (xy (pos-visible-in-window-p bol win t)))
          (if (or (null xy)
                  (not (eql (smear-cursor--row-of-y anim (cadr xy)) row)))
              'bad
            (unless (smear-cursor--anim-anchor anim)
              (setf (smear-cursor--anim-anchor anim) (cons row bol)))
            (smear-cursor--scan-row anim row bol (car xy))))))))

(defun smear-cursor--walk-adjacent (anim row)
  "Derive ANIM's ROW layout from a neighboring unwrapped row, or return nil.

This uses no display query.  Check every row before using it for the next
step."
  (let* ((rows (smear-cursor--anim-rows anim))
         (near (cond ((consp (gethash (1- row) rows)) (1- row))
                     ((consp (gethash (1+ row) rows)) (1+ row)))))
    (when near
      (let ((info (gethash near rows)))
        (when (smear-cursor--row-unwrapped-p anim info)
          (with-current-buffer (smear-cursor--anim-buffer anim)
            (let ((bol (save-excursion
                         (goto-char (nth 0 info))
                         (and (zerop (forward-line (- row near)))
                              (not (eobp))
                              (point)))))
              (when bol
                (smear-cursor--scan-row anim row bol (nth 2 info))))))))))

(defun smear-cursor--walk-row (anim row)
  "Resolve ANIM's ROW by walking logical lines from the nearest known row.

Return nil if the resulting screen y does not match ROW, so a display
probe can be used instead."
  (let* ((win (smear-cursor--anim-window anim))
         (rows (smear-cursor--anim-rows anim))
         (anchor (smear-cursor--anim-anchor anim))
         (from-row (car anchor))
         (from-bol (cdr anchor)))
    (maphash (lambda (r info)
               (when (and (consp info)
                          (< (abs (- row r)) (abs (- row from-row))))
                 (setq from-row r from-bol (car info))))
             rows)
    (with-current-buffer (smear-cursor--anim-buffer anim)
      (let ((bol (save-excursion
                   (goto-char from-bol)
                   (and (zerop (forward-line (- row from-row)))
                        (point)))))
        (when bol
          (let ((xy (pos-visible-in-window-p bol win t)))
            (when (and xy (eql (smear-cursor--row-of-y anim (cadr xy)) row))
              (smear-cursor--scan-row anim row bol (car xy)))))))))

(defun smear-cursor--row-of-y (anim y)
  "Return the screen row of pixel Y on ANIM's grid.

Return nil above the first row.  Each row covers a band of pixels."
  (let ((dy (- y (smear-cursor--anim-y-base anim))))
    (and (>= dy 0) (/ dy (smear-cursor--anim-lh anim)))))

(defun smear-cursor--row-info (anim row)
  "Return cached layout information for ANIM's screen ROW.

Return (BOL EOL BOL-X), a glyph map, or `bad'.  Try walking logical lines
before probing the display."
  (let ((rows (smear-cursor--anim-rows anim)))
    (or (gethash row rows)
        (puthash
         row
         (or (smear-cursor--walk-adjacent anim row)
             (and (smear-cursor--anim-anchor anim)
                  (smear-cursor--walk-row anim row))
             (smear-cursor--probe-row anim row))
         rows))))

(defun smear-cursor--resolve-cell (anim idx row &optional no-pad)
  "Assign ANIM a canvas cell for target IDX on ROW, or return `skip'.

IDX is a column on grid rows or a measured glyph index on proportional
rows.  Non-nil NO-PAD prevents padding past line ends."
  (let ((info (smear-cursor--row-info anim row))
        (cw (smear-cursor--anim-cw anim)))
    (cond
     ((eq info 'bad) 'skip)
     ((smear-cursor--gmap-p info)
      (if (>= idx (smear-cursor--gmap-n info))
          'skip
        (let ((x (aref (smear-cursor--gmap-xs info) idx))
              (w (aref (smear-cursor--gmap-ws info) idx))
              (start (aref (smear-cursor--gmap-starts info) idx))
              (end (aref (smear-cursor--gmap-ends info) idx)))
          (if (or (< w 1) (>= start end))
              'skip
            (with-current-buffer (smear-cursor--anim-buffer anim)
              (smear-cursor--claim-real-cell anim idx row x w start end))))))
     (t
      (let ((bol (nth 0 info)) (eol (nth 1 info)) (bol-x (nth 2 info))
            (cx (* idx cw)))
        (with-current-buffer (smear-cursor--anim-buffer anim)
          (cond
           ((or (< cx bol-x) (/= 0 (% (- cx bol-x) cw)))
            'skip)
           ((< cx (+ bol-x (* (- eol bol) cw)))
            (let ((pos (+ bol (/ (- cx bol-x) cw))))
              (smear-cursor--claim-real-cell anim idx row cx cw pos (1+ pos))))
           (no-pad 'skip)               ; At or past EOL; padding disabled.
           (t
            (let ((eol-x (+ bol-x (* (- eol bol) cw))))
              (smear-cursor--ensure-pad anim row eol eol-x idx)
              (let ((hit (gethash (smear-cursor--cell-key idx row)
                                  (smear-cursor--anim-cells anim))))
                (if (smear-cursor--cell-p hit) hit 'skip)))))))))))

(defun smear-cursor--target-at (anim ax row &optional no-pad)
  "Return ANIM's cell at pixel column AX on ROW, or `skip'.

Pass NO-PAD to `smear-cursor--resolve-cell'.  Use grid arithmetic or
measure the proportional row as needed."
  (let* ((rows (smear-cursor--anim-rows anim))
         ;; Row layout queries share the measurement budget.  Do not cache a
         ;; budget timeout or the row stays unresolved for the whole animation.
         (info (or (gethash row rows)
                   (and (smear-cursor--may-measure-p)
                        (smear-cursor--row-info anim row)))))
    (cond
     ((memq info '(nil bad)) 'skip)
     ((smear-cursor--gmap-p info)
      (let ((i (smear-cursor--gmap-index
                info (smear-cursor--anim-window anim) ax)))
        (if i (smear-cursor--cell-at anim i row no-pad) 'skip)))
     (t (smear-cursor--cell-at anim (/ ax (smear-cursor--anim-cw anim))
                               row no-pad)))))

;;;; Engine

(defvar smear-cursor--anim nil
  "Store the current cursor animation, or nil.")

(defun smear-cursor--hide-cursor (buffer)
  "Hide BUFFER's cursor through buffer-local `cursor-type'.

Return a token for `smear-cursor--restore-cursor'."
  (with-current-buffer buffer
    ;; Do not save the cursor state when already hidden by another effect.
    ;; A deletion can start a smear; saving nil on the second hide would
    ;; keep the cursor hidden after both effects stop.  Return a nil token
    ;; so restoration leaves the saved state alone.
    (unless (null cursor-type)
      (prog1 (if (local-variable-p 'cursor-type)
                 (cons 'local cursor-type)
               'global)
        (setq-local cursor-type nil)))))

(defun smear-cursor--ensure-hidden (anim)
  "Hide the real cursor once ANIM has drawn something on screen."
  (unless (smear-cursor--anim-cursor-saved anim)
    (setf (smear-cursor--anim-cursor-saved anim)
          (smear-cursor--hide-cursor (smear-cursor--anim-buffer anim)))))

(defun smear-cursor--restore-cursor (buffer token)
  "Restore BUFFER's cursor type from TOKEN.

A nil TOKEN leaves the cursor unchanged."
  (when (and token (buffer-live-p buffer))
    (with-current-buffer buffer
      (if (eq token 'global)
          (kill-local-variable 'cursor-type)
        (setq-local cursor-type (cdr token))))))

(defvar smear-cursor-mode)          ; defined by the minor mode below

(defun smear-cursor--revalidate (anim)
  "Clear ANIM's layout and cell caches after text changes or scrolling."
  (let ((now (with-current-buffer (smear-cursor--anim-buffer anim)
               (buffer-chars-modified-tick)))
        (start (let ((win (smear-cursor--anim-window anim)))
                 (and (window-live-p win) (window-start win)))))
    (unless (eql now (smear-cursor--anim-tick anim))
      (smear-cursor--flush-cells anim)
      (setf (smear-cursor--anim-tick anim) now))
    (unless (eql start (smear-cursor--anim-window-start anim))
      (smear-cursor--flush-cells anim)
      (setf (smear-cursor--anim-window-start anim) start))))

(defun smear-cursor--context-valid-p (anim)
  "Return non-nil while ANIM's window still displays its buffer."
  (let ((win (smear-cursor--anim-window anim)))
    (and smear-cursor-mode
         (window-live-p win)
         (buffer-live-p (smear-cursor--anim-buffer anim))
         (eq (window-buffer win) (smear-cursor--anim-buffer anim)))))

(defun smear-cursor--cells-release (anim)
  "Remove ANIM's cells and line-end padding from the screen."
  (maphash (lambda (_k cell)
             (when (smear-cursor--cell-p cell)
               (smear-cursor--release-cell anim cell)))
           (smear-cursor--anim-cells anim))
  (clrhash (smear-cursor--anim-cells anim))
  (maphash (lambda (_row pad)
             (when (overlayp (cdr-safe pad))
               (delete-overlay (cdr pad))))
           (smear-cursor--anim-pads anim))
  (clrhash (smear-cursor--anim-pads anim)))

(defun smear-cursor--without-pads (anim fn)
  "Call FN while ANIM's line-end padding has no display width.

Restore the padding afterward, including if FN exits with an error."
  (let ((saved nil))
    (unwind-protect
        (progn
          (maphash (lambda (_row pad)
                     (let ((ov (cdr-safe pad)))
                       (when (and (overlayp ov)
                                  (overlay-get ov 'after-string))
                         (push (cons ov (overlay-get ov 'after-string)) saved)
                         (overlay-put ov 'after-string nil))))
                   (smear-cursor--anim-pads anim))
          (funcall fn))
      (dolist (e saved) (overlay-put (car e) 'after-string (cdr e))))))

(defun smear-cursor--flush-cells (anim)
  "Release ANIM's cells and padding and clear its row cache."
  (smear-cursor--cells-release anim)
  (clrhash (smear-cursor--anim-rows anim))
  (setf (smear-cursor--anim-anchor anim) nil))

(defun smear-cursor--own-stats (anim)
  "Return ANIM's Lisp timing data, or nil if no frame was drawn."
  (when (> (smear-cursor--anim-paint-frames anim) 0)
    (list :frames (smear-cursor--anim-paint-frames anim)
          :paint-total (or (smear-cursor--anim-paint-total anim) 0.0)
          :gap-max (smear-cursor--anim-gap-max anim)
          :gap-max-at (smear-cursor--anim-gap-max-at anim)
          :gap-sum (smear-cursor--anim-gap-sum anim)
          :gaps (smear-cursor--anim-gaps anim))))

(defun smear-cursor--record-stats (anim renderer played)
  "Save ANIM's timing data and RENDERER for `smear-cursor-report'.

Prefer the player thread's data in PLAYED when available."
  ;; Use PLAYED only if this animation started playback.  The thread
  ;; retains its last result; reusing it for an unplayed animation can
  ;; report misleading figures such as "17 frames in 2 ms".
  (let ((base (or (and (> (smear-cursor--anim-paint-frames anim) 0) played)
                  (smear-cursor--own-stats anim))))
    (when base
      (setq smear-cursor--last-stats
            (append
             base
             (list :gc (- gc-elapsed (or (smear-cursor--anim-gc0 anim) 0.0))
                   :seconds (- (float-time)
                               (or (smear-cursor--anim-born anim)
                                   (float-time)))
                   :style smear-cursor-trail-style
                   :threaded (and played t)
                   ;; Timestamp the report to distinguish a failed animation from the last
                   ;; successful one.
                   :at (float-time)
                   :renderer renderer))))))

(defun smear-cursor--stop ()
  "Stop and remove the current animation.

Repeated calls are safe, and this can be called from any hook."
  (let ((anim smear-cursor--anim))
    (setq smear-cursor--anim nil)
    (when anim
      ;; Read the renderer name before removing the stage.  Read playback
      ;; statistics after stopping, when the final counts are available.
      (let ((renderer (smear-cursor--x11-renderer-name anim))
            (stage (smear-cursor--anim-x11 anim)))
        (smear-cursor--x11-down anim)
        (smear-cursor--record-stats anim renderer
                                    (smear-cursor--play-stats stage)))
      (dolist (timer (list (smear-cursor--anim-timer anim)
                           (smear-cursor--anim-end-timer anim)))
        (when (timerp timer) (cancel-timer timer)))
      (maphash (lambda (_k cell)
                 (when (smear-cursor--cell-p cell)
                   (smear-cursor--release-cell anim cell)))
               (smear-cursor--anim-cells anim))
      (clrhash (smear-cursor--anim-cells anim))
      (maphash (lambda (_row pad)
                 (when (overlayp (cdr-safe pad))
                   (delete-overlay (cdr pad))))
               (smear-cursor--anim-pads anim))
      (clrhash (smear-cursor--anim-pads anim))
      (when (smear-cursor--anim-cursor-saved anim)
        (smear-cursor--restore-cursor (smear-cursor--anim-buffer anim)
                                      (smear-cursor--anim-cursor-saved anim))))))

(defun smear-cursor--tick ()
  "Advance and draw one animation frame, stopping when finished.

On error, remove the animation and report the error once."
  (condition-case err
      (smear-cursor--tick-1)
    (error (smear-cursor--stop)
           (message "smear-cursor: animation aborted: %S" err))))

(defconst smear-cursor--dt-max 1.5
  "Limit each spring step's movement in frame units.")

(defun smear-cursor--dt (gap)
  "Convert GAP seconds to movement in frame units.

Return at least half a frame and at most `smear-cursor--dt-max'."
  (min smear-cursor--dt-max (max 0.5 (* 60.0 gap))))

(defun smear-cursor--over-frame-budget-p (cost gc)
  "Return non-nil if COST seconds exceeds the budget after subtracting GC.

GC is time spent collecting garbage.  One slow frame does not stop an
animation; see `smear-cursor--tick-1'."
  (and smear-cursor-frame-budget
       (> (- cost gc) smear-cursor-frame-budget)))

(defconst smear-cursor--gaps-kept 256
  "Limit the number of frame gaps retained for the timing median.")

(defun smear-cursor--median (gaps)
  "Return the median of GAPS, or 0.0 if empty."
  (let* ((v (sort (copy-sequence gaps) #'<))
         (n (length v)))
    (cond ((= n 0) 0.0)
          ((cl-oddp n) (nth (/ n 2) v))
          (t (/ (+ (nth (1- (/ n 2)) v) (nth (/ n 2) v)) 2.0)))))

(defun smear-cursor--late-frames (gaps interval)
  "Count values in GAPS that exceed one and a half times INTERVAL."
  (cl-count-if (lambda (g) (> g (* 1.5 interval))) gaps))

(defun smear-cursor--note-gap (anim gap)
  "Record GAP seconds between ANIM's frames.

Also record the worst gap and its frame number."
  (cl-incf (smear-cursor--anim-gap-frames anim))
  (cl-incf (smear-cursor--anim-gap-sum anim) gap)
  (when (< (smear-cursor--anim-gap-frames anim) smear-cursor--gaps-kept)
    (push gap (smear-cursor--anim-gaps anim)))
  (when (> gap (smear-cursor--anim-gap-max anim))
    (setf (smear-cursor--anim-gap-max anim) gap
          (smear-cursor--anim-gap-max-at anim)
          (smear-cursor--anim-gap-frames anim))))

(defun smear-cursor--tick-1 ()
  "Run the animation frame work for `smear-cursor--tick'."
  (let ((anim smear-cursor--anim))
    (when anim
      (if (not (smear-cursor--context-valid-p anim))
          (smear-cursor--stop)
        (smear-cursor--revalidate anim)
        (let* ((now (float-time))
               (gap (- now (smear-cursor--anim-last-time anim)))
               (dt (smear-cursor--dt gap))
               (cap (smear-cursor--max-length-px
                     nil (smear-cursor--anim-cw anim)))
               (t0 now)
               (gc0 gc-elapsed))
          (smear-cursor--note-gap anim gap)
          (setf (smear-cursor--anim-last-time anim) now)
          (let ((dist (smear-cursor--ease-step
                       (smear-cursor--anim-corners anim)
                       (smear-cursor--anim-velocities anim)
                       (smear-cursor--anim-target anim)
                       (smear-cursor--spring :head smear-cursor-stiffness-head)
                       (smear-cursor--spring :tail smear-cursor-stiffness-tail)
                       (smear-cursor--spring :damping smear-cursor-damping)
                       (smear-cursor--spring
                        :exponent smear-cursor-trailing-exponent)
                       dt
                       (smear-cursor--overshoot-cap
                        (smear-cursor--anim-cw anim)
                        (smear-cursor--anim-lh anim)))))
            (when cap
              (smear-cursor--shorten-corners
               (smear-cursor--anim-corners anim)
               (smear-cursor--anim-target anim) cap))
            (if (or (< dist smear-cursor--eps)
                    ;; Limit duration because a weak spring converges slowly.  Repeated
                    ;; scrolling retargets the same animation and can keep it running indefinitely.
                    (and smear-cursor-max-duration
                         (smear-cursor--anim-born anim)
                         (> (- (float-time) (smear-cursor--anim-born anim))
                            smear-cursor-max-duration)))
                (smear-cursor--stop)
              ;; Reset flags so paint results from the previous frame do not carry over.
              (setq smear-cursor--offscreen nil
                    smear-cursor--measure-deferred nil)
              (let* ((dirty (smear-cursor--paint-frame anim))
                     (cost (- (float-time) t0))
                     (gc (- gc-elapsed gc0)))
                (setf (smear-cursor--anim-paint-total anim)
                      (+ (or (smear-cursor--anim-paint-total anim) 0.0)
                         cost)
                      (smear-cursor--anim-paint-frames anim)
                      (1+ (smear-cursor--anim-paint-frames anim)))
                ;; Allow one slow frame for setup or retargeting.  A measured 46-row
                ;; trail took 51 ms to set up and 1.5 ms per later frame.  Stopping on
                ;; setup cost ends long trails before their second frame; the budget
                ;; limits sustained drawing cost.
                (if (smear-cursor--over-frame-budget-p cost gc)
                    (cl-incf (smear-cursor--anim-slow anim))
                  (setf (smear-cursor--anim-slow anim) 0))
                (cond
                 ((>= (smear-cursor--anim-slow anim) 2)
                  (smear-cursor--stop))
                 ((> dirty smear-cursor-max-cells)
                  (smear-cursor--stop))
                 ((> dirty 0)
                  (setf (smear-cursor--anim-blanks anim) 0)
                  (smear-cursor--ensure-hidden anim))
                 ;; Keep running when measurement is deferred so later frames can draw
                 ;; the unfinished rows.
                 (smear-cursor--measure-deferred nil)
                 ;; A trail from another window can start outside this window and
                 ;; become visible later.
                 (smear-cursor--offscreen nil)
                 ;; Stop after repeated empty frames.  Unsupported proportional or
                 ;; scaled rows can skip every cell, leaving an invisible animation.
                 ((>= (cl-incf (smear-cursor--anim-blanks anim)) 2)
                  (smear-cursor--stop)))))))))))

(defun smear-cursor--start (win old-rect new-rect)
  "Start or retarget an animation in WIN from OLD-RECT to NEW-RECT."
  (smear-cursor--rest-yield win)
  (let ((anim smear-cursor--anim))
    (if (and anim
             (eq (smear-cursor--anim-window anim) win)
             (smear-cursor--context-valid-p anim))
        (let* ((lh (max 1 (round (aref new-rect 3))))
               (y-base (mod (round (aref new-rect 1)) lh)))
          ;; A different row pitch or phase invalidates cached row and cell positions.
          (when (or (/= lh (smear-cursor--anim-lh anim))
                    (/= y-base (smear-cursor--anim-y-base anim)))
            (smear-cursor--flush-cells anim)
            (setf (smear-cursor--anim-lh anim) lh
                  (smear-cursor--anim-y-base anim) y-base))
          ;; Resume from the displayed frame, not the final precomputed frame.
          (smear-cursor--x11-resume anim)
          (setf (smear-cursor--anim-target anim) new-rect)
          (let ((cap (smear-cursor--max-length-px
                      nil (smear-cursor--anim-cw anim))))
            (when cap
              (smear-cursor--clamp-corners
               (smear-cursor--anim-corners anim) new-rect cap))))
      (smear-cursor--stop)
      (let* ((buf (window-buffer win))
             (frame (window-frame win))
             (lh (max 1 (round (aref new-rect 3))))
             (new (smear-cursor--anim-create
                   :window win :buffer buf
                   :born (float-time)
                   :gc0 gc-elapsed
                   :tick (with-current-buffer buf (buffer-chars-modified-tick))
                   :window-start (window-start win)
                   :corners (make-vector 8 0.0)
                   :target new-rect
                   :cells (make-hash-table :test 'eql)
                   :pads (make-hash-table :test 'eql)
                   :cw (frame-char-width frame)
                   :lh lh
                   :ch (min (frame-char-height frame) lh)
                   :y-base (mod (round (aref new-rect 1)) lh)
                   :win-w (window-text-width win t)
                   :win-h (window-text-height win t)
                   :color (or (and smear-cursor-color
                                   (smear-cursor--color-rgb
                                    smear-cursor-color))
                              (smear-cursor--color-rgb
                               (face-background 'cursor nil t))
                              [200 200 200])
                   :fade (vector 0.0 0.0 1.0)
                   :frame 0 :last-time (float-time)
                   :cursor-saved nil)))
        (smear-cursor--corners-from-rect (smear-cursor--anim-corners new)
                                         old-rect)
        (let ((cap (smear-cursor--max-length-px
                    frame (frame-char-width frame))))
          (when cap
            (smear-cursor--clamp-corners
             (smear-cursor--anim-corners new) new-rect cap)))
        (let ((v (smear-cursor--anim-velocities new))
              (c (smear-cursor--anim-corners new)))
          (dotimes (i 4)
            (aset v (* 2 i)
                  (smear-cursor--wind-up
                   (- (smear-cursor--target-x new-rect i) (aref c (* 2 i)))
                   (frame-char-width frame)))
            (aset v (1+ (* 2 i))
                  (smear-cursor--wind-up
                   (- (smear-cursor--target-y new-rect i)
                      (aref c (1+ (* 2 i))))
                   lh))))
        (setq smear-cursor--anim new)))
    (smear-cursor--launch smear-cursor--anim)))

(defun smear-cursor--launch (anim)
  "Start ANIM using the player thread or a Lisp frame timer."
  (when anim
    (or (smear-cursor--fly-x11 anim)
        (unless (timerp (smear-cursor--anim-timer anim))
          (setf (smear-cursor--anim-timer anim)
                (run-at-time 0 (/ 1.0 (max 1 smear-cursor-fps))
                             #'smear-cursor--tick))))))

;;;; Trigger

(defvar smear-cursor--last-rects (make-hash-table :test 'eq :weakness 'key)
  "Store each window's last sampled position as (BUFFER . RECT).")

(defvar smear-cursor--sample-timer nil)

(defvar smear-cursor--last-window nil
  "Record the window selected at the last cursor sample.

This detects movement between windows even when neither window's point
changes.")

(defun smear-cursor--rect-in-window (rect from to)
  "Convert RECT from window FROM's text-area coordinates to TO's.

The result can lie outside TO for a trail entering from another window.
`smear-cursor--clamp-corners' limits how far back it starts."
  (let ((from-edges (window-edges from t nil t))
        (to-edges (window-edges to t nil t)))
    (vector (+ (aref rect 0) (float (- (nth 0 from-edges) (nth 0 to-edges))))
            (+ (aref rect 1) (float (- (nth 1 from-edges) (nth 1 to-edges))))
            (aref rect 2) (aref rect 3))))

(defun smear-cursor--origin-rect (win _buf)
  "Return the previous cursor rectangle in WIN's pixels, or nil.

Prefer movement from another window over movement within WIN.  Buffer
changes and scrolling do not discard the origin.  _BUF is unused."
  (let ((prev smear-cursor--last-window))
    (if (and prev
             (not (eq prev win))
             (window-live-p prev)
             ;; Frame-relative edges are valid only within the same frame.
             (eq (window-frame prev) (window-frame win)))
        (let ((was (gethash prev smear-cursor--last-rects)))
          (and was (smear-cursor--rect-in-window (nth 2 was) prev win)))
      (let ((was (gethash win smear-cursor--last-rects)))
        (and was (nth 2 was))))))

(defun smear-cursor--cursor-row (win y)
  "Return WIN's displayed cursor row if it contains pixel Y, or nil.

This avoids using a row from an earlier cursor position before redisplay."
  (let* ((box (window-line-height nil win))
         (top (nth 2 box))
         (h (car-safe box)))
    (and top h (<= top y) (< y (+ top h)) box)))

(defun smear-cursor--point-rect (win)
  "Return point's rectangle [X Y W H] in WIN's text-area pixels.

Use the screen line's y coordinate and height rather than the glyph's.
Return nil if point is hidden or the line has zero height."
  (let ((xy (pos-visible-in-window-p (window-point win) win t)))
    (when xy
      (let* ((box (smear-cursor--cursor-row win (cadr xy)))
             (h (or (car-safe box)
                    (with-selected-window win (line-pixel-height))))
             (y (if box (nth 2 box) (cadr xy))))
        (when (and h y (> h 0))
          (vector (float (car xy)) (float y)
                  (float (frame-char-width (window-frame win)))
                  (float h)))))))

(defun smear-cursor--move-cells (old new cw lh)
  "Return the movement between OLD and NEW in character cells.

Use cell width CW and line height LH and take the larger axis distance."
  (max (/ (abs (- (aref new 0) (aref old 0))) (float cw))
       (/ (abs (- (aref new 1) (aref old 1))) (float lh))))

(defun smear-cursor--post-command ()
  "Schedule a cursor position sample after redisplay."
  (setq smear-cursor--last-command this-command)
  (unless smear-cursor--sample-timer
    (setq smear-cursor--sample-timer
          (run-at-time 0 nil #'smear-cursor--sample-soon
                       smear-cursor--redisplay-tries))))

(defvar smear-cursor--sample-serial 0
  "Count cursor samples so landing checks can detect a newer sample.")

(defvar smear-cursor--land-timer nil
  "Store the pending landing check timer, or nil.")

(defconst smear-cursor--landing-checks '(1 1 2 4)
  "Set the frame intervals between checks of the animation's target.")

(defun smear-cursor--land-soon (win serial delays)
  "Check WIN's target for sample SERIAL after the first of DELAYS frames.

Use the remaining DELAYS for later checks."
  (when smear-cursor--land-timer
    (cancel-timer smear-cursor--land-timer))
  (setq smear-cursor--land-timer
        (and delays
             (run-at-time (/ (float (car delays)) (max 1 smear-cursor-fps))
                          nil #'smear-cursor--land win serial (cdr delays)))))

(defun smear-cursor--landing-p (win serial)
  "Return non-nil if sample SERIAL still identifies WIN's displayed trail.

This does not require a live Lisp animation; the thread continues playback
after a window change stops it in Lisp."
  (and smear-cursor-mode
       (= serial smear-cursor--sample-serial)
       (window-live-p win)
       (eq win (selected-window))
       (or (null smear-cursor--anim)
           (eq (smear-cursor--anim-window smear-cursor--anim) win))
       (eq (car (gethash win smear-cursor--last-rects)) (window-buffer win))))

(defun smear-cursor--landing-rect (win)
  "Return point's rectangle in WIN with animation padding width removed."
  (if smear-cursor--anim
      (smear-cursor--without-pads smear-cursor--anim
                                  (lambda () (smear-cursor--point-rect win)))
    (smear-cursor--point-rect win)))

(defun smear-cursor--rects-differ-p (a b)
  "Return non-nil if rectangles A and B are at different positions."
  (or (> (abs (- (aref a 0) (aref b 0))) 0.5)
      (> (abs (- (aref a 1) (aref b 1))) 0.5)))

(defun smear-cursor--land (win serial delays)
  "Retarget WIN's animation if point moved after sample SERIAL.

Use the remaining DELAYS to schedule later checks.  A newer sample
invalidates these checks."
  (setq smear-cursor--land-timer nil)
  (when (smear-cursor--landing-p win serial)
    (let* ((aim (nth 2 (gethash win smear-cursor--last-rects)))
           (new (smear-cursor--landing-rect win)))
      (when (and new (smear-cursor--rects-differ-p aim new))
        (smear-cursor--trace
         "%-28s aimed at y=%.0f, point is now at y=%.0f -> re-aimed"
         (or smear-cursor--last-command "-") (aref aim 1) (aref new 1))
        (smear-cursor--start win aim new)
        (puthash win (list (window-buffer win) (window-start win) new)
                 smear-cursor--last-rects)))
    (smear-cursor--land-soon win serial delays)))

(defun smear-cursor--sample-soon (tries)
  "Sample point, retrying up to TRIES times if it is off screen.

A zero-delay timer can run before redisplay, so retry on later frames."
  (unless (or (smear-cursor--sample) (<= tries 0))
    (setq smear-cursor--sample-timer
          (smear-cursor--again-soon #'smear-cursor--sample-soon tries))))

(defun smear-cursor--tracking-p ()
  "Return non-nil while a mouse gesture is still under way.

`mouse-drag-region\=' binds `track-mouse\=' for the length of a click or a
drag and moves point inside it, and the sample timer runs while it
does.  Unlike a region, this is over as soon as the button comes up."
  (and (not smear-cursor-while-selecting)
       (boundp 'track-mouse)
       track-mouse))

(defun smear-cursor--selecting-p ()
  "Return non-nil when selection should suppress the trail.

Check for an active region or mouse tracking unless
`smear-cursor-while-selecting\=' is enabled."
  (or (and (not smear-cursor-while-selecting) (region-active-p))
      (smear-cursor--tracking-p)))

(defcustom smear-cursor-trace nil
  "Set the file for cursor sample traces, or nil to disable tracing.

Reproduce missing trails and read the trace for the reason each sample did
or did not start an animation.  Compare with x11/smear-cursor-x11-watch to
check display timing."
  :type '(choice (const :tag "trace nothing" nil) file)
  :group 'smear-cursor)

(defun smear-cursor--trace (fmt &rest args)
  "Append FMT formatted with ARGS to `smear-cursor-trace' if enabled.

Prefix the line with a wall-clock timestamp from `float-time'."
  (when smear-cursor-trace
    (ignore-errors
      (let ((line (concat (format "%.3f " (float-time))
                          (apply #'format fmt args) "\n")))
        (write-region line nil smear-cursor-trace t 'quiet)))))

(defvar smear-cursor--prompt-timer nil
  "Timer waiting for the cursor to settle behind a prompt, or nil.")

(defvar smear-cursor--prompt-settled nil
  "Non-nil while the one sample a settled prompt allows is running.")

(defun smear-cursor--prompt-wait ()
  "Put off drawing until the cursor has been still for a moment.

Each jump cancels the last wait and starts another, so a list being
skimmed draws nothing and the pause at the end of it draws one trail."
  (when smear-cursor--prompt-timer
    (cancel-timer smear-cursor--prompt-timer))
  (setq smear-cursor--prompt-timer
        (run-at-time smear-cursor-prompt-settle nil
                     #'smear-cursor--prompt-settle)))

(defun smear-cursor--prompt-settle ()
  "Draw the trail the prompt has been holding back."
  (setq smear-cursor--prompt-timer nil)
  (let ((smear-cursor--prompt-settled t))
    (smear-cursor--flourish (smear-cursor--sample))))

(defun smear-cursor--record-p (why)
  "Return non-nil when a sample refused for WHY should still be recorded.

A burst behind a prompt is not: the trail drawn when it settles should
span the whole of it, and recording each jump would leave it spanning
the last hop."
  (not (and (equal why "a prompt is up") (not smear-cursor--prompt-settled))))

(defun smear-cursor--why-not (win old new)
  "Return why NEW cannot start a trail from OLD in WIN, or nil if it can."
  (cond
   ((null new) "point is not on screen")
   ((null old) "nothing recorded to smear from")
   ((smear-cursor--tracking-p) "the mouse is still down")
   ((smear-cursor--selecting-p) "a selection is being made")
   ;; Asked of the depth rather than of the minibuffer window's buffer,
   ;; which is a minibuffer whether or not anything is being read in
   ;; it.
   ((and (> (minibuffer-depth) 0)
         (not smear-cursor--prompt-settled)
         (or (not smear-cursor-while-prompting)
             smear-cursor-prompt-settle))
    "a prompt is up")
   ((< (smear-cursor--move-cells
        old new (frame-char-width (window-frame win)) (round (aref new 3)))
       smear-cursor-min-distance)
    "the cursor did not move far enough")))

(defun smear-cursor--sample-window ()
  "Return the window whose point to sample.

While the minibuffer is selected, point still moves in the window behind
it: `consult-line' and the other preview commands jump there and leave
the minibuffer selected.  Return that window, so the jump is drawn.

With nothing behind it, return the minibuffer window itself, which
`smear-cursor--sample' then refuses.  A trail in the minibuffer marks
nothing: it is one line tall and already under the eye."
  (let ((win (selected-window)))
    (if (not (minibufferp (window-buffer win)))
        win
      (let ((back (minibuffer-selected-window)))
        (if (and (window-live-p back)
                 (not (minibufferp (window-buffer back))))
            back
          ;; Nothing behind it.  Hand back the minibuffer window so the
          ;; caller's own check rejects it.
          win)))))

(defun smear-cursor--sample ()
  "Sample point's pixel rectangle and start or retarget a trail on movement.

Detect movement within or between windows.  Run from a timer so layout
reflects completed redisplay."
  (setq smear-cursor--sample-timer nil)
  (let ((win (smear-cursor--sample-window)))
    (if (not (and smear-cursor-mode
                  (display-graphic-p)
                  (image-type-available-p 'canvas)
                  (not (minibufferp (window-buffer win)))))
        ;; Return success to prevent retries when sampling is unavailable.
        t
      ;; Measure without padding width.  In an EOL measurement, ten padding
      ;; cells changed x from 400 to 480 pixels, aiming the next trail to the
      ;; right of the cursor.
      (let* ((new (if smear-cursor--anim
                      (smear-cursor--without-pads
                       smear-cursor--anim
                       (lambda () (smear-cursor--point-rect win)))
                    (smear-cursor--point-rect win)))
             (buf (window-buffer win))
             (old (smear-cursor--origin-rect win buf))
             (why (smear-cursor--why-not win old new)))
        (smear-cursor--trace
         "%-28s %s %s -> %s"
         (or smear-cursor--last-command "-")
         (if old (format "from y=%.0f" (aref old 1)) "from nowhere")
         (if new (format "to y=%.0f" (aref new 1)) "to nowhere")
         (or why "smear"))
        (when new
          (progn
            (setq smear-cursor--sample-serial
                  (1+ smear-cursor--sample-serial))
            (when (and (equal why "a prompt is up")
                       smear-cursor-while-prompting
                       smear-cursor-prompt-settle)
              (smear-cursor--prompt-wait))
            (unless why
              (smear-cursor--start win old new)
              (smear-cursor--land-soon win smear-cursor--sample-serial
                                       smear-cursor--landing-checks)
              ;; Use distance so jumps from commands outside the configured list
              ;; still get a pulse.
              (when (and smear-cursor-pulse-min-rows
                         (>= (/ (abs (- (aref new 1) (aref old 1)))
                                (max 1.0 (aref new 3)))
                             smear-cursor-pulse-min-rows))
                (smear-cursor--pulse-soon)))
            ;; Not while the button is still down.  The gesture has
            ;; already moved point, and recording where it now is
            ;; would leave the sample after the release comparing the
            ;; new position against itself, so a plain click would
            ;; draw nothing.  A region outlives the gesture and is
            ;; recorded as before: a cursor defining one is not
            ;; travelling, and the next real move should start from
            ;; where the selection left it.
            (unless (or (smear-cursor--tracking-p)
                        (not (smear-cursor--record-p why)))
              (puthash win (list buf (window-start win) new)
                       smear-cursor--last-rects))))
        (setq smear-cursor--last-window win)
        ;; Retry only when point could not be measured.
        (and new t)))))

(defun smear-cursor--on-window-change (&rest _)
  "Stop the animation when the window configuration changes.

Hook arguments _ are ignored."
  (when smear-cursor--anim
    (smear-cursor--stop)))

;;;###autoload
(define-minor-mode smear-cursor-mode
  "Animate cursor movement with a canvas trail.

This is inactive on terminal frames or without canvas image support."
  :global t
  (if smear-cursor-mode
      (progn
        (add-hook 'post-command-hook #'smear-cursor--post-command)
        (add-hook 'post-command-hook #'smear-cursor--maybe-pulse)
        (add-hook 'before-change-functions #'smear-cursor--delete-fire)
        (add-hook 'post-self-insert-hook #'smear-cursor--insert-effect)
        ;; Advise `copy-region-as-kill' because `kill-ring-save' returns after
        ;; `indicate-copied-region' sleeps.  That delays the flash by a third
        ;; of a second, so it can highlight text during the next action.
        (advice-add 'copy-region-as-kill :after #'smear-cursor--copy-effect)
        (advice-add 'yank :after #'smear-cursor--yank-effect)
        (advice-add 'yank-pop :after #'smear-cursor--yank-effect)
        (add-hook 'after-change-functions #'smear-cursor--newline-fire)
        (add-hook 'after-change-functions #'smear-cursor--highlight-change)
        (add-hook 'window-configuration-change-hook
                  #'smear-cursor--on-window-change)
        (add-hook 'post-command-hook #'smear-cursor--idle-interrupt)
        (add-hook 'post-command-hook #'smear-cursor--rest-follow)
        (smear-cursor--idle-setup)
        (smear-cursor--rest-setup))
    (remove-hook 'post-command-hook #'smear-cursor--rest-follow)
    (remove-hook 'post-command-hook #'smear-cursor--idle-interrupt)
    (remove-hook 'post-command-hook #'smear-cursor--post-command)
    (remove-hook 'post-command-hook #'smear-cursor--maybe-pulse)
    (remove-hook 'before-change-functions #'smear-cursor--delete-fire)
    (remove-hook 'post-self-insert-hook #'smear-cursor--insert-effect)
    (advice-remove 'kill-ring-save #'smear-cursor--copy-effect)  ; Remove any advice still installed on the outer copy command.
    (advice-remove 'copy-region-as-kill #'smear-cursor--copy-effect)
    (advice-remove 'yank #'smear-cursor--yank-effect)
    (advice-remove 'yank-pop #'smear-cursor--yank-effect)
    (smear-cursor--effects-down)
    (remove-hook 'after-change-functions #'smear-cursor--newline-fire)
    (remove-hook 'after-change-functions #'smear-cursor--highlight-change)
    (smear-cursor--idle-stop)
    (smear-cursor--rest-stop)
    (smear-cursor--clear-highlights)
    (remove-hook 'window-configuration-change-hook
                 #'smear-cursor--on-window-change)
    (when smear-cursor--sample-timer
      (cancel-timer smear-cursor--sample-timer)
      (setq smear-cursor--sample-timer nil))
    (setq smear-cursor--last-window nil)
    (smear-cursor--stop)))

;;;; Telling whether an effect reached the screen

(defun smear-cursor--probe-layer (stage head rect layer u)
  "Return how many pixels LAYER changed where it should draw at time U.

STAGE is what it draws on, HEAD the effect\'s centre in frame pixels
and RECT its rectangle.
Compares the overlay with the window under it, so the answer is what
the layer put on the screen rather than what was asked of it."
  (let* ((quad (or (plist-get layer :quad) (smear-cursor--layer-phase layer u)))
         (at (if quad
                 ;; A shape of its own is drawn around its own middle,
                 ;; wherever the head is.
                 (let ((cx 0.0) (cy 0.0))
                   (dotimes (i 4)
                     (setq cx (+ cx (/ (aref quad (* 2 i)) 4.0))
                           cy (+ cy (/ (aref quad (1+ (* 2 i))) 4.0))))
                   (vector (+ (aref head 0) cx)
                           (+ (smear-cursor--layer-anchor layer rect) cy)))
               (smear-cursor--layer-head layer head rect u)))
         (r (max 12 (round (or (plist-get layer :radius) 12))))
         (s (ignore-errors
              (smear-cursor-x11--sample stage
                                        (round (- (aref at 0) r))
                                        (round (- (aref at 1) r))
                                        (* 2 r) (* 2 r)))))
    (list (or (plist-get layer :part) (plist-get layer :shape))
          (round (- (aref at 0) (aref head 0)))
          (round (- (aref at 1) (aref head 1)))
          ;; How many pixels differ from the window underneath, not how
          ;; bright the place is: the overlay carries a copy of the
          ;; window, so brightness says what the text there looks like
          ;; and nothing about whether the layer drew.
          (and s (round (aref s 2))))))

(defun smear-cursor--probe-play (win name effect layer)
  "Play EFFECT\='s LAYER by itself in WIN and return what it drew.

NAME is the effect it came from.  One layer at a time, because layers
overlap: an effect that covers text with one layer changes the pixels
under all of them, and a reading taken with everything playing says
only that something drew."
  (let* ((stage (smear-cursor--effect-stage win))
         (rect (smear-cursor--pos-rect win (window-point win)))
         (frect (smear-cursor--rect-in-frame win rect))
         (head (vector (+ (aref frect 0) (/ (aref frect 2) 2.0))
                       (+ (aref frect 1) (/ (aref frect 3) 2.0))))
         (one (append (list :layers (list layer)) effect))
         (n (max 2 (round (* (or (plist-get effect :duration) 0.3)
                             (smear-cursor--effect-rate effect)))))
         (best nil))
    (cl-letf (((symbol-function 'smear-cursor-effect) (lambda (_n) one)))
      (smear-cursor--play-effect win name (list rect)
                                 smear-cursor--track-occasion))
    (dotimes (_ 10)
      (let* ((pos (or (ignore-errors
                        (smear-cursor-x11--play-position
                         stage smear-cursor--track-occasion))
                      0))
             (u (/ (float (min pos (1- n))) (max 1 (1- n))))
             (row (smear-cursor--probe-layer stage head frect layer u)))
        (when (or (null best) (> (or (nth 3 row) 0) (or (nth 3 best) 0)))
          (setq best row)))
      (sit-for 0.06))
    (sit-for 0.15)
    best))

(defun smear-cursor-report-effect (name)
  "Play effect NAME a layer at a time and report what each one drew.

For finding out why an effect does not show up.  Each layer is played
by itself and the overlay is read back where it should be drawing, so
a nought means that layer put nothing on the screen rather than that
something else covered it."
  (interactive
   (list (intern (completing-read "Report on effect: "
                                  (mapcar #'symbol-name
                                          (smear-cursor--effect-names))
                                  nil t))))
  (let* ((win (smear-cursor--sample-window))
         (stage (smear-cursor--effect-stage win))
         (effect (let ((smear-cursor--effect-window win))
                   (smear-cursor-effect name)))
         (out nil))
    (unless stage
      (user-error "Smear-cursor: no stage on this frame; effects need the `x11' backend"))
    (unless (smear-cursor--pos-rect win (window-point win))
      (user-error "Smear-cursor: point is not visible, so there is nowhere to draw"))
    (dolist (layer (plist-get effect :layers))
      (push (smear-cursor--probe-play win name effect layer) out))
    (with-current-buffer (get-buffer-create "*smear-cursor report*")
      (erase-buffer)
      (insert (format "%s on %s, %d layers, %s renderer\n\n"
                      name (smear-cursor-x11--describe stage)
                      (length (plist-get effect :layers))
                      (if (boundp 'smear-cursor-x11-renderer)
                          smear-cursor-x11-renderer "?")))
      (insert "part          offset from cursor   pixels it changed\n")
      (dolist (row (nreverse out))
        (insert (format "%-12s  %+5d %+5d          %s\n"
                        (nth 0 row) (nth 1 row) (nth 2 row)
                        (if (nth 3 row) (nth 3 row) "could not read"))))
      (insert "\nEach layer played on its own.  A nought drew nothing.\n")
      (display-buffer (current-buffer)))))

;;;; Demo

(defun smear-cursor-demo ()
  "Demonstrate cursor trails by moving in a scratch buffer and reporting times."
  (interactive)
  (unless (and (display-graphic-p) (image-type-available-p 'canvas))
    (user-error "smear-cursor needs a GUI frame with canvas support"))
  (let ((buf (get-buffer-create "*smear-cursor-demo*")))
    (with-current-buffer buf
      (erase-buffer)
      (dotimes (i 30)
        (insert (format "%02d  the quick brown fox jumps over the lazy dog %s\n"
                        i (make-string (% (* i 7) 25) ?x)))))
    (pop-to-buffer buf)
    (smear-cursor-mode 1)
    (goto-char (point-min))
    (dolist (step '((25 . 40) (2 . 5) (28 . 60) (14 . 0) (0 . 70) (29 . 10)))
      (goto-char (point-min))
      (forward-line (car step))
      (move-to-column (cdr step))
      ;; Programmatic motion does not run `post-command-hook'.
      (smear-cursor--post-command)
      (sit-for 0.45))
    (when smear-cursor--last-stats
      (let ((n (max 1 (plist-get smear-cursor--last-stats :frames))))
        (message (concat "smear-cursor: %.2f ms paint/frame, delivered"
                         " %.1f ms/frame avg (worst %.1f ms), %d frames")
                 (/ (* 1000.0 (plist-get smear-cursor--last-stats
                                         :paint-total))
                    n)
                 (/ (* 1000.0 (plist-get smear-cursor--last-stats
                                         :gap-sum))
                    n)
                 (* 1000.0 (plist-get smear-cursor--last-stats :gap-max))
                 n)))))

;;;; Reload safety

(defun smear-cursor--reset-state ()
  "Clear all cached and active animation state.

Avoid animation structure accessors so cleanup is safe even when stored
records have different layouts."
  (dolist (tm (copy-sequence timer-list))
    (when (memq (timer--function tm)
                '(smear-cursor--tick smear-cursor--sample))
      (cancel-timer tm)))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'smear-cursor)
          (delete-overlay ov)))))
  (setq smear-cursor--anim nil
        ;; Replace the table without accessing records whose struct layouts
        ;; may differ from the current definitions.
        smear-cursor--pool (make-hash-table :test 'eql)
        smear-cursor--pool-size nil
        smear-cursor--sample-timer nil
        smear-cursor--last-window nil
        smear-cursor--last-stats nil
        smear-cursor--stale-cells 0))

(smear-cursor--reset-state)

(provide 'smear-cursor)
;;; smear-cursor.el ends here
