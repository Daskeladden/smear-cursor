;;; smear-cursor-menu.el --- One place to see what smear-cursor is doing -*- lexical-binding: t -*-

;; Copyright (C) 2026 smear-cursor contributors

;; Author: Daskeladden
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

;; Use `M-x smear-cursor-menu' to view and change settings, see which
;; apply to the current backend, and access diagnostics.
;;
;; For example, changing an X11-only setting while using the Lisp
;; rasterizer has no visible effect and produces no error.  The menu marks such settings
;; so they are easier to distinguish from broken features.

;;; Code:

(require 'transient)
(require 'smear-cursor)
;; X11 settings may be unavailable when using another backend.
;; Guard references to them so the menu works without this library.
(require 'smear-cursor-x11 nil t)

(defconst smear-cursor-menu--needs
  '((smear-cursor-x11-renderer   . x11)
    (smear-cursor-x11-alpha      . x11)
    (smear-cursor-x11-tail-alpha . x11))
  "Map settings to the backend that uses them.

The menu marks no backend restriction for settings absent from this
list.")

(defun smear-cursor-menu--inert-p (sym)
  "Return non-nil if setting SYM requires a different backend."
  (let ((needs (cdr (assq sym smear-cursor-menu--needs))))
    (and needs (not (eq needs smear-cursor-backend)))))

(defun smear-cursor-menu--default (sym)
  "Return the default value of SYM, or nil if it has none."
  (let ((sv (get sym 'standard-value)))
    (and sv (ignore-errors (eval (car sv) t)))))

(defun smear-cursor-menu--ignored ()
  "Return changed settings that do not apply to the current backend.

Their values are read without errors but have no visible effect or
explanation.  Omit settings that still have their default values."
  (seq-filter (lambda (sym)
                (and (boundp sym)
                     (smear-cursor-menu--inert-p sym)
                     (not (equal (symbol-value sym)
                                 (smear-cursor-menu--default sym)))))
              (mapcar #'car smear-cursor-menu--needs)))

(defun smear-cursor-menu--show (v)
  "Return V as a short string for a menu line."
  (cond ((null v) "off")
        ((eq v t) "on")
        ((numberp v) (format "%s" v))
        ((stringp v) v)
        (t (format "%s" v))))

(defface smear-cursor-menu-value '((t :inherit transient-value))
  "Face for a setting\='s current value in the menu."
  :group 'smear-cursor)

(defface smear-cursor-menu-off '((t :inherit transient-inactive-value))
  "Face for a value that is off, or not set.

Read at a glance, a setting doing nothing should look like it."
  :group 'smear-cursor)

(defface smear-cursor-menu-note '((t :inherit shadow))
  "Face for the asides in the menu: what a key does, what a value costs."
  :group 'smear-cursor)

(defface smear-cursor-menu-alert '((t :inherit warning))
  "Face for what wants attention: settings changed and not yet kept."
  :group 'smear-cursor)

(defun smear-cursor-menu--paint (value face)
  "Return VALUE in FACE, unless it carries a face of its own already."
  (if (and (> (length value) 0) (get-text-property 0 'face value))
      value
    (propertize value 'face face)))

(defun smear-cursor-menu--hint (text)
  "Return TEXT as an aside rather than as a value."
  (smear-cursor-menu--paint text 'smear-cursor-menu-note))

(defconst smear-cursor-menu--label 18
  "Columns given to a setting\='s name.")

(defconst smear-cursor-menu--value 13
  "Columns given to its value.

Padded rather than trimmed: a value wider than this widens its column,
which is one ragged column against a menu of unreadable half-values.")

(defconst smear-cursor-menu--mark "*"
  "What marks a setting changed this session and not yet saved.")

(defun smear-cursor-menu--marker (mark)
  "Return the column every line starts with, starred when MARK."
  (if mark
      (propertize smear-cursor-menu--mark 'face 'smear-cursor-menu-alert)
    (make-string (length smear-cursor-menu--mark) ?\s)))

(defun smear-cursor-menu--line (text value &optional note mark)
  "Return a menu line reading TEXT then VALUE, with NOTE after it.

Non-nil MARK stars it as changed and not saved.  The star has a column
of its own whether or not it is there, so a starred line is no wider
than a plain one.

One width for every line in the menu, so that transient -- which pads
a column to its widest line, and each row of columns to itself --
lines the columns up down the whole menu rather than row by row."
  (concat (smear-cursor-menu--marker mark)
          (string-pad text smear-cursor-menu--label) " "
          (string-pad (smear-cursor-menu--paint value 'smear-cursor-menu-value)
                      smear-cursor-menu--value)
          (if note (smear-cursor-menu--paint note 'smear-cursor-menu-note) "")))

(defun smear-cursor-menu--command (text)
  "Return a menu line for a command with no value, reading TEXT.

It opens with the star column every setting line has, so its text
starts where their labels do."
  (concat (smear-cursor-menu--marker nil) text))

(defun smear-cursor-menu--setting-line (sym text value &optional note)
  "Return a menu line for setting SYM, reading TEXT then VALUE.

Starred when SYM has been changed this session and not saved.  Every
line that stands for a setting goes through here, however it works out
what to show: a star wired into one of them marks one of them."
  (smear-cursor-menu--line text value note (get sym 'customized-value)))

(defun smear-cursor-menu--describe-of (sym text most &optional unset)
  "Return a menu line for SYM, labelled TEXT, showing MOST as its limit.

Read as \"2 of 4\": a ceiling the drawing knows about and the menu does
not is a setting whose number means less than it looks.  UNSET labels
a nought, which is off rather than none of four."
  (let ((v (and (boundp sym) (symbol-value sym))))
    (if (or (null v) (eql v 0))
        ;; None of four is off, and reads better as off.
        (smear-cursor-menu--setting-line
         sym text (smear-cursor-menu--paint (or unset "off")
                                            'smear-cursor-menu-off))
      (smear-cursor-menu--setting-line
       sym text (smear-cursor-menu--paint
                 (format "%s of %s" (smear-cursor-menu--show v) most)
                 'smear-cursor-menu-value)))))

(defun smear-cursor-menu--describe (sym text &optional unset)
  "Return a menu line for setting SYM, labelled TEXT.

Use UNSET to label a nil value when nil means something other than off."
  (let ((v (and (boundp sym) (symbol-value sym))))
    (smear-cursor-menu--setting-line
     sym text
     (smear-cursor-menu--paint
      (if (and (null v) unset) unset (smear-cursor-menu--show v))
      (if v 'smear-cursor-menu-value 'smear-cursor-menu-off))
     (when (smear-cursor-menu--inert-p sym)
       (format "  (needs backend `%s' -- doing nothing)"
               (cdr (assq sym smear-cursor-menu--needs)))))))

(defun smear-cursor-menu--state ()
  "Return a heading with the mode status, backend, renderer and trail style."
  (concat "smear-cursor "
          (smear-cursor-menu--paint
           (if smear-cursor-mode "on" "off")
           (if smear-cursor-mode 'smear-cursor-menu-value
             'smear-cursor-menu-off))
          (smear-cursor-menu--hint "  |  ")
          (smear-cursor-menu--paint
           (format "%s/%s  %s"
                   smear-cursor-backend
                   (if (and (eq smear-cursor-backend 'x11)
                            (boundp 'smear-cursor-x11-renderer))
                       (symbol-value 'smear-cursor-x11-renderer)
                     "-")
                   smear-cursor-trail-style)
           'smear-cursor-menu-value)
          ;; here rather than on the renderer's own line: a note long
          ;; enough to say anything is longer than the column it would
          ;; be in, and would push the column beside it across the frame
          (let ((note (smear-cursor-menu--renderer-note)))
            (if note (smear-cursor-menu--paint note 'smear-cursor-menu-alert)
              ""))))

(defun smear-cursor-menu--trail-names ()
  "Return the names of all defined trail styles."
  (let (names)
    (maphash (lambda (k _v) (push k names)) smear-cursor--trails)
    (sort names #'string<)))

(defalias 'smear-cursor-menu--effect-names #'smear-cursor--effect-names
  "Every effect anyone has defined, by name.")

(defvar smear-cursor-menu--effects-before nil
  "`smear-cursor-effects\=' as it stood before the menu first changed it.

Five occasions are five lines of one setting, so `customized-value\='
says the alist is unsaved without saying which line of it moved.  This
is what the lines are compared against to find out.")

(defun smear-cursor-menu--effects-forget ()
  "Forget which occasions have been changed, having nothing outstanding."
  (setq smear-cursor-menu--effects-before nil))

(defun smear-cursor-menu--effects-remember ()
  "Keep the effects as they stand, if this is the first change to them.

On whether anything is recorded rather than on whether the setting is
marked customized: a session that changed an occasion before this
record existed is marked already, and comparing against nothing stars
every line."
  (unless smear-cursor-menu--effects-before
    (setq smear-cursor-menu--effects-before (copy-alist smear-cursor-effects))))

(defun smear-cursor-menu--occasion-moved-p (occasion)
  "Return non-nil when OCCASION has been changed here and not saved."
  (and smear-cursor-menu--effects-before
       (get 'smear-cursor-effects 'customized-value)
       (not (eq (cdr (assq occasion smear-cursor-effects))
                (cdr (assq occasion smear-cursor-menu--effects-before))))))

(defun smear-cursor-menu--set-occasion (occasion effect)
  "Set the effect for OCCASION to EFFECT in `smear-cursor-effects'.

Replace an existing entry for OCCASION."
  (smear-cursor-menu--effects-remember)
  (let ((cell (assq occasion smear-cursor-effects)))
    (if cell
        (setcdr cell effect)
      (setq smear-cursor-effects
            (append smear-cursor-effects (list (cons occasion effect))))))
  (smear-cursor-menu--put 'smear-cursor-effects smear-cursor-effects)
  smear-cursor-effects)


;;;; Reading a new value

(defun smear-cursor-menu--put (sym val)
  "Store VAL in SYM the way `customize' would.

Through the setting\='s own setter rather than `set': a setter is
where the work behind a setting lives -- re-arming the idle timer,
say -- and a menu that skipped it would change the value and leave the
package behaving as it did before."
  (customize-set-variable sym val))

(defun smear-cursor-menu--unsaved ()
  "Return the smear-cursor settings changed this session but not saved.

Read off `customize\='s own record rather than a list kept here: a
setting changed through the menu, through `customize', or by hand with
`customize-set-variable' is in the same position either way."
  (let (out)
    (mapatoms (lambda (sym)
                (when (and (get sym 'customized-value)
                           (string-prefix-p "smear-cursor-" (symbol-name sym)))
                  (push sym out))))
    (sort out #'string-lessp)))

(defun smear-cursor-menu--save ()
  "Save the settings changed here, so the next Emacs starts with them.

Everything the menu changes lasts as long as the session until this is
run.  It writes through `customize', so the values land wherever the
rest of your customisation does: `custom-file', or the init file when
there is none."
  (let ((changed (smear-cursor-menu--unsaved)))
    (if (not changed)
        (message "Smear-cursor: nothing changed since the last save")
      (dolist (sym changed)
        (customize-save-variable sym (symbol-value sym)))
      (smear-cursor-menu--effects-forget)
      (message "Smear-cursor: saved %d setting%s to %s"
               (length changed) (if (cdr changed) "s" "")
               (or custom-file user-init-file "your customisation")))))

(defun smear-cursor-menu--ships-as (sym)
  "Return what SYM ships as, or nil when it is at it or has none."
  (let ((standard (car (get sym 'standard-value))))
    (when standard
      (let ((was (ignore-errors (eval standard t))))
        (unless (equal was (symbol-value sym)) was)))))

(defun smear-cursor-menu--number-prompt (sym text)
  "Return the prompt for reading a number for SYM, labelled TEXT.

Names the shipped value when the setting has moved away from it: a
number tuned for ten minutes is otherwise one you cannot undo without
going to `customize'.

The value the setting has now is not named here.  It is put in the
input line instead, where it can be edited: read as a default it came
with `read-number\='s own \"(default N)\", which is a second number in
the prompt under a word that in Emacs usually means the shipped one."
  (let ((ships (smear-cursor-menu--ships-as sym)))
    (if ships
        (format "%s (ships %s): " text ships)
      (format "%s: " text))))

(defun smear-cursor-menu--read-a-number (sym text)
  "Read a number for SYM, prompting with TEXT and offering what it has.

The value is in the input line rather than behind a default, so it can
be edited into the next one.  Anything that is not a number leaves the
setting where it was."
  (let ((typed (read-string (smear-cursor-menu--number-prompt sym text)
                            (number-to-string (symbol-value sym)))))
    (if (string-match-p "\\`[ \t]*-?[0-9]+\\(\\.[0-9]+\\)?[ \t]*\\'" typed)
        (string-to-number typed)
      (symbol-value sym))))

(defun smear-cursor-menu--read-number (sym text &optional most)
  "Prompt with TEXT and set SYM to the number entered.

With MOST, hold the answer between nought and MOST: a setting the
drawing can only honour so far is one you can put four hundred in and
see four, and the number on the line should be the number you get.

Say so when this Emacs has no such setting.  A session that loaded the
package before it was updated has the menu of one version and the
settings of another, and `void-variable' on its own does not say what
to do about that."
  (unless (boundp sym)
    (error "Smear-cursor: this Emacs has no %s; restart it to pick up the \
package as it is now" sym))
  (let ((got (smear-cursor-menu--read-a-number
              sym (if most (format "%s (0 to %d)" text most) text))))
    (smear-cursor-menu--put sym (if most (max 0 (min got most)) got))))

(defun smear-cursor-menu--toggle (sym)
  "Turn SYM on if it is off, and off if it is on.

Say so when this Emacs has no such setting, the way
`smear-cursor-menu--read-number' does: a session that loaded the
package before it was updated has the menu of one version and the
settings of another."
  (unless (boundp sym)
    (error "Smear-cursor: this Emacs has no %s; restart it to pick up the \
package as it is now" sym))
  (smear-cursor-menu--put sym (not (symbol-value sym))))

(defun smear-cursor-menu--read-symbol (sym text choices)
  "Prompt with TEXT and set SYM to a symbol from CHOICES."
  (smear-cursor-menu--put
   sym (intern (completing-read (format "%s: " text)
                                (mapcar #'symbol-name choices) nil t nil nil
                                (symbol-name (symbol-value sym))))))

(defcustom smear-cursor-menu-live-preview t
  "Play each effect as it comes under point while one is being chosen.

The effect is drawn in the window behind the minibuffer: over the
cursor for the effects that mark it, and over the line for the rest."
  :type 'boolean
  :group 'smear-cursor)

(defvar smear-cursor-menu--previewed nil
  "The candidate previewed last, so each one is shown once.")

(defconst smear-cursor-menu--preview-buffer-name "*smear-cursor preview*"
  "Name of the buffer the preview demonstrates in.")

(defconst smear-cursor-menu--preview-text
  (concat "Lorem ipsum dolor sit amet, consectetur adipiscing elit, "
          "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
  "The paragraph the preview types out, over and over.

Filler on purpose: the point is to watch the effect, not to read the
text.  It wraps over several lines, which is what gives the effects
that cover a region something of the right shape to cover.")

(defvar smear-cursor-menu--preview-timer nil
  "Timer stepping the demonstration, or nil when none runs.")

(defvar smear-cursor-menu--preview-column 0
  "How much of the sample text has been typed out so far.")

(defun smear-cursor-menu--candidate ()
  "Return the candidate the completion UI has settled on.

Which candidate is current is the UI\='s own idea: Vertico highlights
one, Icomplete puts it first, and plain completion has only the text
that has been typed."
  (cond ((and (bound-and-true-p vertico--index)
              (>= (symbol-value 'vertico--index) 0)
              (fboundp 'vertico--candidate))
         (funcall 'vertico--candidate))
        ((and (bound-and-true-p icomplete-mode)
              (fboundp 'completion-all-sorted-completions))
         (car (funcall 'completion-all-sorted-completions)))
        (t (minibuffer-contents-no-properties))))

(defun smear-cursor-menu--preview-window ()
  "Return a window showing the preview buffer, opening one if needed.

A side window at the foot of the frame.  The prompt keeps the focus,
and the effect is shown on a line of its own rather than drawn over
the buffer being worked in."
  (let ((buf (get-buffer-create smear-cursor-menu--preview-buffer-name)))
    (with-current-buffer buf
      (setq-local mode-line-format nil)
      (setq-local truncate-lines nil)
      (setq-local word-wrap t))
    (or (get-buffer-window buf)
        (display-buffer-in-side-window
         buf '((side . bottom) (window-height . 6))))))

(defun smear-cursor-menu--preview-write (win text)
  "Put TEXT in WIN\='s buffer as the whole sample line.

Edit with the modification hooks held off: the package hangs its own
typing highlight and delete effect on them, and the preview is meant
to show one effect at a time."
  (with-current-buffer (window-buffer win)
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t))
      (erase-buffer)
      (insert "\n  " text)
      (set-window-point win (point-max)))))

(defun smear-cursor-menu--preview-step (win name effect)
  "Show one step of EFFECT, called NAME, in WIN.

Type the sample out one character at a time for the effects that mark
the cursor, and show the whole line at once for the ones that cover a
region.  Start the sample over when it runs out."
  (let* ((point-shaped (eq (plist-get effect :shape) 'point))
         (text smear-cursor-menu--preview-text)
         (done (>= smear-cursor-menu--preview-column (length text))))
    (cond
     ((not point-shaped)
      (smear-cursor-menu--preview-write win text)
      (smear-cursor-menu--preview-play win name effect))
     (done
      (setq smear-cursor-menu--preview-column 0)
      (smear-cursor-menu--preview-write win ""))
     (t
      (setq smear-cursor-menu--preview-column
            (1+ smear-cursor-menu--preview-column))
      (smear-cursor-menu--preview-write
       win (substring text 0 smear-cursor-menu--preview-column))
      ;; The rectangle comes from the display, so the character has to
      ;; be on screen before it can be measured.
      (redisplay t)
      (smear-cursor-menu--preview-play win name effect)))))

(defun smear-cursor-menu--preview-rects (win effect)
  "Return the rectangles to show EFFECT over in WIN.

Measure in WIN\='s own buffer.  This runs from the minibuffer, and
`smear-cursor--pos-rect' checks the position against the bounds of
the current buffer, which would throw away every position past the
end of the prompt."
  (with-current-buffer (window-buffer win)
    (if (eq (plist-get effect :shape) 'point)
        (let ((r (smear-cursor--pos-rect win (max (point-min)
                                                  (1- (window-point win))))))
          (and r (list r)))
      (smear-cursor--region-rects win (point-min) (point-max)))))

(defun smear-cursor-menu--preview-play (win name effect)
  "Play effect NAME, described by EFFECT, in WIN."
  (let ((rects (and (window-live-p win)
                    (smear-cursor-menu--preview-rects win effect))))
    (when rects
      (smear-cursor--play-effect win name rects smear-cursor--track-occasion))))

(defun smear-cursor-menu--preview-pace (effect)
  "Return the seconds between steps when demonstrating EFFECT.

Type at a readable speed.  An effect over a region is played whole,
so leave it time to finish before it comes round again."
  (if (eq (plist-get effect :shape) 'point)
      0.1
    (+ (or (plist-get effect :duration) 0.3) 0.45)))

(defun smear-cursor-menu--preview-start (name)
  "Begin demonstrating effect NAME in the preview window."
  (smear-cursor-menu--preview-halt)
  (let ((effect (smear-cursor-effect name))
        (win (smear-cursor-menu--preview-window)))
    (when (and effect win)
      (setq smear-cursor-menu--preview-column 0)
      (smear-cursor-menu--preview-write win "")
      (setq smear-cursor-menu--preview-timer
            (run-at-time 0 (smear-cursor-menu--preview-pace effect)
                         (lambda ()
                           (smear-cursor--flourish
                             (smear-cursor-menu--preview-step
                              win name effect))))))))

(defun smear-cursor-menu--preview-halt ()
  "Stop stepping the demonstration, leaving the window alone."
  (when smear-cursor-menu--preview-timer
    (cancel-timer smear-cursor-menu--preview-timer)
    (setq smear-cursor-menu--preview-timer nil)))

(defun smear-cursor-menu--preview-stop ()
  "Stop the demonstration and take its window and buffer down."
  (smear-cursor-menu--preview-halt)
  (let ((buf (get-buffer smear-cursor-menu--preview-buffer-name)))
    (when buf
      (let ((win (get-buffer-window buf)))
        (when (and win (window-live-p win) (not (one-window-p)))
          (delete-window win)))
      (kill-buffer buf))))

(defun smear-cursor-menu--preview-candidate ()
  "Demonstrate the candidate under point, once for each one reached.

Run from `post-command-hook' in the minibuffer, which fires after
every key, so a candidate already showing is left alone."
  (when smear-cursor-menu-live-preview
    (let ((cand (smear-cursor-menu--candidate)))
      (unless (equal cand smear-cursor-menu--previewed)
        (setq smear-cursor-menu--previewed cand)
        (let ((name (and cand (intern-soft cand))))
          ;; Half-typed names and `off' reach here too, and neither
          ;; names an effect.
          (if (smear-cursor-effect name)
              (smear-cursor--flourish (smear-cursor-menu--preview-start name))
            (smear-cursor-menu--preview-halt)))))))

(defun smear-cursor-menu--pick-effect (prompt now)
  "Prompt with PROMPT for an effect, demonstrating each one under point.

NOW is the effect in use, offered as the default.  Return the name, or
nil when off is chosen.  See `smear-cursor-menu-live-preview'."
  (let* ((names (cons "off" (mapcar #'symbol-name
                                    (smear-cursor-menu--effect-names))))
         (pick (unwind-protect
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (setq smear-cursor-menu--previewed nil)
                         (add-hook 'post-command-hook
                                   #'smear-cursor-menu--preview-candidate nil t))
                     (completing-read prompt names nil t nil nil
                                      (if now (symbol-name now) "off")))
                 ;; Also on `keyboard-quit', which leaves through here.
                 (smear-cursor-menu--preview-stop))))
    (unless (equal pick "off") (intern pick))))

(defun smear-cursor-menu--read-effect (occasion)
  "Prompt for the effect to use for OCCASION, with off as an option."
  (smear-cursor-menu--set-occasion
   occasion (smear-cursor-menu--pick-effect
             (format "Effect for %s: " occasion)
             (cdr (assq occasion smear-cursor-effects)))))

(defun smear-cursor-menu--read-idle-effect ()
  "Prompt for the effect to play while the cursor is left alone."
  (smear-cursor-menu--put
   'smear-cursor-idle-effect
   (smear-cursor-menu--pick-effect "Effect when idle: "
                                   smear-cursor-idle-effect)))

(defun smear-cursor-menu--using-p (effect)
  "Return non-nil when EFFECT is in use anywhere.

Assigned to an occasion, chosen for the idle spell, or worn by the
cursor.  Half the menu is knobs for effects nobody has turned on, and
what is not in use has nothing to tune."
  (or (rassq effect smear-cursor-effects)
      (eq effect smear-cursor-idle-effect)
      (eq effect (bound-and-true-p smear-cursor-rest-effect))))

(defun smear-cursor-menu--using (&rest effects)
  "Return a predicate for whether any of EFFECTS is in use."
  (lambda () (seq-some #'smear-cursor-menu--using-p effects)))

(defun smear-cursor-menu--occasion-line (occasion text)
  "Return a menu line for OCCASION, labelled TEXT."
  (smear-cursor-menu--line
   text (smear-cursor-menu--show (cdr (assq occasion smear-cursor-effects)))
   nil (smear-cursor-menu--occasion-moved-p occasion)))


;;;; Suffixes

(transient-define-suffix smear-cursor-menu-style ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-trail-style "trail style"))
  (interactive)
  (smear-cursor-menu--read-symbol 'smear-cursor-trail-style "Trail style"
                                  (smear-cursor-menu--trail-names)))

(transient-define-suffix smear-cursor-menu-color ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-color "trail colour"
                           "style's own"))
  (interactive)
  (smear-cursor-menu--put
   'smear-cursor-color
   (let ((c (read-string "Trail colour (empty for the style's own): "
                         smear-cursor-color)))
     (unless (string-empty-p c) c))))

(transient-define-suffix smear-cursor-menu-effect-strength ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-effect-strength "effect strength"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-effect-strength
                                  "How loud effects are (1.0 as shipped)")
  ;; Preview the strength so the user need not wait for the next effect.
  (smear-cursor-flash (or (cdr (assq 'copy smear-cursor-effects))
                          'region-flash)))

(transient-define-suffix smear-cursor-menu-effect-color ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-effect-color "effect colour"
                           "effect's own"))
  (interactive)
  (smear-cursor-menu--put
   'smear-cursor-effect-color
   (let ((c (completing-read
             "Effect colour: "
             '("each effect's own" "trail") nil nil)))
     (cond ((equal c "trail") 'trail)
           ((equal c "each effect's own") nil)
           ((string-empty-p c) nil)
           (t c)))))

(transient-define-suffix smear-cursor-menu-fps ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe 'smear-cursor-fps "fps"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-fps "Frames a second"))

(transient-define-suffix smear-cursor-menu-min-distance ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-min-distance "least move"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-min-distance
                                  "Cells a move must cover"))

(transient-define-suffix smear-cursor-menu-long-jump-rows ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-long-jump-rows "long jump rows"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-long-jump-rows
                                  "Rows that count as a long jump"))

(transient-define-suffix smear-cursor-menu-long-jump-duration ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-long-jump-duration "long jump secs"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-long-jump-duration
                                  "Seconds a long jump's trail lasts"))

(transient-define-suffix smear-cursor-menu-backend ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-backend "backend"))
  (interactive)
  (smear-cursor-menu--read-symbol 'smear-cursor-backend "Backend"
                                  '(x11 cpu)))

(defun smear-cursor-menu--renderer-note ()
  "Return what to say about the renderer in use, or nil when nothing.

GL renders a picture of each frame and uploads it.  Across a network
that is the whole frame going over the wire thirty times a second, so
an effect that moves arrives as a handful of stills.  `render' sends
coordinates instead, which is why `auto' picks it for a display
reached over a network."
  (when (and (eq smear-cursor-x11-renderer 'gl)
             (fboundp 'smear-cursor-x11--local-display-p)
             (not (smear-cursor-x11--local-display-p (selected-frame))))
    "  --  gl sends a picture a frame over this connection"))

(transient-define-suffix smear-cursor-menu-renderer ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-x11-renderer "x11 renderer"))
  (interactive)
  (smear-cursor-menu--read-symbol 'smear-cursor-x11-renderer "Renderer"
                                  '(auto gl render)))

(transient-define-suffix smear-cursor-menu-pulse-rows ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-pulse-min-rows "pulse from rows"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-pulse-min-rows
                                  "Rows a jump must cover to pulse"))

(transient-define-suffix smear-cursor-menu-scan-passes ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-scan-passes "scan passes"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-scan-passes
                                  "Throws across the line (even ends home)"))

(transient-define-suffix smear-cursor-menu-scan-pace ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-scan-pace "scan pace"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-scan-pace
                                  "Seconds between one throw and the next"))

(transient-define-suffix smear-cursor-menu-typing-highlight ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-typing-highlight "typing highlight" "off"))
  (interactive)
  (smear-cursor-menu--toggle 'smear-cursor-typing-highlight))

(transient-define-suffix smear-cursor-menu-typing-highlight-strength ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-typing-highlight-strength "highlight strength"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-typing-highlight-strength
                                  "Peak opacity of the highlight"))

(transient-define-suffix smear-cursor-menu-typing-highlight-duration ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-typing-highlight-duration "highlight time"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-typing-highlight-duration
                                  "Seconds the highlight lasts"))

(transient-define-suffix smear-cursor-menu-pad-line-ends ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-pad-line-ends "pad line ends" "off"))
  (interactive)
  (smear-cursor-menu--toggle 'smear-cursor-pad-line-ends))

(transient-define-suffix smear-cursor-menu-prompt-settle ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-prompt-settle "prompt settle"
                           "draw every jump"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-prompt-settle
                                  "Seconds still before a trail draws"))

(transient-define-suffix smear-cursor-menu-while-prompting ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-while-prompting "while prompting" "off"))
  (interactive)
  (smear-cursor-menu--toggle 'smear-cursor-while-prompting))

(transient-define-suffix smear-cursor-menu-while-selecting ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-while-selecting "while selecting" "off"))
  (interactive)
  (smear-cursor-menu--toggle 'smear-cursor-while-selecting))

(transient-define-suffix smear-cursor-menu-customize ()
  :transient t
  :description (lambda ()
                 (smear-cursor-menu--line
                  "customize" (smear-cursor-menu--hint "every setting")))
  (interactive)
  (customize-group 'smear-cursor))

(transient-define-suffix smear-cursor-menu-pulse-strength ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-pulse-strength "pulse strength"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-pulse-strength
                                  "Peak opacity, as a fraction of the trail's"))

(transient-define-suffix smear-cursor-menu-pulse-flashes ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-pulse-flashes "pulse flashes"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-pulse-flashes
                                  "Times the line blinks (1 is one wash)"))

(transient-define-suffix smear-cursor-menu-pulse ()
  :transient t
  :description (lambda () (smear-cursor-menu--occasion-line 'pulse "pulse"))
  (interactive) (smear-cursor-menu--read-effect 'pulse))

(transient-define-suffix smear-cursor-menu-copy ()
  :transient t
  :description (lambda () (smear-cursor-menu--occasion-line 'copy "copy"))
  (interactive) (smear-cursor-menu--read-effect 'copy))

(transient-define-suffix smear-cursor-menu-delete ()
  :transient t
  :description (lambda () (smear-cursor-menu--occasion-line 'delete "delete"))
  (interactive) (smear-cursor-menu--read-effect 'delete))

(transient-define-suffix smear-cursor-menu-yank ()
  :transient t
  :description (lambda () (smear-cursor-menu--occasion-line 'yank "paste"))
  (interactive) (smear-cursor-menu--read-effect 'yank))

(transient-define-suffix smear-cursor-menu-newline ()
  :transient t
  :description (lambda () (smear-cursor-menu--occasion-line 'newline "return"))
  (interactive)
  (smear-cursor-menu--read-effect 'newline))

(transient-define-suffix smear-cursor-menu-insert ()
  :transient t
  :description (lambda () (smear-cursor-menu--occasion-line 'insert "typing"))
  (interactive) (smear-cursor-menu--read-effect 'insert))

(transient-define-suffix smear-cursor-menu-blink-duration ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-type-blink-duration "blink secs"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-type-blink-duration
                                  "Seconds a typing blink lasts"))

(transient-define-suffix smear-cursor-menu-blink-strength ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-type-blink-strength "blink strength"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-type-blink-strength
                                  "How solid, 0 to 1"))

(transient-define-suffix smear-cursor-menu-blink-radius ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-type-blink-radius "blink radius"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-type-blink-radius
                                  "Radius in pixels"))

(transient-define-suffix smear-cursor-menu-rest-effect ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-rest-effect "style" "off"))
  (interactive)
  (smear-cursor-menu--put
   'smear-cursor-rest-effect
   (smear-cursor-menu--pick-effect "Cursor glow: "
                                   smear-cursor-rest-effect)))

(transient-define-suffix smear-cursor-menu-rest-strength ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-rest-strength "strength"))
  (interactive)
  (smear-cursor-menu--read-glow 'smear-cursor-rest-strength
                                "Strength, as a multiple of the style's"))

(transient-define-suffix smear-cursor-menu-rest-duration ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-rest-duration "seconds"))
  (interactive)
  (smear-cursor-menu--read-glow 'smear-cursor-rest-duration
                                "Seconds one cycle takes"))

(transient-define-suffix smear-cursor-menu-rest-dip ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-rest-dip "fade"))
  (interactive)
  (smear-cursor-menu--read-glow 'smear-cursor-rest-dip
                                "How far it fades between cycles, 0 to 1"))

(transient-define-suffix smear-cursor-menu-rest-size ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-rest-size "size"))
  (interactive)
  (smear-cursor-menu--read-glow 'smear-cursor-rest-size
                                "Size, as a multiple of the style's rings"))

(transient-define-suffix smear-cursor-menu-idle-effect ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-idle-effect "when idle" "off"))
  (interactive)
  (smear-cursor-menu--read-idle-effect))

(transient-define-suffix smear-cursor-menu-idle-delay ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-idle-delay "wait"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-idle-delay
                                  "Seconds of quiet before it plays"))

(transient-define-suffix smear-cursor-menu-idle-while-prompting ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-idle-while-prompting
                           "at a prompt" "off"))
  (interactive)
  (smear-cursor-menu--toggle 'smear-cursor-idle-while-prompting))

(transient-define-suffix smear-cursor-menu-roam-columns ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-roam-columns "roams"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-roam-columns
                                  "Characters either side of the cursor"))

(transient-define-suffix smear-cursor-menu-pacman-ghosts ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe-of
                           'smear-cursor-pacman-ghosts "ghosts"
                           smear-cursor--pacman-most-ghosts "none"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-pacman-ghosts
                                  "Ghosts chasing Pacman"
                                  smear-cursor--pacman-most-ghosts))

(transient-define-suffix smear-cursor-menu-pacman-berserk ()
  :transient t
  :description
  (lambda ()
    (smear-cursor-menu--setting-line
     'smear-cursor-pacman-berserk "berserk words"
     (smear-cursor-menu--paint
      (if smear-cursor-pacman-berserk
          (string-join smear-cursor-pacman-berserk " ")
        "none")
      (if smear-cursor-pacman-berserk 'smear-cursor-menu-value
        'smear-cursor-menu-off))))
  (interactive)
  (smear-cursor-menu--put
   'smear-cursor-pacman-berserk
   (split-string (read-string "Words that set him off, space separated: "
                              (string-join smear-cursor-pacman-berserk " "))
                 " +" t)))

(transient-define-suffix smear-cursor-menu-pacman-power ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-pacman-power "power pellet" "off"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-pacman-power
                                  "Plays between power pellets, 0 for none"))

(defun smear-cursor-menu--lines-value (text)
  "Return the bolt height TEXT stands for, or nil when it stands for none.

The symbols `window' and `frame', or a number of text lines.  Anything
else, including a name half typed on the way to one of them, is
nothing yet."
  (cond ((equal text "window") 'window)
        ((equal text "frame") 'frame)
        ((string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)?\\'" (or text ""))
         (string-to-number text))))

(defun smear-cursor-menu--preview-lines ()
  "Draw the bolt at the height under point while one is being chosen.

Run from `post-command-hook' at the height prompt.  The height is
baked into the shape when a strike is built, so the panel is started
again to show it."
  (when smear-cursor-menu-live-preview
    (let ((text (smear-cursor-menu--candidate)))
      (unless (equal text smear-cursor-menu--previewed)
        (setq smear-cursor-menu--previewed text)
        (let ((height (smear-cursor-menu--lines-value text)))
          (when height
            (setq smear-cursor-lightning-lines height)
            (smear-cursor--flourish
              (smear-cursor-menu--preview-start 'lightning))))))))

(defun smear-cursor-menu--preview-glow (sym)
  "Set SYM to the number being typed and play the glow again.

Run from `post-command-hook\=' while the number is read.  A number for
something you are looking at is guesswork until you see it, and the
glow is on the screen while the menu is open, so it may as well
answer."
  (when smear-cursor-menu-live-preview
    (let ((text (minibuffer-contents)))
      (unless (equal text smear-cursor-menu--previewed)
        (setq smear-cursor-menu--previewed text)
        (when (string-match-p "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" text)
          (set sym (string-to-number text))
          (smear-cursor--rest-forget)
          (smear-cursor--rest-play))))))

(defun smear-cursor-menu--read-glow (sym text)
  "Prompt with TEXT for SYM, showing the glow as each value is typed.

The glow is turned on for the reading if it was off, because a preview
of something not on the screen shows nothing.  Everything is put back
on `keyboard-quit\=': the preview moves the setting as it goes."
  (unless (boundp sym)
    (error "Smear-cursor: this Emacs has no %s; restart it to pick up the \
package as it is now" sym))
  (let ((start (symbol-value sym))
        (was smear-cursor-rest-effect))
    (unwind-protect
        (condition-case nil
            (let ((got (progn
                         (unless smear-cursor-rest-effect
                           (setq smear-cursor-rest-effect 'cursor-rest)
                           (smear-cursor--rest-setup))
                         (minibuffer-with-setup-hook
                             (lambda ()
                               (setq smear-cursor-menu--previewed nil)
                               (add-hook 'post-command-hook
                                         (lambda ()
                                           (smear-cursor-menu--preview-glow sym))
                                         nil t))
                           (smear-cursor-menu--read-a-number sym text)))))
              (set sym start)          ; so the setter sees a change
              (smear-cursor-menu--put sym got))
          (quit (set sym start)))
      (setq smear-cursor-rest-effect was)
      (smear-cursor--rest-setup))))

(defun smear-cursor-menu--read-lightning-lines ()
  "Read how tall a bolt should be, drawing each height as it is typed.

Put the height back on `keyboard-quit': the preview moves the setting
as it goes, so quitting has to undo that."
  (let ((start smear-cursor-lightning-lines))
    (unwind-protect
        (condition-case nil
            (let ((pick (minibuffer-with-setup-hook
                            (lambda ()
                              (setq smear-cursor-menu--previewed nil)
                              (add-hook 'post-command-hook
                                        #'smear-cursor-menu--preview-lines nil t))
                          (completing-read
                           "Bolt height, in lines or `window' or `frame': "
                           '("window" "frame" "2" "3" "5" "10" "20")
                           nil nil))))
              (smear-cursor-menu--put
               'smear-cursor-lightning-lines
               (or (smear-cursor-menu--lines-value pick) start)))
          (quit (setq smear-cursor-lightning-lines start)))
      (smear-cursor-menu--preview-stop))))

(transient-define-suffix smear-cursor-menu-lightning-lines ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-lightning-lines "bolt height"))
  (interactive)
  (smear-cursor-menu--read-lightning-lines))

(transient-define-suffix smear-cursor-menu-lightning-noise ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-lightning-noise "bolt noise"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-lightning-noise
                                  "How far it wanders, as a share of its height"))

(transient-define-suffix smear-cursor-menu-lightning-vary ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-lightning-vary "reshape each play"))
  (interactive)
  (smear-cursor-menu--toggle 'smear-cursor-lightning-vary))

(defconst smear-cursor-menu--seed-keys
  '((?n . 1) (?\s . 1) (?+ . 1) (?= . 1) (?p . -1) (?- . -1))
  "Keys that step the noise seed, and how far each moves it.")

(defun smear-cursor-menu--seed-step (key)
  "Return how far KEY moves the noise seed, or nil when it does not."
  (cdr (assq key smear-cursor-menu--seed-keys)))

(transient-define-suffix smear-cursor-menu-plasma-arcs ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-plasma-arcs "sparks"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-plasma-arcs
                                  "How many sparks, 1 to 7"))

(transient-define-suffix smear-cursor-menu-plasma-noise ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-plasma-noise "spark bend"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-plasma-noise
                                  "How far they bend, as a share of reach"))

(transient-define-suffix smear-cursor-menu-fire-embers ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-fire-embers "embers"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-fire-embers
                                  "How many embers, 1 to 7"))

(transient-define-suffix smear-cursor-menu-fire-height ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-fire-height "flame height"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-fire-height
                                  "How far they reach, in text lines"))

(defconst smear-cursor-menu--vary-for
  '((lightning . smear-cursor-lightning-vary)
    (plasma    . smear-cursor-plasma-vary)
    (fire      . smear-cursor-fire-vary))
  "The setting that gives each effect a new shape on every play.")

(defun smear-cursor-menu--varies-p (name)
  "Return non-nil when effect NAME takes a new shape on every play."
  (let ((sym (cdr (assq name smear-cursor-menu--vary-for))))
    (and sym (symbol-value sym))))

(defun smear-cursor-menu--roll-noise ()
  "Step through noise seeds with the effect playing in the panel.

Show the effect typing is set to, or the typing blink when it is set
to none, and draw it again for each seed.  RET keeps the seed reached
and \`C-g' puts back the one this started from.

Hold the shape still while rolling.  Lightning, sparks and fire take a
new shape on every play by default, and a seed cannot be judged
against something that reshuffles anyway."
  (let* ((start smear-cursor-noise-seed)
         (name (or (cdr (assq 'insert smear-cursor-effects)) 'type-blink))
         (varies (smear-cursor-menu--varies-p name)))
    (unwind-protect
        (cl-progv (mapcar #'cdr smear-cursor-menu--vary-for)
            (make-list (length smear-cursor-menu--vary-for) nil)
          (smear-cursor-menu--preview-start name)
          (catch 'done
            (while t
              (let* ((key (read-key
                           (format (concat "noise seed %d:  n next,  p previous,"
                                           "  RET keep,  C-g cancel")
                                   smear-cursor-noise-seed)))
                     (step (smear-cursor-menu--seed-step key)))
                (cond
                 (step (setq smear-cursor-noise-seed
                             (+ smear-cursor-noise-seed step))
                       (smear-cursor-menu--preview-start name))
                 ((eq key ?\C-g)
                  (setq smear-cursor-noise-seed start)
                  (throw 'done nil))
                 (t
                  ;; The seeds tried on the way here were only tried;
                  ;; this is the one being kept, so store it properly.
                  (smear-cursor-menu--put 'smear-cursor-noise-seed
                                          smear-cursor-noise-seed)
                  (when varies
                    (message (concat "Seed %d kept.  %s takes a new shape each"
                                     " play, so turn that off to keep this one")
                             smear-cursor-noise-seed name))
                  (throw 'done nil)))))))
      (smear-cursor-menu--preview-stop))))
(transient-define-suffix smear-cursor-menu-noise ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-noise-seed "noise seed"))
  (interactive)
  (smear-cursor-menu--roll-noise))

(defun smear-cursor-menu--commands-line ()
  "Return the menu line for the commands that pulse."
  (smear-cursor-menu--setting-line
   'smear-cursor-pulse-commands "pulse after"
   (format "%d command(s)" (length smear-cursor-pulse-commands))))

(defun smear-cursor-menu--trace-line ()
  "Return the menu line for the trace file."
  (smear-cursor-menu--setting-line
   'smear-cursor-trace "trace to file"
   (smear-cursor-menu--paint (or smear-cursor-trace "off")
                             (if smear-cursor-trace 'smear-cursor-menu-value
                               'smear-cursor-menu-off))))

(transient-define-suffix smear-cursor-menu-pulse-commands ()
  :transient t
  :description
  (lambda () (smear-cursor-menu--commands-line))
  (interactive)
  (customize-variable 'smear-cursor-pulse-commands))

(transient-define-suffix smear-cursor-menu-delete-min ()
  :transient t
  :description (lambda () (smear-cursor-menu--describe
                           'smear-cursor-delete-min-chars "delete from chars"))
  (interactive)
  (smear-cursor-menu--read-number 'smear-cursor-delete-min-chars
                                  "Fewest characters a deletion must remove"))

(transient-define-suffix smear-cursor-menu-preview ()
  :transient t
  :description (lambda () (smear-cursor-menu--command "see an effect now"))
  (interactive)
  (call-interactively #'smear-cursor-flash))

(transient-define-suffix smear-cursor-menu-trace ()
  :transient t
  :description
  (lambda () (smear-cursor-menu--trace-line))
  (interactive)
  (smear-cursor-menu--put
   'smear-cursor-trace
   (unless smear-cursor-trace
     (read-file-name "Trace decisions to: " temporary-file-directory
                     nil nil "smear-trace.txt"))))

(defun smear-cursor-menu--level ()
  "Return the level the menu is showing at, or nil when it is not open."
  (and (bound-and-true-p transient--prefix)
       (ignore-errors (oref transient--prefix level))))

(defun smear-cursor-menu--level-text ()
  "Return the level to show beside the key that changes it."
  (let ((n (smear-cursor-menu--level)))
    (if n (format "%d of 7" n) "4 to 7")))

(transient-define-suffix smear-cursor-menu-report ()
  :description (lambda () (smear-cursor-menu--line
                           "report" (smear-cursor-menu--hint "last smear")))
  (interactive)
  (call-interactively #'smear-cursor-report))

(transient-define-suffix smear-cursor-menu-save ()
  :transient t
  :description
  (lambda ()
    (let ((n (length (smear-cursor-menu--unsaved))))
      (smear-cursor-menu--line
       "save settings"
       (if (= n 0)
           (smear-cursor-menu--paint "saved" 'smear-cursor-menu-off)
         (smear-cursor-menu--paint (format "%d unsaved" n)
                                   'smear-cursor-menu-alert)))))
  (interactive)
  (smear-cursor-menu--save))

(transient-define-suffix smear-cursor-menu-toggle ()
  :transient t
  :description (lambda () (smear-cursor-menu--command
                           (if smear-cursor-mode "turn off" "turn on")))
  (interactive)
  (smear-cursor-mode (if smear-cursor-mode -1 1)))

(transient-define-suffix smear-cursor-menu-ignored ()
  :transient t
  :description
  (lambda ()
    (let ((n (length (smear-cursor-menu--ignored))))
      (smear-cursor-menu--line
       "ignored settings"
       (if (zerop n)
           (smear-cursor-menu--paint "none" 'smear-cursor-menu-off)
         (smear-cursor-menu--paint (format "%d -- press to list" n)
                                   'smear-cursor-menu-alert)))))
  (interactive)
  (let ((ignored (smear-cursor-menu--ignored)))
    (if (not ignored)
        (message "smear-cursor: every setting you have changed applies here")
      (with-help-window "*smear-cursor settings*"
        (princ (format "These are set, and the `%s' backend does not draw them.\n"
                       smear-cursor-backend))
        (princ "Nothing is wrong; they simply belong to another backend.\n\n")
        (dolist (sym ignored)
          (princ (format "  %-38s = %S   (needs `%s')\n"
                         sym (symbol-value sym)
                         (cdr (assq sym smear-cursor-menu--needs)))))))))

(transient-define-suffix smear-cursor-menu-watch ()
  :description (lambda () (smear-cursor-menu--line
                           "watch overlay"
                           (smear-cursor-menu--hint "shell command")))
  (interactive)
  (message "%s"
           (concat "Run beside this session, then move the cursor:  "
                   (expand-file-name "x11/smear-cursor-x11-watch"
                                     (file-name-directory
                                      (or (locate-library "smear-cursor") "")))
                   " 60 20")))


;;;; The menu

;;;###autoload (autoload 'smear-cursor-menu "smear-cursor-menu" nil t)
(defclass smear-cursor-menu-prefix (transient-prefix) ()
  "The menu itself, so that transient\='s own save keys reach the settings.

A transient\='s value is normally the arguments its infixes build, and
`C-x C-s\=' writes that.  This menu has no arguments: each key sets an
Emacs option there and then.  Without these methods `C-x C-s\=' would
save an empty value and look like it had kept something.")

(cl-defmethod transient-save-value ((_obj smear-cursor-menu-prefix))
  "Keep the settings changed here, rather than an empty prefix value."
  (smear-cursor-menu--save))

(cl-defmethod transient-set-value ((_obj smear-cursor-menu-prefix))
  "Say that there is nothing to set: every key here has set it already."
  (message "Smear-cursor: already set for this session; C-x C-s keeps it"))

(transient-define-prefix smear-cursor-menu ()
  "Show a menu for viewing and changing smear-cursor settings.

Everyday settings are in front.  The rest -- the ones set once and
forgotten -- are a level up: press \\`C-x l\\' and raise it to 5, which
transient remembers for next time.

Nothing changed here outlasts the session until it is saved, which is
what the last column counts and what \\`W\\' does -- as does transient\='s
own \\`C-x C-s\\', which for this menu saves the settings rather than a
prefix value it does not have."
  :class 'smear-cursor-menu-prefix
  ;; Which columns there are depends on what is in use, and this menu
  ;; is where that is changed: asked once at setup, `:if' left the
  ;; settings of an effect hidden until the menu was closed and opened
  ;; again.
  :refresh-suffixes t
  ;; Every row pads its keys to two columns, the widest key in the
  ;; menu.  Transient pads keys only for a group that asks, and then
  ;; only to that group's widest key, so the one-letter groups sat a
  ;; character left of the two-letter ones and no column of values
  ;; lined up.  A column asking for `t' would pad to its own widest.
  [:description smear-cursor-menu--state :pad-keys 2
   ["Trail"
    ("t" smear-cursor-menu-style)
    ("k" smear-cursor-menu-color)
    ("f" smear-cursor-menu-fps)
    ("d" smear-cursor-menu-min-distance :level 5)
    ("j" smear-cursor-menu-long-jump-rows :level 5)
    ("J" smear-cursor-menu-long-jump-duration :level 5)
    ("u" smear-cursor-menu-pad-line-ends :level 5)
    ("e" smear-cursor-menu-while-selecting :level 5)
    ("h" smear-cursor-menu-while-prompting :level 5)
    ("Q" smear-cursor-menu-prompt-settle :level 5)]
   ["What plays"
    ("p" smear-cursor-menu-pulse)
    ("c" smear-cursor-menu-copy)
    ("x" smear-cursor-menu-delete)
    ("y" smear-cursor-menu-yank)
    ("i" smear-cursor-menu-insert)
    ("E" smear-cursor-menu-newline)
    ("C" smear-cursor-menu-pulse-commands :level 5)
    ("X" smear-cursor-menu-delete-min :level 5
     :if (lambda () (cdr (assq 'delete smear-cursor-effects))))]]
  [:pad-keys 2
   ["Effects"
    ("s" smear-cursor-menu-effect-strength)
    ("K" smear-cursor-menu-effect-color)
    ("v" smear-cursor-menu-preview)
    ("P" smear-cursor-menu-pulse-rows :level 5
     :if (lambda () (smear-cursor-menu--using-p 'line-pulse)))
    ("H" smear-cursor-menu-pulse-flashes :level 5
     :if (lambda () (smear-cursor-menu--using-p 'line-pulse)))
    ("G" smear-cursor-menu-pulse-strength :level 5
     :if (lambda () (smear-cursor-menu--using-p 'line-pulse)))
    ("N" smear-cursor-menu-noise :level 5)]
   ["When idle"
    ("nb" smear-cursor-menu-idle-effect)
    ("nd" smear-cursor-menu-idle-delay)
    ("np" smear-cursor-menu-idle-while-prompting :level 5)
    ("nc" smear-cursor-menu-roam-columns :level 5
     :if (lambda () (funcall (smear-cursor-menu--using
                              'pacman 'janitor 'ufo 'grinch))))
    ("ng" smear-cursor-menu-pacman-ghosts :level 5
     :if (lambda () (smear-cursor-menu--using-p 'pacman)))
    ("nu" smear-cursor-menu-pacman-power :level 5
     :if (lambda () (smear-cursor-menu--using-p 'pacman)))
    ("nw" smear-cursor-menu-pacman-berserk :level 5
     :if (lambda () (smear-cursor-menu--using-p 'pacman)))]]
  [:pad-keys 2
   ["Backend"
    ("B" smear-cursor-menu-backend)
    ("R" smear-cursor-menu-renderer)]
   ["While typing"
    ("mh" smear-cursor-menu-typing-highlight)
    ("ms" smear-cursor-menu-typing-highlight-strength :level 5)
    ("md" smear-cursor-menu-typing-highlight-duration :level 5)
    ("bs" smear-cursor-menu-blink-strength :level 5
     :if (lambda () (smear-cursor-menu--using-p 'type-blink)))
    ("bd" smear-cursor-menu-blink-duration :level 5
     :if (lambda () (smear-cursor-menu--using-p 'type-blink)))
    ("br" smear-cursor-menu-blink-radius :level 5
     :if (lambda () (smear-cursor-menu--using-p 'type-blink)))]]
  [:pad-keys 2
   [:level 5 :if (lambda () (smear-cursor-menu--using-p 'lightning))
    "Lightning"
    ("lh" smear-cursor-menu-lightning-lines)
    ("ln" smear-cursor-menu-lightning-noise)
    ("lv" smear-cursor-menu-lightning-vary)]
   ;; Its own section rather than a line in the trail's: the glow is
   ;; drawn on the cursor all the time, which is a different thing from
   ;; what the trail does when the cursor moves.  The switch stays in
   ;; front, or there would be no way to turn on the thing whose
   ;; settings are hidden until it is on.
   ["Cursor glow"
    ("a" smear-cursor-menu-rest-effect)
    ("oi" smear-cursor-menu-rest-strength :level 5
     :if (lambda () (and (bound-and-true-p smear-cursor-rest-effect) t)))
    ("ob" smear-cursor-menu-rest-duration :level 5
     :if (lambda () (and (bound-and-true-p smear-cursor-rest-effect) t)))
    ("od" smear-cursor-menu-rest-dip :level 5
     :if (lambda () (and (bound-and-true-p smear-cursor-rest-effect) t)))
    ("os" smear-cursor-menu-rest-size :level 5
     :if (lambda () (and (bound-and-true-p smear-cursor-rest-effect) t)))]
   [:level 5 :if (lambda () (funcall (smear-cursor-menu--using 'plasma 'fire)))
    "Sparks and fire"
    ("Sa" smear-cursor-menu-plasma-arcs)
    ("Sn" smear-cursor-menu-plasma-noise)
    ("Fa" smear-cursor-menu-fire-embers)
    ("Fh" smear-cursor-menu-fire-height)]
   [:level 5 :if (lambda () (smear-cursor-menu--using-p 'line-scan))
    "Line scan"
    ("zp" smear-cursor-menu-scan-passes)
    ("zs" smear-cursor-menu-scan-pace)]]
  [:pad-keys 2
   ["Diagnostics"
    ("r" smear-cursor-menu-report)
    ("A" smear-cursor-menu-customize)
    ("T" smear-cursor-menu-trace :level 5)
    ("w" smear-cursor-menu-watch :level 5)
    ("!" smear-cursor-menu-ignored :level 5)]
   ["Menu"
    ;; transient's own `C-x l', on a key of its own: half this menu is
    ;; held back by level, and nothing on screen would otherwise say so
    ("L" transient-set-level
     :description (lambda () (smear-cursor-menu--line
                              "detail level"
                              (smear-cursor-menu--hint
                               (smear-cursor-menu--level-text)))))
    ("W" smear-cursor-menu-save)
    ;; Not `g': that reverts or refreshes a buffer everywhere else in
    ;; Emacs, and turning the mode off is the last thing anybody wants
    ;; from the key they press to redraw one.
    ("M" smear-cursor-menu-toggle)
    ("q" transient-quit-one
     :description (lambda () (smear-cursor-menu--command "quit")))]])

(provide 'smear-cursor-menu)
;;; smear-cursor-menu.el ends here
