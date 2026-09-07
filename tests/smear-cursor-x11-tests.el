;;; smear-cursor-x11-tests.el --- ERT tests for the x11 backend -*- lexical-binding: t -*-

;; Two kinds of test here.  The arithmetic runs anywhere, including
;; batch.  The rest needs a real X frame to draw on, and batch mode
;; cannot make one ("Unknown terminal type"), so those skip unless
;; run from a graphical Emacs.  `make test' runs the first set,
;; `make test-x' both.

(require 'ert)
(setq load-prefer-newer t)
(require 'smear-cursor-x11)
;; the styles and their layer vectors live in the main file
(require 'smear-cursor)

(defun smear-cursor-x11-tests--graphical-p ()
  (and (display-graphic-p) (eq (framep (selected-frame)) 'x)))

(defun smear-cursor-x11-tests--module-p ()
  "Non-nil when the module is built and loadable."
  (ignore-errors (smear-cursor-x11-load)))

;;;; Coordinates

(ert-deftest smear-cursor-x11-test-window-pixels-become-frame-pixels ()
  ;; GIVEN a point in a window, as `pos-visible-in-window-p' reports it
  ;; WHEN it is put into the frame's own coordinates
  ;; THEN X takes the text area's left edge and Y takes the WINDOW's top.
  ;;
  ;;      X is relative to the text area: column 0 gives 0 regardless of
  ;;      fringe or margin width.  Y is relative to the window top: the
  ;;      measured 43 is the combined height of the header and tab lines.
  ;;      A shared origin would put the trail two lines too high.  Adding
  ;;      the text area's top to Y counts those heights twice.
  (cl-letf (((symbol-function 'window-pixel-edges)
             (lambda (&rest _) '(1 37 837 623)))
            ((symbol-function 'window-inside-pixel-edges)
             (lambda (&rest _) '(29 80 829 623))))   ; header + tab line
    (should (equal (smear-cursor-x11--frame-xy nil 0 0) '(29 . 37)))
    (should (equal (smear-cursor-x11--frame-xy nil 10 20) '(39 . 57)))))

(ert-deftest smear-cursor-x11-test-furniture-is-not-counted-twice ()
  ;; GIVEN a window carrying a header line and a tab line
  ;; WHEN point sits on the first text line, which `pos-visible' puts at
  ;;      the foot of that furniture rather than at zero
  ;; THEN the frame coordinate lands at the text area's top, not below it
  (cl-letf (((symbol-function 'window-pixel-edges)
             (lambda (&rest _) '(1 37 837 623)))
            ((symbol-function 'window-inside-pixel-edges)
             (lambda (&rest _) '(29 80 829 623))))
    ;; 43 is what pos-visible reports for the first text line here
    (should (equal (cdr (smear-cursor-x11--frame-xy nil 0 43)) 80))))

(ert-deftest smear-cursor-x11-test-corners-move-as-one ()
  ;; GIVEN the eight numbers of a quad in window pixels
  ;; WHEN they are put into frame coordinates
  ;; THEN every corner shifts by the same offset and the shape is kept
  (cl-letf (((symbol-function 'window-pixel-edges)
             (lambda (&rest _) '(1 35 837 749)))
            ((symbol-function 'window-inside-pixel-edges)
             (lambda (&rest _) '(29 35 829 749))))
    (let ((out (smear-cursor-x11--corners-in-frame
                nil [0.0 0.0 10.0 0.0 10.0 20.0 0.0 20.0])))
      (should (equal out [29.0 35.0 39.0 35.0 39.0 55.0 29.0 55.0]))
      ;; the quad is the same size it was
      (should (= (- (aref out 2) (aref out 0)) 10.0))
      (should (= (- (aref out 5) (aref out 1)) 20.0)))))

(ert-deftest smear-cursor-x11-test-corners-must-be-eight ()
  ;; GIVEN a corner vector of the wrong length
  ;; WHEN it is converted
  ;; THEN it is refused rather than half-read.  A quad with a missing
  ;;      corner would draw somewhere arbitrary, and finding that from
  ;;      the picture is far more work than an error here.
  (cl-letf (((symbol-function 'window-pixel-edges)
             (lambda (&rest _) '(0 0 100 100)))
            ((symbol-function 'window-inside-pixel-edges)
             (lambda (&rest _) '(0 0 100 100))))
    (should-error (smear-cursor-x11--corners-in-frame nil [0.0 0.0 1.0 1.0])
                  :type 'wrong-length-argument)))

(ert-deftest smear-cursor-x11-test-head-of-a-rect-is-its-middle ()
  ;; GIVEN the rect the cursor is chasing, in window pixels
  ;; WHEN the head is taken
  ;; THEN it is the centre of that rect, in frame coordinates, where
  ;;      the trail is brightest and where the gradient starts
  (cl-letf (((symbol-function 'window-pixel-edges)
             (lambda (&rest _) '(1 35 837 749)))
            ((symbol-function 'window-inside-pixel-edges)
             (lambda (&rest _) '(29 35 829 749))))
    (should (equal (smear-cursor-x11--head-of nil [10.0 20.0 8.0 18.0])
                   [43.0 64.0]))))

;;;; Availability

(ert-deftest smear-cursor-x11-test-unavailable-without-the-module ()
  ;; GIVEN an Emacs where the module did not load
  ;; WHEN availability is asked
  ;; THEN the answer is no, and nothing is signalled.  This is one
  ;;      backend of several, and the others still work.
  (cl-letf (((symbol-function 'fboundp) (lambda (_) nil)))
    (should-not (smear-cursor-x11-available-p))))

(ert-deftest smear-cursor-x11-test-unavailable-off-x ()
  ;; GIVEN a frame that is not an X frame: a terminal, pgtk or macOS
  ;; WHEN availability is asked
  ;; THEN no.  Wayland forbids reading another surface or placing an
  ;;      override-redirect window.
  (cl-letf (((symbol-function 'framep) (lambda (&rest _) 'pgtk)))
    (should-not (smear-cursor-x11-available-p))))

;;;; The module itself, on a real frame

(ert-deftest smear-cursor-x11-test-opens-a-stage-on-this-frame ()
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (skip-unless (smear-cursor-x11-tests--module-p))
  ;; GIVEN this Emacs frame
  ;; WHEN a stage is opened on it
  ;; THEN there is one, and it has no complaint
  (let ((stage (smear-cursor-x11-stage (selected-frame))))
    (should stage)
    (should-not (smear-cursor-x11--trouble stage))
    (should (stringp (smear-cursor-x11--describe stage)))))

(ert-deftest smear-cursor-x11-test-a-bogus-window-is-refused ()
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (skip-unless (smear-cursor-x11-tests--module-p))
  ;; GIVEN a window id that is not on this display
  ;; WHEN a stage is opened for it
  ;; THEN it says so, rather than drawing somewhere unrelated
  (let ((stage (smear-cursor-x11--open "0x7fffffff" nil)))
    (should stage)
    (should (stringp (smear-cursor-x11--trouble stage)))
    (smear-cursor-x11--close stage)))

(ert-deftest smear-cursor-x11-test-the-trail-stands-on-text ()
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (skip-unless (smear-cursor-x11-tests--module-p))
  ;; GIVEN a frame with something written in it
  ;; WHEN a frame of trail is drawn over it
  ;; THEN what is underneath is the buffer, not a blank rectangle.
  ;;
  ;;      Saving the background and putting it back captures black.
  ;;      Unmapping only damages what is beneath, and the repaint
  ;;      belongs to the other client.  Comparing the screen before a
  ;;      run against after does not catch this: the buffer's own churn
  ;;      is larger than the fault.
  (let ((stage (smear-cursor-x11-stage (selected-frame))))
    (skip-unless (and stage (not (smear-cursor-x11--trouble stage))))
    (insert (mapconcat (lambda (_) "the quick brown fox jumps over it\n")
                       (number-sequence 1 40) ""))
    (redisplay t)
    (smear-cursor-x11--begin stage)
    (smear-cursor-x11--frame-begin stage 40 40 420 220)
    (smear-cursor-x11--draw
     stage (vector 0 [60.0 60.0 400.0 60.0 400.0 200.0 60.0 200.0]
                   [380.0 70.0] [154.0 184.0 232.0] 0 0 0 [0.0 0.7 1.0 0.05]))
    (smear-cursor-x11--frame-end stage)
    (unwind-protect
        (should (eq t (smear-cursor-x11--background-has-detail-p stage)))
      (smear-cursor-x11--end stage))))

(ert-deftest smear-cursor-x11-test-numbers-may-be-integers ()
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (skip-unless (smear-cursor-x11-tests--module-p))
  ;; GIVEN a colour written as the rest of the package writes one,
  ;;       [154 184 232] as integers, and corners that land exactly on
  ;;       pixels, which Lisp hands over as integers too
  ;; WHEN a frame is drawn
  ;; THEN it draws, rather than signalling `wrong-type-argument floatp'.
  ;;
  ;;      The tests in smear-cursor cannot catch this.  They stub the
  ;;      module, and the stub accepts what the real one refuses.
  (let ((stage (smear-cursor-x11-stage (selected-frame))))
    (skip-unless (and stage (not (smear-cursor-x11--trouble stage))))
    (smear-cursor-x11--begin stage)
    (smear-cursor-x11--frame-begin stage 40 40 420 220)
    (unwind-protect
        (should (smear-cursor-x11--draw
                 stage (vector 0 [60 60 400 60 400 200 60 200]
                               [380 70] [154 184 232] 0 0 0 [0 0.7 1 0.05])))
      (smear-cursor-x11--end stage))))

(ert-deftest smear-cursor-x11-test-a-blurred-layer-has-a-soft-edge ()
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (skip-unless (smear-cursor-x11-tests--module-p))
  ;; GIVEN a display that reports the convolution filter
  ;; WHEN a layer is drawn with a blur radius
  ;; THEN it draws.
  ;;
  ;;      The blur is a filter set on the coverage mask, which is the
  ;;      part that could quietly do nothing: a server may list the
  ;;      filter and still ignore it on a mask.  Whether it *looks*
  ;;      blurred is for the eye; that it does not fail is for here.
  (let ((stage (smear-cursor-x11-stage (selected-frame))))
    (skip-unless (and stage (not (smear-cursor-x11--trouble stage))))
    (should (memq (smear-cursor-x11--can-blur-p stage) '(nil t)))
    (smear-cursor-x11--begin stage)
    (smear-cursor-x11--frame-begin stage 40 40 420 220)
    (unwind-protect
        (should (smear-cursor-x11--draw
                 stage (vector 0 [60 60 400 60 400 200 60 200]
                               [380 70] [154 184 232] 4 5 0 [0 0.5 1 0.05])))
      (smear-cursor-x11--end stage))))

(ert-deftest smear-cursor-x11-test-a-radial-layer-draws ()
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (skip-unless (smear-cursor-x11-tests--module-p))
  ;; GIVEN a round glow rather than a quad
  ;; WHEN it is drawn at the head
  ;; THEN it draws.  A radial gradient needs no coverage mask, since
  ;;      its own alpha falls to nothing at the rim.
  (let ((stage (smear-cursor-x11-stage (selected-frame))))
    (skip-unless (and stage (not (smear-cursor-x11--trouble stage))))
    (smear-cursor-x11--begin stage)
    (smear-cursor-x11--frame-begin stage 40 40 420 220)
    (unwind-protect
        (should (smear-cursor-x11--draw
                 stage (vector 1 [0 0 0 0 0 0 0 0]
                               [200 200] [154 184 232] 0 0 24 [0 0.6 1 0.0])))
      (smear-cursor-x11--end stage))))

(ert-deftest smear-cursor-x11-test-a-local-display-is-told-from-a-forward ()
  ;; GIVEN the several ways a display can be named
  ;; WHEN each is judged local or not
  ;; THEN a bare display number is local and anything naming a host is
  ;;      not.  `localhost:10.0' reads as local but is an SSH forward,
  ;;      which is the slowest case.
  (dolist (case '((":0" . t) ("unix:0" . t) (":1.0" . t)
                  ("localhost:10.0" . nil) ("192.168.4.33:0.0" . nil)))
    (cl-letf (((symbol-function 'frame-parameter) (lambda (&rest _) (car case))))
      (should (eq (and (smear-cursor-x11--local-display-p nil) t) (cdr case))))))

(ert-deftest smear-cursor-x11-test-gl-declines-in-words ()
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (skip-unless (smear-cursor-x11-tests--module-p))
  ;; GIVEN a stage asked for the GL renderer
  ;; WHEN GL cannot come up
  ;; THEN the answer says why, rather than silently drawing with the
  ;;      other renderer and leaving the caller to wonder
  (let ((stage (smear-cursor-x11-stage (selected-frame))))
    (skip-unless (and stage (not (smear-cursor-x11--trouble stage))))
    (let ((got (smear-cursor-x11--renderer stage t)))
      (should (memq (if (stringp got) 'explained got) '(gl render explained)))
      ;; and asking for RENDER always gets RENDER
      (should (eq 'render (smear-cursor-x11--renderer stage nil))))))

(ert-deftest smear-cursor-x11-test-grow-means-the-same-in-both-renderers ()
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (skip-unless (smear-cursor-x11-tests--module-p))
  ;; GIVEN a slanted trail narrowed by an inset
  ;; WHEN it is drawn by each renderer
  ;; THEN both draw about as much of it.
  ;;
  ;;      GL's grow is an offset on a signed distance, so it is
  ;;      perpendicular.  RENDER offsets along the edge normals.  At
  ;;      `:grow -7' on a quad whose perpendicular half-width was about
  ;;      seven, an implementation pushing corners away from the
  ;;      centroid drew 4502 pixels where GL drew none.  GL is right.
  (let ((stage (smear-cursor-x11-stage (selected-frame))))
    (skip-unless (and stage (not (smear-cursor-x11--trouble stage))))
    (skip-unless (eq 'gl (smear-cursor-x11--renderer stage t)))
    (let* ((layer '(:shape quad :grow -2 :alpha 0.9))
           (corners [80.0 80.0 100.0 80.0 380.0 300.0 360.0 300.0])
           (v (smear-cursor--layer-vector layer corners [370.0 290.0]
                                          [154 184 232]))
           (counts nil))
      (smear-cursor-x11--begin stage)
      (dolist (gl '(nil t))
        (smear-cursor-x11--renderer stage gl)
        (smear-cursor-x11--frame-begin stage 60 60 360 280)
        (smear-cursor-x11--draw stage v)
        (smear-cursor-x11--frame-end stage)
        (push (aref (smear-cursor-x11--sample stage 60 60 360 280) 2) counts))
      (smear-cursor-x11--end stage)
      (smear-cursor-x11--renderer stage nil)
      (cl-destructuring-bind (gl render) counts
        (should (> render 500))
        (should (> gl 500))
        ;; within half of each other: not pixel-identical renderers,
        ;; but the same shape
        (should (< (max gl render) (* 2 (min gl render))))))))

(ert-deftest smear-cursor-x11-test-pos-rect-agrees-with-point-rect ()
  ;; GIVEN point somewhere in a real window
  ;; WHEN the rect is asked for by position and again for point
  ;; THEN they are the same rect.
  ;;
  ;;      `smear-cursor--pos-rect' exists because effects mark spans of
  ;;      text, not just point.  It has to agree with the one the trail
  ;;      uses, or a region is marked a row off the text it covers.
  ;;      `pos-visible-in-window-p' with PARTIALLY non-nil returns the
  ;;      six-element answer carrying the row only when the glyph is
  ;;      partly hidden.  A fully visible glyph comes back as two
  ;;      elements, which yields an empty region.
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (with-temp-buffer
    (dotimes (i 40) (insert (format "line %d %s\n" i (make-string 40 ?z))))
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (forward-line 3)
    (forward-char 5)
    (redisplay t)
    (let ((win (selected-window)))
      (should (equal (smear-cursor--point-rect win)
                     (smear-cursor--pos-rect win (point)))))))

(ert-deftest smear-cursor-x11-test-a-region-over-three-lines-is-three-rects ()
  ;; GIVEN a region running from the middle of one line to the middle
  ;;       of one two below, on a real frame
  ;; WHEN it is broken into rectangles
  ;; THEN there are three, which is the shape a selection has
  (skip-unless (smear-cursor-x11-tests--graphical-p))
  (with-temp-buffer
    (dotimes (i 40) (insert (format "line %d %s\n" i (make-string 40 ?z))))
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (forward-line 2) (forward-char 10)
    (let ((beg (point)))
      (forward-line 2) (forward-char 5)
      (redisplay t)
      (should (= 3 (length (smear-cursor--region-rects
                            (selected-window) beg (point))))))))

(ert-deftest smear-cursor-test-offset-corners-match-the-window-query ()
  ;; GIVEN a quad in window pixels
  ;; WHEN it is shifted by an offset taken once, and separately by
  ;;      asking the window for every corner
  ;; THEN the two agree.
  ;;
  ;;      The offset exists because the query is not free: a fifty-frame
  ;;      flight of four layers asked the window for its geometry two
  ;;      hundred times, which cost 31 ms of the Lisp thread on every
  ;;      cursor movement.  `smear-cursor-report' does not show this:
  ;;      it measures the player thread.
  (cl-letf (((symbol-function 'window-pixel-edges)
             (lambda (&rest _) '(1 37 837 623)))
            ((symbol-function 'window-inside-pixel-edges)
             (lambda (&rest _) '(29 80 829 623))))
    (let* ((corners (vector 10.0 20.0 30.0 20.0 30.0 40.0 10.0 40.0))
           (off (smear-cursor-x11--frame-offset nil)))
      (should (equal (smear-cursor-x11--corners-in-frame nil corners)
                     (smear-cursor-x11--corners-offset corners
                                                       (car off) (cdr off)))))))

(provide 'smear-cursor-x11-tests)
;;; smear-cursor-x11-tests.el ends here
