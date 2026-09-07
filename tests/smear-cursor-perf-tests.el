;;; smear-cursor-perf-tests.el --- budgets, not stopwatches -*- lexical-binding: t -*-

;; These are budgets on work, not on wall-clock time.  `make test'
;; checks them, so a regression is caught here rather than noticed as
;; something feeling wrong.
;;
;; What a flight costs is decided when it is laid out: how many frames,
;; over how large a box, in how many layers.  All of that is arithmetic
;; on numbers Lisp holds before a single pixel moves.  Timing the result
;; instead would measure the machine the tests ran on, passing on a fast
;; one and flaking on a loaded one.
;;
;; The budgets are deliberately loose: roughly twice what the code does
;; today.  A test that fails when a number moves by a tenth is a test
;; that gets deleted.  These are meant to catch the faults that
;; actually happened, each of which was a factor of ten or more.

(require 'ert)
(require 'cl-lib)
(require 'smear-cursor)

;;;; What a flight costs

(defun smear-cursor-perf--bytes (frames nlayers &optional still)
  "Bytes FRAMES will push over the X connection with NLAYERS layers.

STILL for a flight whose shapes do not move: those are rendered and
uploaded once, and the frames after the first are a composite the
server does by itself.  See `smear-cursor--effect-still-p\='.

The renderer reads its result back and hands it to `XPutImage', one
box per frame, four bytes a pixel.  On a display reached over a
network that is the whole cost of an animation, and it is invisible to
`smear-cursor-report': `XFlush' does not wait, so the frames are all
queued inside their budget and the wire falls behind."
  (let* ((stride (smear-cursor--flight-stride nlayers))
         (n (/ (length frames) stride))
         (bytes 0))
    (dotimes (k (if still (min n 1) n) bytes)
      (let ((w (max 0 (aref frames (+ (* k stride) 2))))
            (h (max 0 (aref frames (+ (* k stride) 3)))))
        (setq bytes (+ bytes (* 4 w h)))))))

(defun smear-cursor-perf--boxes (frames nlayers)
  "The (W . H) of every frame in FRAMES, which has NLAYERS layers."
  (let ((stride (smear-cursor--flight-stride nlayers))
        (out nil))
    (dotimes (k (/ (length frames) stride) (nreverse out))
      (push (cons (aref frames (+ (* k stride) 2))
                  (aref frames (+ (* k stride) 3)))
            out))))

(defun smear-cursor-perf--pulse (width height)
  "The frames a line pulse lays over a WIDTH by HEIGHT line."
  (let* ((effect (smear-cursor-effect 'line-pulse))
         (rects (list (vector 0.0 0.0 (float width) (float height))))
         (n (max 2 (round (* (plist-get effect :duration)
                             (smear-cursor--effect-rate))))))
    (cons (smear-cursor--effect-frames effect rects n)
          (length (smear-cursor--effect-layers effect rects)))))

;;;; The budgets

(ert-deftest smear-cursor-perf-test-a-line-pulse-stays-under-its-budget ()
  ;; GIVEN a full-width line on a large window, 1622 by 44, measured
  ;;       from a real session
  ;; WHEN the pulse over it is laid out
  ;; THEN it pushes under a megabyte.  It costs 0.49 today.
  ;;
  ;; At sixty frames a second, uploaded every frame, this pulse costs
  ;; 14.7 MB.  That is a third of a second of backlog on every jump over
  ;; `ssh -X'.  `smear-cursor-report' does not show it: `XFlush' does
  ;; not wait, so the report measures the queue rather than the wire.
  ;; Thirty frames a second halves the cost, and rendering a still shape
  ;; once takes the other thirtieth.
  ;;
  ;; Three things push it back over budget: dropping the stamp, a longer
  ;; pulse, or a larger box.  The box is grown for blur and glow, so a
  ;; softer edge costs bytes as surely as a longer one.  All three are
  ;; covered here.
  (let* ((smear-cursor-fps 60)
         (smear-cursor-effect-fps 30)
         (smear-cursor-trail-style 'laser)
         (effect (smear-cursor-effect 'line-pulse))
         (made (smear-cursor-perf--pulse 1622 44))
         (mb (/ (smear-cursor-perf--bytes
                 (car made) (cdr made)
                 (smear-cursor--effect-still-p effect))
                1e6)))
    (should (smear-cursor--effect-still-p effect))
    (should (< mb 1.0))))

(ert-deftest smear-cursor-perf-test-a-marked-region-is-cheap ()
  ;; GIVEN a copy and a deletion over a few hundred pixels of one line
  ;; WHEN each is laid out
  ;; THEN both stay under a megabyte.  These fire far more often than a
  ;;      pulse does, so they are the ones that must not grow.
  (let ((smear-cursor-fps 60)
        (smear-cursor-effect-fps 30)
        (rects (list (vector 0.0 0.0 400.0 21.0))))
    (dolist (name '(region-flash region-fade))
      (let* ((effect (smear-cursor-effect name))
             (nl (length (smear-cursor--effect-layers effect rects)))
             (n (max 2 (round (* (plist-get effect :duration)
                                 (smear-cursor--effect-rate)))))
             (mb (/ (smear-cursor-perf--bytes
                     (smear-cursor--effect-frames effect rects n) nl
                     (smear-cursor--effect-still-p effect))
                    1e6)))
        (should (smear-cursor--effect-still-p effect))
        (should (< mb 0.2))))))

(ert-deftest smear-cursor-perf-test-a-trail-stays-a-narrow-streak ()
  ;; GIVEN a long jump straight down a window
  ;; WHEN its flight is laid out
  ;; THEN it pushes under six megabytes (4.4 today) and no frame's box
  ;;      is wider than two hundred pixels.
  ;;
  ;; The width is the interesting half.  A trail is a streak a few
  ;; cells across, and the fault that cost 93 ms a frame was the
  ;; renderer being handed the overlay's shape instead of the frame's
  ;; own box: the shape is most of the window, and compositing that
  ;; rectangle sixty times a second is what a window's worth of pixels
  ;; costs.  A box that starts covering the window shows up here first.
  (let* ((smear-cursor-fps 60)
         (smear-cursor-trail-style 'laser)
         (anim (smear-cursor--anim-create
                :corners (make-vector 8 0.0)
                :target (vector 400.0 900.0 10.0 21.0)
                :cw 10 :lh 21 :ch 21 :color [200 200 200]
                :born (float-time)))
         (layers (plist-get (smear-cursor-trail 'laser) :layers)))
    (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                     (vector 400.0 20.0 10.0 21.0))
    (cl-letf (((symbol-function 'smear-cursor-x11--frame-offset)
               (lambda (_win) (cons 0 0)))
              ((symbol-function 'smear-cursor-x11--corners-offset)
               (lambda (corners _dx _dy) corners)))
      (let* ((flight (smear-cursor--flight-springs
                      anim (smear-cursor--flight-limit anim)))
             (n (nth 0 flight))
             (frames (smear-cursor--flight-frames
                      anim nil layers (nth 1 flight) n (vector 400.0 900.0)))
             (nl (length layers))
             (mb (/ (smear-cursor-perf--bytes frames nl) 1e6))
             (widest (apply #'max (mapcar #'car
                                          (smear-cursor-perf--boxes frames nl)))))
        (should (> n 0))
        (should (< mb 6.0))
        (should (< widest 200))))))

(ert-deftest smear-cursor-perf-test-no-frame-has-an-empty-box ()
  ;; GIVEN the effects that ship, over a line and over a region
  ;; WHEN each is laid out
  ;; THEN every frame has something to draw in.
  ;;
  ;; An empty box is worse than wasted work.  The player takes the
  ;; union of the boxes still playing and finds nothing to draw.  It
  ;; waits out the frame rather than spinning on the lock a handoff
  ;; needs, so an effect that lays out empty frames wastes them.
  (let ((rects (list (vector 0.0 0.0 800.0 21.0))))
    (dolist (name (list 'line-pulse 'region-flash 'region-fade 'type-blink))
      (let* ((effect (smear-cursor-effect name))
             (nl (length (smear-cursor--effect-layers effect rects)))
             (frames (smear-cursor--effect-frames effect rects 12)))
        (dolist (box (smear-cursor-perf--boxes frames nl))
          (should (>= (car box) 1))
          (should (>= (cdr box) 1)))))))

(ert-deftest smear-cursor-perf-test-a-flight-fits-the-module-s-array ()
  ;; GIVEN the longest flight the settings allow
  ;; WHEN its length is asked for
  ;; THEN it fits what the module can hold.  Over that the far end is
  ;;      dropped, and the far end is the part the eye follows.
  (let ((smear-cursor-fps 240)
        (smear-cursor-max-duration 10.0)
        (smear-cursor-long-jump-duration 10.0))
    (should (<= (smear-cursor--flight-limit) smear-cursor--flight-max))))

(ert-deftest smear-cursor-perf-test-laying-out-a-flight-does-not-cons-wildly ()
  ;; GIVEN a long jump
  ;; WHEN its flight is worked out
  ;; THEN it allocates under twelve megabytes of cons and vector cells.
  ;;
  ;; Counted rather than timed: allocation is the same on every machine
  ;; and garbage collection is what a person actually feels.  A long
  ;; smear was measured at six megabytes, so this catches a doubling
  ;; and leaves ordinary drift alone.
  (let* ((smear-cursor-fps 60)
         (smear-cursor-trail-style 'laser)
         (layers (plist-get (smear-cursor-trail 'laser) :layers))
         (anim (smear-cursor--anim-create
                :corners (make-vector 8 0.0)
                :target (vector 400.0 900.0 10.0 21.0)
                :cw 10 :lh 21 :ch 21 :color [200 200 200]
                :born (float-time))))
    (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                     (vector 40.0 20.0 10.0 21.0))
    (cl-letf (((symbol-function 'smear-cursor-x11--frame-offset)
               (lambda (_win) (cons 0 0)))
              ((symbol-function 'smear-cursor-x11--corners-offset)
               (lambda (corners _dx _dy) corners)))
      (let ((before (memory-use-counts)))
        (let* ((flight (smear-cursor--flight-springs
                        anim (smear-cursor--flight-limit anim))))
          (smear-cursor--flight-frames anim nil layers (nth 1 flight)
                                       (nth 0 flight) (vector 400.0 900.0)))
        (let* ((after (memory-use-counts))
               ;; conses are two words, vector cells one; eight bytes a
               ;; word is near enough for a budget
               (bytes (+ (* 16 (- (nth 0 after) (nth 0 before)))
                         (* 8 (- (nth 3 after) (nth 3 before))))))
          (should (< bytes 12e6)))))))

(provide 'smear-cursor-perf-tests)
;;; smear-cursor-perf-tests.el ends here
