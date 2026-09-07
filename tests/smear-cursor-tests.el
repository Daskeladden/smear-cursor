;;; smear-cursor-tests.el --- ERT tests for smear-cursor -*- lexical-binding: t -*-

(require 'ert)
(setq load-prefer-newer t)   ; never test a stale .elc
(require 'smear-cursor)

;; A frame only resolves rows while its measure budget lasts, and
;; `smear-cursor--paint-frame' resets that budget from
;; `smear-cursor-measure-budget', four milliseconds by default.  A
;; garbage collection can land inside four milliseconds, and a test that
;; paints a frame would then fail on timing rather than on what it is
;; about.  Give the suite a budget nothing can exhaust.  The tests that do
;; exercise deferral drive `smear-cursor--measure-deadline' themselves
;; and are unaffected.
(setq smear-cursor-measure-budget 100.0)

;;;; Easing

(ert-deftest smear-cursor-test-corners-from-rect ()
  ;; GIVEN a rect [10 20 8 21]
  ;; WHEN corners are initialized from it
  ;; THEN the four corners are the rect's corners in TL TR BR BL order
  (let ((c (make-vector 8 0.0)))
    (smear-cursor--corners-from-rect c [10.0 20.0 8.0 21.0])
    (should (equal (append c nil)
                   '(10.0 20.0 18.0 20.0 18.0 41.0 10.0 41.0)))))

(defun smear-cursor-tests--spring (corner-rect target n &optional dt)
  "Run N spring steps from CORNER-RECT toward TARGET; return (corners vel last-ret)."
  (let ((c (make-vector 8 0.0)) (v (make-vector 8 0.0)) (ret 1e9))
    (smear-cursor--corners-from-rect c corner-rect)
    (dotimes (_ n)
      (setq ret (smear-cursor--ease-step c v target 0.6 0.45 0.85 3.0
                                         (or dt 1.0))))
    (list c v ret)))

(ert-deftest smear-cursor-test-ease-translation-stretches ()
  ;; GIVEN corners at rest at (0,0,10,20) and a target rect at (100,0,10,20)
  ;; WHEN a few spring steps run
  ;; THEN the quad is stretched horizontally (leading edge ahead of
  ;;      trailing edge by more than the original width) and moving right
  (let* ((r (smear-cursor-tests--spring [0.0 0.0 10.0 20.0]
                                        [100.0 0.0 10.0 20.0] 3))
         (c (car r))
         (tl-x (aref c 0)) (tr-x (aref c 2)))
    (should (> tr-x 10.0))
    (should (> (- tr-x tl-x) 10.0))))

(ert-deftest smear-cursor-test-ease-continuous-taper ()
  ;; GIVEN a diagonal jump
  ;; WHEN one spring step runs from rest
  ;; THEN the corner farthest from the target center moved the smallest
  ;;      fraction of its distance (continuous trailing, not binary)
  (let ((c (make-vector 8 0.0)) (v (make-vector 8 0.0))
        (target [200.0 150.0 10.0 21.0])
        (before (make-vector 8 0.0)))
    (smear-cursor--corners-from-rect c [0.0 0.0 10.0 21.0])
    (dotimes (i 8) (aset before i (aref c i)))
    (smear-cursor--ease-step c v target 0.6 0.45 0.85 3.0 1.0)
    (let* ((tcx 205.0) (tcy 160.5) (fracs nil) (dists nil))
      (dotimes (i 4)
        (let* ((bx (aref before (* 2 i))) (by (aref before (1+ (* 2 i))))
               (nx (aref c (* 2 i))) (ny (aref c (1+ (* 2 i))))
               (d (sqrt (+ (expt (- tcx bx) 2) (expt (- tcy by) 2))))
               (moved (sqrt (+ (expt (- nx bx) 2) (expt (- ny by) 2)))))
          (push (/ moved d) fracs)
          (push d dists)))
      (setq fracs (nreverse fracs) dists (nreverse dists))
      ;; farthest corner (top-left, index 0) has the smallest step fraction
      (let ((far-i 0) (far-d 0.0))
        (dotimes (i 4)
          (when (> (nth i dists) far-d)
            (setq far-d (nth i dists) far-i i)))
        (dotimes (i 4)
          (unless (= i far-i)
            (should (<= (nth far-i fracs) (+ (nth i fracs) 1e-9)))))))))

(ert-deftest smear-cursor-test-ease-converges ()
  ;; GIVEN corners far from a target rect
  ;; WHEN spring steps run repeatedly
  ;; THEN distance and velocity both drop below 0.5 within 300 steps
  (let ((c (make-vector 8 0.0)) (v (make-vector 8 0.0))
        (target [300.0 150.0 10.0 21.0])
        (ret 1e9) (n 0))
    (smear-cursor--corners-from-rect c [0.0 0.0 10.0 21.0])
    (while (and (>= ret 0.5) (< n 300))
      (setq ret (smear-cursor--ease-step c v target 0.6 0.45 0.85 3.0 1.0)
            n (1+ n)))
    (should (< ret 0.5))))

(ert-deftest smear-cursor-test-default-tuning-is-visible ()
  ;; GIVEN the shipped tuning and a 20-cell jump
  ;; WHEN the animation runs to convergence
  ;; THEN the quad stretches several cells wide and holds a streak for
  ;;      long enough to read as one, without dragging past ~0.5 s
  (let* ((cw 10.0) (lh 21.0)
         (target (vector (* 20 cw) 0.0 cw lh))
         (c (make-vector 8 0.0)) (v (make-vector 8 0.0))
         (ret 1e9) (frames 0) (streak 0) (peak 0.0))
    (smear-cursor--corners-from-rect c (vector 0.0 0.0 cw lh))
    (while (and (>= ret smear-cursor--eps) (< frames 200))
      (setq ret (smear-cursor--ease-step c v target
                                         smear-cursor-stiffness-head
                                         smear-cursor-stiffness-tail
                                         smear-cursor-damping
                                         smear-cursor-trailing-exponent 1.0)
            frames (1+ frames))
      (let ((minx 1e30) (maxx -1e30))
        (dotimes (i 4)
          (setq minx (min minx (aref c (* 2 i)))
                maxx (max maxx (aref c (* 2 i)))))
        (let ((cells (/ (- maxx minx) cw)))
          (setq peak (max peak cells))
          (when (>= cells 3.0) (setq streak (1+ streak))))))
    (should (< ret smear-cursor--eps))
    (should (>= peak 6.0))                ; a streak, not a fat cursor
    (should (>= streak 6))                ; ~100 ms of it, at 60 Hz
    (should (<= frames 30))))             ; and gone within half a second

(ert-deftest smear-cursor-test-ease-retarget ()
  ;; GIVEN a spring animation part-way toward target A with momentum
  ;; WHEN the target changes to B and stepping continues
  ;; THEN corners converge on B
  (let ((c (make-vector 8 0.0)) (v (make-vector 8 0.0))
        (ret 1e9) (n 0))
    (smear-cursor--corners-from-rect c [0.0 0.0 10.0 21.0])
    (dotimes (_ 5)
      (smear-cursor--ease-step c v [200.0 0.0 10.0 21.0]
                               0.6 0.45 0.85 3.0 1.0))
    (while (and (>= ret 0.5) (< n 300))
      (setq ret (smear-cursor--ease-step c v [0.0 100.0 10.0 21.0]
                                         0.6 0.45 0.85 3.0 1.0)
            n (1+ n)))
    (should (< ret 0.5))
    (should (< (abs (- (aref c 0) 0.0)) 0.5))
    (should (< (abs (- (aref c 1) 100.0)) 0.5))))

(ert-deftest smear-cursor-test-ease-dt-monotonic ()
  ;; GIVEN identical rest states
  ;; WHEN one steps with dt 2.0 and another with dt 1.0
  ;; THEN the dt 2.0 step moves the head at least as far
  (let* ((a (smear-cursor-tests--spring [0.0 0.0 10.0 21.0]
                                        [100.0 0.0 10.0 21.0] 1 2.0))
         (b (smear-cursor-tests--spring [0.0 0.0 10.0 21.0]
                                        [100.0 0.0 10.0 21.0] 1 1.0)))
    (should (>= (aref (car a) 2) (aref (car b) 2)))))

(ert-deftest smear-cursor-test-clamp-corners-translates ()
  ;; GIVEN a cursor quad 800 px from its target and a 50 px length cap
  ;; WHEN the corners are clamped
  ;; THEN the quad lands within the cap plus its own extent (single
  ;;      rigid translation along the farthest corner's ray)
  ;;      AND the quad's shape is exactly preserved (pure translation)
  (let ((c (make-vector 8 0.0))
        (target [800.0 0.0 10.0 21.0]))
    (smear-cursor--corners-from-rect c [0.0 0.0 10.0 21.0])
    (smear-cursor--clamp-corners c target 50.0)
    (let ((dmax 0.0))
      (dotimes (i 4)
        (let ((d (sqrt (+ (expt (- 805.0 (aref c (* 2 i))) 2)
                          (expt (- 10.5 (aref c (1+ (* 2 i)))) 2)))))
          (when (> d dmax) (setq dmax d))))
      (should (>= dmax 49.9))
      (should (<= dmax (+ 50.0 24.0))))
    (should (< (abs (- (- (aref c 2) (aref c 0)) 10.0)) 1e-6))
    (should (< (abs (- (- (aref c 5) (aref c 1)) 21.0)) 1e-6))))

(ert-deftest smear-cursor-test-clamp-corners-noop-in-range ()
  ;; GIVEN a quad already within the length cap
  ;; WHEN the corners are clamped
  ;; THEN they are unchanged
  (let ((c (make-vector 8 0.0)) (before (make-vector 8 0.0))
        (target [30.0 0.0 10.0 21.0]))
    (smear-cursor--corners-from-rect c [0.0 0.0 10.0 21.0])
    (dotimes (i 8) (aset before i (aref c i)))
    (smear-cursor--clamp-corners c target 200.0)
    (dotimes (i 8)
      (should (= (aref c i) (aref before i))))))

;;;; Rasterizer

(defun smear-cursor-tests--rect-corners (x y w h)
  (let ((c (make-vector 8 0.0)))
    (smear-cursor--corners-from-rect c (vector (float x) (float y)
                                               (float w) (float h)))
    c))

(ert-deftest smear-cursor-test-scanline-xs-rect ()
  ;; GIVEN an axis-aligned rect quad from (2,3) to (7,9)
  ;; WHEN a scanline at y=5.5 is intersected
  ;; THEN there are two intersections at x=2 and x=7, sorted
  (let ((xs (make-vector 4 0.0))
        (c (smear-cursor-tests--rect-corners 2 3 5 6)))
    (should (= (smear-cursor--scanline-xs c 5.5 xs) 2))
    (should (< (abs (- (aref xs 0) 2.0)) 1e-9))
    (should (< (abs (- (aref xs 1) 7.0)) 1e-9))))

(ert-deftest smear-cursor-test-scanline-xs-outside ()
  ;; GIVEN the same rect quad
  ;; WHEN scanlines above and below are intersected
  ;; THEN there are zero intersections
  (let ((xs (make-vector 4 0.0))
        (c (smear-cursor-tests--rect-corners 2 3 5 6)))
    (should (= (smear-cursor--scanline-xs c 2.5 xs) 0))
    (should (= (smear-cursor--scanline-xs c 9.5 xs) 0))))

(ert-deftest smear-cursor-test-scanline-xs-butterfly ()
  ;; GIVEN a self-intersecting (butterfly) quad TL(0,0) TR(10,0)
  ;;       BR(0,10) BL(10,10), a crossed corner order
  ;; WHEN a scanline at y=2 is intersected
  ;; THEN even-odd filling yields the top triangle span [2, 8]
  (let ((xs (make-vector 4 0.0))
        (c (vector 0.0 0.0 10.0 0.0 0.0 10.0 10.0 10.0)))
    (should (= (smear-cursor--scanline-xs c 2.0 xs) 2))
    (should (< (abs (- (aref xs 0) 2.0)) 1e-9))
    (should (< (abs (- (aref xs 1) 8.0)) 1e-9))))

(ert-deftest smear-cursor-test-row-coverage-interior ()
  ;; GIVEN a rect quad from (2,3) to (7,9)
  ;; WHEN coverage is computed for pixel row y=5 over pixels 0..9
  ;; THEN interior pixels 2..6 have full coverage (32/32) and others none
  (let ((row (make-vector 10 0)) (xs (make-vector 4 0.0))
        (c (smear-cursor-tests--rect-corners 2 3 5 6)))
    (should (smear-cursor--row-coverage c 5 row 0 10 xs))
    (dotimes (px 10)
      (let ((f (aref row px)))
        (if (and (>= px 2) (< px 7))
            (should (= f 32))
          (should (= f 0)))))))

(ert-deftest smear-cursor-test-row-coverage-fractional-edge ()
  ;; GIVEN a rect quad whose left edge is at x=2.5
  ;; WHEN coverage is computed for an interior pixel row
  ;; THEN pixel 2 has half coverage (16/32)
  (let ((row (make-vector 10 0)) (xs (make-vector 4 0.0))
        (c (smear-cursor-tests--rect-corners 2.5 3 4.5 6)))
    (smear-cursor--row-coverage c 5 row 0 10 xs)
    (should (= (aref row 2) 16))
    (should (= (aref row 3) 32))))

(ert-deftest smear-cursor-test-row-coverage-vertical-supersample ()
  ;; GIVEN a rect quad from (0,3.5) to (10,9)
  ;; WHEN coverage is computed for pixel row y=3 (only the lower
  ;;      sub-scanline at y=3.75 is inside)
  ;; THEN covered pixels get half coverage (16/32)
  (let ((row (make-vector 10 0)) (xs (make-vector 4 0.0))
        (c (smear-cursor-tests--rect-corners 0 3.5 10 5.5)))
    (smear-cursor--row-coverage c 3 row 0 10 xs)
    (should (= (aref row 5) 16))))

(ert-deftest smear-cursor-test-row-coverage-clip ()
  ;; GIVEN a rect quad extending left of the requested pixel range
  ;; WHEN coverage is computed for pixels 4..9 only
  ;; THEN indices are relative to the range start and nothing overflows
  (let ((row (make-vector 6 0)) (xs (make-vector 4 0.0))
        (c (smear-cursor-tests--rect-corners 0 3 8 6)))
    (should (smear-cursor--row-coverage c 5 row 4 6 xs))
    ;; pixel 4 (row index 0) through pixel 7 (row index 3) covered
    (should (= (aref row 0) 32))
    (should (= (aref row 3) 32))
    (should (= (aref row 4) 0))))

;;;; Color

(ert-deftest smear-cursor-test-blend-endpoints ()
  ;; GIVEN background [16 32 48] and smear color [240 200 100]
  ;; WHEN blending with coverage 0.0 and 1.0
  ;; THEN the result is exactly the opaque bg / fg ARGB pixel
  (should (= (smear-cursor--blend [16 32 48] [240 200 100] 0.0) #xFF102030))
  (should (= (smear-cursor--blend [16 32 48] [240 200 100] 1.0) #xFFF0C864)))

(ert-deftest smear-cursor-test-blend-midpoint-and-clamp ()
  ;; GIVEN background [0 0 0] and smear color [100 200 50]
  ;; WHEN blending with coverage 0.5, and with out-of-range coverages
  ;; THEN 0.5 yields the midpoint and coverage is clamped to [0,1]
  (should (= (smear-cursor--blend [0 0 0] [100 200 50] 0.5) #xFF326419))
  (should (= (smear-cursor--blend [0 0 0] [100 200 50] 1.7) #xFF64C832))
  (should (= (smear-cursor--blend [0 0 0] [100 200 50] -0.2) #xFF000000)))

(ert-deftest smear-cursor-test-fade-coeffs-horizontal ()
  ;; GIVEN a quad stretched from x=0 back-corner to a target at x=100
  ;; WHEN fade coefficients are computed
  ;; THEN fade is ~0 at the tail corner and ~1 at the target center
  (let ((c (vector 0.0 0.0 60.0 0.0 60.0 21.0 0.0 21.0))
        (coeffs (make-vector 3 0.0)))
    (smear-cursor--fade-coeffs c [100.0 0.0 10.0 21.0] coeffs)
    (let ((fade-tail (+ (* (aref coeffs 0) 0.0)
                        (* (aref coeffs 1) 10.0) (aref coeffs 2)))
          (fade-head (+ (* (aref coeffs 0) 105.0)
                        (* (aref coeffs 1) 10.5) (aref coeffs 2))))
      (should (< (abs fade-tail) 0.01))
      (should (< (abs (- fade-head 1.0)) 0.01)))))

(ert-deftest smear-cursor-test-fade-coeffs-degenerate ()
  ;; GIVEN a quad already sitting on its target (no motion axis)
  ;; WHEN fade coefficients are computed
  ;; THEN the constant-1 fade is returned (no divide-by-zero)
  (let ((c (make-vector 8 0.0)) (coeffs (make-vector 3 9.0)))
    (smear-cursor--corners-from-rect c [50.0 50.0 10.0 21.0])
    (smear-cursor--fade-coeffs c [50.0 50.0 10.0 21.0] coeffs)
    (should (equal (append coeffs nil) '(0.0 0.0 1.0)))))

;;;; Frame painting (display glue stubbed)

(defun smear-cursor-tests--fake-anim (corner-rect target-rect)
  "Build an anim with 10x21 cells; cell resolution gets stubbed."
  (let ((anim (smear-cursor--anim-create
               :window nil :buffer nil
               :corners (make-vector 8 0.0)
               :target target-rect
               :cells (make-hash-table :test 'eql)
               :pads (make-hash-table :test 'eql)
               :cw 10 :lh 21 :ch 21 :y-base 0
               :win-w 800 :win-h 420
               :color [255 0 0]
               :fade (vector 0.0 0.0 1.0)
               :frame 0)))
    (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                     corner-rect)
    anim))

(defmacro smear-cursor-tests--with-stub-cells (refreshed &rest body)
  "Stub cell resolution to plain records; collect canvas-refresh calls."
  (declare (indent 1))
  `(let ((,refreshed nil))
     (cl-letf (((symbol-function 'smear-cursor--row-info)
                (lambda (_anim _row) '(1 1000 0)))   ; a simple grid row
               ((symbol-function 'smear-cursor--resolve-cell)
                (lambda (anim col row &optional _no-pad)
                  (let ((cw (smear-cursor--anim-cw anim)))
                    (smear-cursor--cell-create
                     :data (make-vector (* cw (smear-cursor--anim-ch anim)) 0)
                     :image (list 'image :type 'canvas)
                     :overlay nil
                     :bg [0 0 0] :bgpix #xFF000000
                     :stamp -1 :col col :row row
                     :x (* col cw) :w cw))))
               ((symbol-function 'canvas-refresh)
                (lambda (img &optional _reload)
                  (push img ,refreshed))))
       ,@body)))

(ert-deftest smear-cursor-test-paint-touches-covered-cells ()
  ;; GIVEN a quad covering pixel rect (5,2)-(25,40) on a 10x21 grid
  ;; WHEN one frame is painted
  ;; THEN exactly the cells (0,0) (1,0) (2,0) (0,1) (1,1) (2,1) are
  ;;      claimed AND each dirty cell's canvas was refreshed once
  (let ((anim (smear-cursor-tests--fake-anim
               [5.0 2.0 20.0 38.0] [5.0 2.0 20.0 38.0])))
    (smear-cursor-tests--with-stub-cells refreshed
      (smear-cursor--paint-frame anim)
      (should (= (hash-table-count (smear-cursor--anim-cells anim)) 6))
      (should (= (length refreshed) 6)))))

(ert-deftest smear-cursor-test-paint-writes-blended-pixels ()
  ;; GIVEN a quad exactly covering cell (0,0) (pixels 0-9 x 0-20)
  ;; WHEN one frame is painted
  ;; THEN that cell's interior pixels equal the smear color over bg
  (let ((anim (smear-cursor-tests--fake-anim
               [0.0 0.0 10.0 21.0] [0.0 0.0 10.0 21.0])))
    (smear-cursor-tests--with-stub-cells _refreshed
      (smear-cursor--paint-frame anim)
      (let* ((cell (gethash (smear-cursor--cell-key 0 0)
                            (smear-cursor--anim-cells anim)))
             (data (smear-cursor--cell-data cell)))
        ;; interior pixel (5, 10): full coverage -> pure smear color
        (should (= (aref data (+ (* 10 10) 5)) #xFFFF0000))))))

(ert-deftest smear-cursor-test-paint-releases-departed-overlay-cells ()
  ;; GIVEN a painted frame whose cell carries its own overlay
  ;; WHEN the quad moves entirely away and a frame is painted
  ;; THEN the departed cell is released from the cells table and its
  ;;      overlay detached
  (with-temp-buffer
    (insert "some text\n")
    (let ((anim (smear-cursor-tests--fake-anim
                 [0.0 0.0 10.0 21.0] [50.0 0.0 10.0 21.0]))
          (smear-cursor--pool (make-hash-table :test 'eql))
          (ov (make-overlay 1 2)))
      (cl-letf (((symbol-function 'smear-cursor--row-info)
                 (lambda (_anim _row) '(1 1000 0)))
                ((symbol-function 'smear-cursor--resolve-cell)
                 (lambda (anim col row &optional _no-pad)
                   (let ((cw (smear-cursor--anim-cw anim)))
                     (smear-cursor--cell-create
                      :data (make-vector (* cw (smear-cursor--anim-ch anim)) 0)
                      :image (list 'image :type 'canvas)
                      :overlay (and (= col 0) (= row 0) ov)
                      :bg [0 0 0] :bgpix #xFF000000
                      :stamp -1 :col col :row row
                      :x (* col cw) :w cw))))
                ((symbol-function 'canvas-refresh) #'ignore))
        (smear-cursor--paint-frame anim)
        (should (gethash (smear-cursor--cell-key 0 0)
                         (smear-cursor--anim-cells anim)))
        (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                         [50.0 0.0 10.0 21.0])
        (smear-cursor--paint-frame anim)
        (should-not (gethash (smear-cursor--cell-key 0 0)
                             (smear-cursor--anim-cells anim)))
        (should-not (overlay-buffer ov))
        (should (= (length (gethash 10 smear-cursor--pool)) 1))
        (should (gethash (smear-cursor--cell-key 5 0)
                         (smear-cursor--anim-cells anim)))))))

(ert-deftest smear-cursor-test-pool-rejects-stale-height ()
  ;; GIVEN a cell taken when the glyph height was 12
  ;; WHEN the height becomes 13 (which flushes the pool while that cell
  ;;      is live, so it misses the flush) and it is released only
  ;;      afterwards
  ;; THEN the pool does not hand its canvas back: it holds w*12 pixels
  ;;      and the painter would write w*13 of them
  (let* ((smear-cursor--pool (make-hash-table :test 'eql))
         (smear-cursor--pool-size nil)
         (short (smear-cursor--anim-create :ch 12))
         (tall (smear-cursor--anim-create :ch 13))
         stale)
    (cl-letf (((symbol-function 'canvas-refresh) #'ignore))
      (setq stale (smear-cursor--pool-cell short 12))
      (should (= (length (smear-cursor--cell-data stale)) (* 12 12)))
      (smear-cursor--pool-cell tall 12)   ; the height changes: pool flushed
      (smear-cursor--release-cell nil stale)
      (should (= (length (smear-cursor--cell-data
                          (smear-cursor--pool-cell tall 12)))
                 (* 12 13))))))

(ert-deftest smear-cursor-test-paint-parks-departed-pad-cells ()
  ;; GIVEN a painted frame whose covered cell is a pad cell (no own
  ;;       overlay; its canvas is referenced by a pad after-string)
  ;; WHEN the quad moves entirely away and frames are painted
  ;; THEN the pad cell is NOT pooled (the pad still displays its
  ;;      canvas) but parked in the cells table, cleared once, and it
  ;;      revives when covered again
  (let ((anim (smear-cursor-tests--fake-anim
               [0.0 0.0 10.0 21.0] [50.0 0.0 10.0 21.0]))
        (smear-cursor--pool (make-hash-table :test 'eql)))
    (smear-cursor-tests--with-stub-cells refreshed
      (smear-cursor--paint-frame anim)
      (let ((cell (gethash (smear-cursor--cell-key 0 0)
                           (smear-cursor--anim-cells anim))))
        (should (smear-cursor--cell-p cell))
        (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                         [50.0 0.0 10.0 21.0])
        (smear-cursor--paint-frame anim)
        ;; parked, not pooled, not removed
        (should (eq (gethash (smear-cursor--cell-key 0 0)
                             (smear-cursor--anim-cells anim))
                    cell))
        (should-not (memq cell (gethash 10 smear-cursor--pool)))
        (should (= (smear-cursor--cell-stamp cell) -2))
        ;; cleared exactly once: painting the far quad again must not
        ;; refresh the parked cell a second time
        (let ((before (cl-count (smear-cursor--cell-image cell) refreshed
                                :test #'eq)))
          (smear-cursor--paint-frame anim)
          (should (= (cl-count (smear-cursor--cell-image cell) refreshed
                               :test #'eq)
                     before)))
        ;; revive: quad returns over the parked cell
        (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                         [0.0 0.0 10.0 21.0])
        (smear-cursor--paint-frame anim)
        (should (> (smear-cursor--cell-stamp cell) 0))))))

(ert-deftest smear-cursor-test-paint-skip-cells-stay-skipped ()
  ;; GIVEN cell resolution that returns 'skip for row 0
  ;; WHEN a frame is painted over rows 0 and 1
  ;; THEN no cell records exist for row 0 and painting did not error
  (let ((anim (smear-cursor-tests--fake-anim
               [0.0 0.0 10.0 42.0] [0.0 0.0 10.0 42.0])))
    (cl-letf (((symbol-function 'smear-cursor--row-info)
               (lambda (_anim _row) '(1 1000 0)))
              ((symbol-function 'smear-cursor--resolve-cell)
               (lambda (anim col row &optional _no-pad)
                 (if (= row 0) 'skip
                   (let ((cw (smear-cursor--anim-cw anim)))
                     (smear-cursor--cell-create
                      :data (make-vector (* cw (smear-cursor--anim-lh anim)) 0)
                      :image (list 'image :type 'canvas) :overlay nil
                      :bg [0 0 0] :bgpix #xFF000000
                      :stamp -1 :col col :row row
                      :x (* col cw) :w cw)))))
              ((symbol-function 'canvas-refresh) #'ignore))
      (smear-cursor--paint-frame anim)
      (should (eq (gethash (smear-cursor--cell-key 0 0)
                           (smear-cursor--anim-cells anim))
                  'skip))
      (should (smear-cursor--cell-p
               (gethash (smear-cursor--cell-key 0 1)
                        (smear-cursor--anim-cells anim)))))))

(ert-deftest smear-cursor-test-paint-clamps-to-window ()
  ;; GIVEN a quad extending past the window edges (negative x/y)
  ;; WHEN a frame is painted
  ;; THEN painting stays in bounds and does not error
  (let ((anim (smear-cursor-tests--fake-anim
               [-15.0 -30.0 30.0 60.0] [-15.0 -30.0 30.0 60.0])))
    (smear-cursor-tests--with-stub-cells _refreshed
      (smear-cursor--paint-frame anim)
      (should (> (hash-table-count (smear-cursor--anim-cells anim)) 0)))))

;;;; Proportional rows: glyph maps

(defmacro smear-cursor-tests--with-glyph-row (xs &rest body)
  "Run BODY over a simulated proportional row with glyph edges XS.
Buffer position 1+I renders at (aref XS I); the last entry is the
line end, so the glyph before it has a width.  Neighbours sharing an x
are one glyph spanning several characters, as a ligature is.  GMAP is
bound to a fresh map for the row, CALLS counts the layout queries BODY
causes, and the frame's measuring budget starts generous.  A real
frame's is opened by the painter."
  (declare (indent 1))
  `(let* ((xs ,xs)
          (row-y 40)
          (calls 0)
          (smear-cursor--measure-deadline (+ (float-time) 100))
          (smear-cursor--measure-deferred nil)
          (gmap (smear-cursor--gmap-create
                 :bol 1 :eol (length xs)
                 :row-y row-y :row-y1 (+ row-y 20) :mid-y (+ row-y 10))))
     (ignore calls)
     (cl-letf (((symbol-function 'pos-visible-in-window-p)
                (lambda (pos &optional _win _partially)
                  (setq calls (1+ calls))
                  (when (and (>= pos 1) (<= pos (length xs)))
                    (list (aref xs (1- pos)) row-y))))
               ((symbol-function 'posn-at-x-y)
                (lambda (x _y &optional _win _whole)
                  (let ((p 1))
                    (dotimes (i (length xs))
                      (when (<= (aref xs i) x) (setq p (1+ i))))
                    (list nil nil (cons x 0) nil nil p))))
               ((symbol-function 'posn-area) (lambda (_posn) nil))
               ((symbol-function 'posn-point) (lambda (posn) (nth 5 posn))))
       ,@body)))

(defun smear-cursor-tests--gmap-box (gmap i)
  "Entry I of GMAP as (X W START END)."
  (list (aref (smear-cursor--gmap-xs gmap) i)
        (aref (smear-cursor--gmap-ws gmap) i)
        (aref (smear-cursor--gmap-starts gmap) i)
        (aref (smear-cursor--gmap-ends gmap) i)))

(ert-deftest smear-cursor-test-gmap-measures-glyph-under-pixel ()
  ;; GIVEN a proportional row with glyphs at 0, 7, 18, 26 and end at 40
  ;; WHEN the pixel inside the second glyph is looked up
  ;; THEN that glyph is measured, with its own left edge and advance
  (smear-cursor-tests--with-glyph-row [0 7 18 26 40]
    (let ((i (smear-cursor--gmap-index gmap nil 12)))
      (should i)
      (should (equal (smear-cursor-tests--gmap-box gmap i) '(7 11 2 3))))))

(ert-deftest smear-cursor-test-gmap-grows-both-ways ()
  ;; GIVEN a map seeded in the middle of the row
  ;; WHEN pixels to the right and then to the left are looked up
  ;; THEN the row is measured outward in both directions
  (smear-cursor-tests--with-glyph-row [0 7 18 26 40]
    (should (smear-cursor--gmap-index gmap nil 20))       ; seed on glyph 3
    (let ((right (smear-cursor--gmap-index gmap nil 30))
          (left (smear-cursor--gmap-index gmap nil 2)))
      (should (equal (smear-cursor-tests--gmap-box gmap right) '(26 14 4 5)))
      (should (equal (smear-cursor-tests--gmap-box gmap left) '(0 7 1 2)))
      ;; seed, one step right, two steps left
      (should (= (smear-cursor--gmap-n gmap) 4)))))

(ert-deftest smear-cursor-test-gmap-groups-shared-glyph ()
  ;; GIVEN two characters rendered as one glyph, a ligature or text
  ;;       hidden behind an ellipsis, both reporting the same x
  ;; WHEN the glyph is looked up
  ;; THEN one entry covers both characters, so the cell that replaces
  ;;      it stands in for the whole cluster
  (smear-cursor-tests--with-glyph-row [0 7 7 18 30]
    (let ((i (smear-cursor--gmap-index gmap nil 10)))
      (should (equal (smear-cursor-tests--gmap-box gmap i) '(7 11 2 4))))))

(ert-deftest smear-cursor-test-gmap-stops-past-end-of-line ()
  ;; GIVEN a row whose text ends at x 40
  ;; WHEN a pixel past the end is looked up
  ;; THEN there is no glyph to paint into, since proportional rows get
  ;;      no end-of-line padding, and the row is not measured endlessly
  (smear-cursor-tests--with-glyph-row [0 7 18 26 40]
    (should (null (smear-cursor--gmap-index gmap nil 200)))
    (should (<= (smear-cursor--gmap-n gmap) 4))))

(ert-deftest smear-cursor-test-gmap-honours-the-cap ()
  ;; GIVEN a row far longer than the measuring cap, measured from its
  ;;       left edge (a lookup starts where it lands, so reaching far
  ;;       across a line is what walks up a bill)
  ;; WHEN a pixel beyond the cap's reach is looked up
  ;; THEN measuring stops at the cap instead of walking the whole line
  (smear-cursor-tests--with-glyph-row
      (vconcat (cl-loop for i from 0 to 400 collect (* i 5)))
    (should (smear-cursor--gmap-index gmap nil 2))
    (should (null (smear-cursor--gmap-index gmap nil 1900)))
    (should (= (smear-cursor--gmap-n gmap) smear-cursor--gmap-cap))))

(ert-deftest smear-cursor-test-gmap-bisects-a-long-shared-glyph ()
  ;; GIVEN 200 characters rendered as a single glyph, the shape of the
  ;;       hidden URL half of an org link
  ;; WHEN the glyph under a pixel inside it is measured
  ;; THEN the run's extent is bisected rather than walked, so its cost
  ;;      is logarithmic in its length: walking it cost one layout
  ;;      query per character, some 20 ms for one glyph
  (smear-cursor-tests--with-glyph-row (vconcat (make-vector 200 7) [18 30])
    (let ((i (smear-cursor--gmap-index gmap nil 10)))
      (should i)
      ;; one entry covering all 200 characters, with the advance to the
      ;; next glyph as its width
      (should (equal (smear-cursor-tests--gmap-box gmap i) '(7 11 1 201)))
      (should (< calls 40)))))

(ert-deftest smear-cursor-test-gmap-defers-work-past-the-budget ()
  ;; GIVEN a frame that has used up its measuring time
  ;; WHEN a pixel right of the measured glyphs is looked up
  ;; THEN nothing more is measured.  The frame yields rather than
  ;;      blocking redisplay, and the deferral is recorded so the engine
  ;;      does not read the empty frame as "nothing to draw", and the
  ;;      next frame picks the row up where this one stopped
  (smear-cursor-tests--with-glyph-row
      (vconcat (cl-loop for i from 0 to 60 collect (* i 5)))
    (should (smear-cursor--gmap-index gmap nil 2))
    (let ((measured (smear-cursor--gmap-n gmap))
          (spent calls))
      (setq smear-cursor--measure-deadline (- (float-time) 1))
      (should (null (smear-cursor--gmap-index gmap nil 250)))
      (should (= (smear-cursor--gmap-n gmap) measured))
      (should (= calls spent))
      (should smear-cursor--measure-deferred))
    (setq smear-cursor--measure-deadline (+ (float-time) 100))
    (should (smear-cursor--gmap-index gmap nil 250))))

(ert-deftest smear-cursor-test-gmap-budget-not-spent-when-covered ()
  ;; GIVEN a map that already covers the pixel asked for
  ;; WHEN it is looked up
  ;; THEN no deferral is recorded: there was no work to put off, and
  ;;      claiming otherwise would keep a finished animation alive
  (smear-cursor-tests--with-glyph-row [0 7 18 26 40]
    (should (smear-cursor--gmap-index gmap nil 12))
    (setq smear-cursor--measure-deferred nil)
    (should (smear-cursor--gmap-index gmap nil 12))
    (should-not smear-cursor--measure-deferred)))

(ert-deftest smear-cursor-test-gmap-retires-an-unmeasurable-frontier ()
  ;; GIVEN a row ending in characters that share the last glyph's x, so
  ;;       nothing further right can be measured to give it a width
  ;; WHEN the same pixel past it is looked up on frame after frame
  ;; THEN the failed search runs once, not once per frame
  (smear-cursor-tests--with-glyph-row [0 7 14 14 14]
    (should (smear-cursor--gmap-index gmap nil 8))
    (should (null (smear-cursor--gmap-index gmap nil 200)))
    (let ((spent calls))
      (should (null (smear-cursor--gmap-index gmap nil 200)))
      (should (= calls spent)))))

;;;; Engine

(defun smear-cursor-tests--engine-anim (corner-rect target-rect &optional show)
  "Anim wired to the current buffer/window.
The cursor starts hidden unless SHOW, which mimics the engine before
its first painted frame."
  (let ((anim (smear-cursor--anim-create
               :window (selected-window) :buffer (current-buffer)
               :corners (make-vector 8 0.0)
               :target target-rect
               :cells (make-hash-table :test 'eql)
               :pads (make-hash-table :test 'eql)
               :cw 10 :lh 21 :ch 21 :y-base 0
               :win-w 800 :win-h 420
               :color [255 0 0] :fade (vector 0.0 0.0 1.0)
               :frame 0 :last-time (float-time)
               :cursor-saved (unless show
                               (smear-cursor--hide-cursor (current-buffer))))))
    (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                     corner-rect)
    anim))

(ert-deftest smear-cursor-test-stop-is-idempotent ()
  ;; GIVEN no animation running
  ;; WHEN smear-cursor--stop is called twice
  ;; THEN nothing errors and the state stays nil
  (smear-cursor--stop)
  (smear-cursor--stop)
  (should (null smear-cursor--anim)))

(ert-deftest smear-cursor-test-cursor-save-restore-global ()
  ;; GIVEN a buffer with no local cursor-type
  ;; WHEN cursor is hidden and then restored
  ;; THEN cursor-type has no local binding afterward
  (with-temp-buffer
    (let ((saved (smear-cursor--hide-cursor (current-buffer))))
      (should (eq cursor-type nil))
      (should (local-variable-p 'cursor-type))
      (smear-cursor--restore-cursor (current-buffer) saved)
      (should-not (local-variable-p 'cursor-type)))))

(ert-deftest smear-cursor-test-cursor-save-restore-local ()
  ;; GIVEN a buffer with a local cursor-type of 'bar
  ;; WHEN cursor is hidden and then restored
  ;; THEN the local value 'bar is back
  (with-temp-buffer
    (setq-local cursor-type 'bar)
    (let ((saved (smear-cursor--hide-cursor (current-buffer))))
      (should (eq cursor-type nil))
      (smear-cursor--restore-cursor (current-buffer) saved)
      (should (eq cursor-type 'bar)))))

(ert-deftest smear-cursor-test-tick-converges-and-stops ()
  ;; GIVEN a live animation whose quad is nearly at its target
  ;;       (display glue stubbed, window checks stubbed true)
  ;; WHEN ticks run
  ;; THEN the animation stops and state is cleared
  (with-temp-buffer
    (cl-letf (((symbol-function 'smear-cursor--resolve-cell)
               (lambda (_a _c _r) 'skip))
              ((symbol-function 'canvas-refresh) #'ignore)
              ((symbol-function 'smear-cursor--context-valid-p)
               (lambda (_anim) t)))
      (setq smear-cursor--anim
            (smear-cursor-tests--engine-anim [99.9 0.0 10.0 21.0]
                                             [100.0 0.0 10.0 21.0]))
      (let ((n 0))
        (while (and smear-cursor--anim (< n 50))
          (smear-cursor--tick)
          (setq n (1+ n))))
      (should (null smear-cursor--anim))
      (should-not (local-variable-p 'cursor-type)))))

(ert-deftest smear-cursor-test-tick-stops-on-invalid-context ()
  ;; GIVEN a live animation whose context has become invalid
  ;; WHEN a tick runs
  ;; THEN the animation stops immediately
  (with-temp-buffer
    (cl-letf (((symbol-function 'smear-cursor--context-valid-p)
               (lambda (_anim) nil)))
      (setq smear-cursor--anim
            (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                             [100.0 0.0 10.0 21.0]))
      (smear-cursor--tick)
      (should (null smear-cursor--anim)))))

(ert-deftest smear-cursor-test-max-cells-aborts ()
  ;; GIVEN smear-cursor-max-cells of 3 and a quad covering many cells
  ;; WHEN a tick paints the first frame
  ;; THEN the animation aborts
  (with-temp-buffer
    (let ((smear-cursor-max-cells 3)
          (smear-cursor-max-length 1000)) ; keep the clamp out of the way
      (cl-letf (((symbol-function 'smear-cursor--row-info)
                 (lambda (_anim _row) '(1 1000 0)))
                ((symbol-function 'smear-cursor--resolve-cell)
                 (lambda (anim col row &optional _no-pad)
                   (let ((cw (smear-cursor--anim-cw anim)))
                     (smear-cursor--cell-create
                      :data (make-vector (* cw (smear-cursor--anim-ch anim)) 0)
                      :image (list 'image :type 'canvas) :overlay nil
                      :bg [0 0 0] :bgpix #xFF000000
                      :stamp -1 :col col :row row
                      :x (* col cw) :w cw))))
                ((symbol-function 'canvas-refresh) #'ignore)
                ((symbol-function 'smear-cursor--context-valid-p)
                 (lambda (_anim) t)))
        (setq smear-cursor--anim
              (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                               [400.0 0.0 10.0 21.0]))
        (smear-cursor--tick)
        (should (null smear-cursor--anim))))))

(ert-deftest smear-cursor-test-blank-frames-keep-the-cursor ()
  ;; GIVEN rows the grid cannot paint in, such as proportional org
  ;;       headings, where every cell is skipped
  ;; WHEN the animation ticks
  ;; THEN the real cursor is never hidden, and the animation gives up
  ;;      rather than running invisibly to convergence
  (with-temp-buffer
    (cl-letf (((symbol-function 'smear-cursor--paint-frame) (lambda (_anim) 0))
              ((symbol-function 'smear-cursor--context-valid-p) (lambda (_anim) t)))
      (setq smear-cursor--anim
            (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                             [400.0 0.0 10.0 21.0] 'show))
      (smear-cursor--tick)
      (should smear-cursor--anim)
      (should-not (local-variable-p 'cursor-type))
      (smear-cursor--tick)
      (should (null smear-cursor--anim))
      (should-not (local-variable-p 'cursor-type)))))

(ert-deftest smear-cursor-test-first-painted-frame-hides-the-cursor ()
  ;; GIVEN an animation that has not painted yet
  ;; WHEN a frame paints cells
  ;; THEN the real cursor is hidden from that frame on, and restored on stop
  (with-temp-buffer
    (cl-letf (((symbol-function 'smear-cursor--paint-frame) (lambda (_anim) 4))
              ((symbol-function 'smear-cursor--context-valid-p) (lambda (_anim) t)))
      (setq smear-cursor--anim
            (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                             [400.0 0.0 10.0 21.0] 'show))
      (should-not (local-variable-p 'cursor-type))
      (smear-cursor--tick)
      (should (local-variable-p 'cursor-type))
      (should (null cursor-type))
      (smear-cursor--stop)
      (should-not (local-variable-p 'cursor-type)))))

(ert-deftest smear-cursor-test-single-blank-frame-does-not-flicker ()
  ;; GIVEN an animation that paints, then misses one frame, then paints
  ;; WHEN it ticks through all three
  ;; THEN it stays alive and the cursor stays hidden, with no one-frame
  ;;      blink
  (with-temp-buffer
    (let* ((counts (list 3 0 3)) (n 0))
      (cl-letf (((symbol-function 'smear-cursor--paint-frame)
                 (lambda (_anim) (prog1 (or (nth n counts) 0) (setq n (1+ n)))))
                ((symbol-function 'smear-cursor--context-valid-p) (lambda (_anim) t)))
        (setq smear-cursor--anim
              (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                               [400.0 0.0 10.0 21.0] 'show))
        (dotimes (_ 3) (smear-cursor--tick))
        (should smear-cursor--anim)
        (should (null cursor-type))
        (smear-cursor--stop)
        (should-not (local-variable-p 'cursor-type))))))

(ert-deftest smear-cursor-test-scan-row-stops-at-the-window-edge ()
  ;; GIVEN a line far longer than the window is wide
  ;; WHEN the row's layout is worked out
  ;; THEN only the characters that could be drawn on are examined:
  ;;      cells are claimed for pixel columns inside the window, so
  ;;      classifying the rest of a 200k-character log line bought
  ;;      nothing and cost over a hundred milliseconds a frame
  (with-temp-buffer
    (insert (make-string 200000 ?x) "\n")
    (let ((examined 0)
          (anim (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                                 [0.0 0.0 10.0 21.0] 'show)))
      (cl-letf (((symbol-function 'line-pixel-height) (lambda () 21))
                ((symbol-function 'pos-visible-in-window-p)
                 (lambda (&rest _) (list 0 0)))
                ((symbol-function 'invisible-p)
                 (lambda (_p) (setq examined (1+ examined)) nil)))
        (should (smear-cursor--scan-row anim 0 (point-min) 0))
        ;; win-w 800 at cw 10 is 80 columns, plus one for the boundary
        (should (> examined 0))
        (should (<= examined 82))))))

(ert-deftest smear-cursor-test-frame-budget-stops-a-slow-animation ()
  ;; GIVEN an animation whose frames cost more than the budget allows
  ;; WHEN ticks run
  ;; THEN it gives up rather than paying the same cost for every
  ;;      remaining frame with redisplay blocked.  The smear is
  ;;      decoration; an unresponsive Emacs is not.
  ;;
  ;;      Not on the first frame, which builds a pane for every row the
  ;;      smear crosses and costs what the whole journey costs to set
  ;;      up; on the second, by which point the expense is the smear's
  ;;      running cost rather than its setup.
  (with-temp-buffer
    (cl-letf (((symbol-function 'smear-cursor--paint-frame)
               (lambda (_anim) (sleep-for 0.05) 3))
              ((symbol-function 'smear-cursor--context-valid-p)
               (lambda (_anim) t)))
      (let ((smear-cursor-frame-budget 0.02))
        (setq smear-cursor--anim
              (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                               [400.0 0.0 10.0 21.0] 'show))
        (smear-cursor--tick)
        (should smear-cursor--anim)          ; the setup frame is forgiven
        (smear-cursor--tick)
        (should (null smear-cursor--anim))))))

(ert-deftest smear-cursor-test-frame-budget-leaves-fast-frames-alone ()
  ;; GIVEN an animation whose frames are comfortably inside the budget
  ;; WHEN a tick runs
  ;; THEN the animation carries on
  (with-temp-buffer
    (cl-letf (((symbol-function 'smear-cursor--paint-frame) (lambda (_anim) 3))
              ((symbol-function 'smear-cursor--context-valid-p)
               (lambda (_anim) t)))
      (let ((smear-cursor-frame-budget 0.02))
        (setq smear-cursor--anim
              (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                               [400.0 0.0 10.0 21.0] 'show))
        (smear-cursor--tick)
        (should smear-cursor--anim)
        (smear-cursor--stop)))))

(ert-deftest smear-cursor-test-offscreen-frames-are-not-blank-frames ()
  ;; GIVEN an animation whose quad has not reached the window yet, as
  ;;       one crossing in from another window starts outside it
  ;; WHEN several frames in a row paint nothing
  ;; THEN it stays alive rather than being read as having nothing to
  ;;      draw: without this, windmove onto a cursor near the edge it
  ;;      came from gave up before the smear ever arrived
  (with-temp-buffer
    (cl-letf (((symbol-function 'smear-cursor--paint-frame)
               (lambda (_anim) (setq smear-cursor--offscreen t) 0))
              ((symbol-function 'smear-cursor--context-valid-p)
               (lambda (_anim) t)))
      (setq smear-cursor--anim
            (smear-cursor-tests--engine-anim [-400.0 0.0 10.0 21.0]
                                             [400.0 0.0 10.0 21.0] 'show))
      (dotimes (_ 4) (smear-cursor--tick))
      (should smear-cursor--anim)
      (smear-cursor--stop))))

(ert-deftest smear-cursor-test-blank-frames-on-screen-still-stop ()
  ;; GIVEN a quad over the window that paints nothing twice running,
  ;;       over rows the grid cannot use at all
  ;; WHEN it ticks
  ;; THEN it gives up, as before: only being off screen excuses a blank
  ;;      frame, and burning frames on an invisible quad helps no one
  (with-temp-buffer
    (cl-letf (((symbol-function 'smear-cursor--paint-frame) (lambda (_anim) 0))
              ((symbol-function 'smear-cursor--context-valid-p)
               (lambda (_anim) t)))
      (setq smear-cursor--anim
            (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                             [400.0 0.0 10.0 21.0] 'show))
      (dotimes (_ 3) (smear-cursor--tick))
      (should (null smear-cursor--anim)))))

(ert-deftest smear-cursor-test-tick-aborts-on-signal ()
  ;; GIVEN a live animation whose paint step signals
  ;; WHEN a tick runs
  ;; THEN the signal does not escape the timer callback, the animation
  ;;      is torn down, and the buffer's cursor is restored
  (with-temp-buffer
    (cl-letf (((symbol-function 'smear-cursor--paint-frame)
               (lambda (_anim) (signal 'arith-error nil)))
              ((symbol-function 'smear-cursor--context-valid-p)
               (lambda (_anim) t)))
      (setq smear-cursor--anim
            (smear-cursor-tests--engine-anim [0.0 0.0 10.0 21.0]
                                             [100.0 0.0 10.0 21.0]))
      (let ((inhibit-message t))
        (smear-cursor--tick))
      (should (null smear-cursor--anim))
      (should-not (local-variable-p 'cursor-type)))))

(ert-deftest smear-cursor-test-start-clamps-zero-row-pitch ()
  ;; GIVEN a target rect whose height rounds to zero
  ;; WHEN an animation starts on it
  ;; THEN the row pitch is clamped to at least one pixel, so the
  ;;      rasterizer's row division never divides by zero
  (unwind-protect
      (progn
        (smear-cursor--start (selected-window)
                             [0.0 0.0 10.0 21.0] [100.0 0.0 10.0 0.0])
        (should smear-cursor--anim)
        (should (= (smear-cursor--anim-lh smear-cursor--anim) 1))
        (should (= (smear-cursor--anim-y-base smear-cursor--anim) 0))
        (should (>= (smear-cursor--anim-ch smear-cursor--anim) 1)))
    (smear-cursor--stop)))

;;;; Trigger

(ert-deftest smear-cursor-test-distance-threshold ()
  ;; GIVEN two rects one cell apart horizontally on a 10x21 grid
  ;; WHEN the movement distance is computed in cells
  ;; THEN it is 1.0 (below the default 1.5 threshold)
  ;;      AND a two-line vertical move computes 2.0
  (should (< (abs (- (smear-cursor--move-cells
                      [0.0 0.0 10.0 21.0] [10.0 0.0 10.0 21.0] 10 21)
                     1.0))
             1e-9))
  (should (< (abs (- (smear-cursor--move-cells
                      [0.0 0.0 10.0 21.0] [0.0 42.0 10.0 21.0] 10 21)
                     2.0))
             1e-9)))

(ert-deftest smear-cursor-test-point-rect-rejects-zero-height ()
  ;; GIVEN a visible point sitting on a zero-height display line (a
  ;;       newline carrying `line-height' t, say)
  ;; WHEN point's rect is sampled
  ;; THEN it is nil, since that row has no pitch for the cell grid
  ;;      AND a normal row still yields a rect carrying its height
  (cl-letf (((symbol-function 'pos-visible-in-window-p)
             (lambda (&rest _) '(0 42))))
    (cl-letf (((symbol-function 'line-pixel-height) (lambda () 0)))
      (should (null (smear-cursor--point-rect (selected-window)))))
    (cl-letf (((symbol-function 'line-pixel-height) (lambda () 21)))
      (let ((rect (smear-cursor--point-rect (selected-window))))
        (should rect)
        (should (= (aref rect 3) 21.0))))))

(ert-deftest smear-cursor-test-mode-enables-hooks ()
  ;; GIVEN smear-cursor-mode off and no leftover animation state
  ;; WHEN the mode is enabled and then disabled
  ;; THEN the post-command hook is added and removed
  (smear-cursor--reset-state)
  (smear-cursor-mode 1)
  (should (memq #'smear-cursor--post-command post-command-hook))
  (smear-cursor-mode -1)
  (should-not (memq #'smear-cursor--post-command post-command-hook))
  (should (null smear-cursor--anim)))

(ert-deftest smear-cursor-test-rect-in-window-translates-between-bodies ()
  ;; GIVEN two window bodies sitting at different places in the frame
  ;; WHEN a rect measured in one is restated in the other
  ;; THEN it is offset by the distance between the bodies, landing
  ;;      outside the destination, which is what makes the smear reach
  ;;      in from the edge the cursor came from
  (cl-letf (((symbol-function 'window-edges)
             (lambda (win &rest _)
               (pcase win
                 ('left '(0 0 500 800))
                 ('right '(516 0 1016 800))))))
    (should (equal (smear-cursor--rect-in-window (vector 100.0 84.0 10.0 21.0)
                                                 'left 'right)
                   (vector -416.0 84.0 10.0 21.0)))
    (should (equal (smear-cursor--rect-in-window (vector 100.0 84.0 10.0 21.0)
                                                 'right 'left)
                   (vector 616.0 84.0 10.0 21.0)))))

(ert-deftest smear-cursor-test-sample-smears-across-a-window-switch ()
  ;; GIVEN point sampled in one window, then a second window selected
  ;;       with point in it exactly where it already was
  ;; WHEN the sample runs
  ;; THEN the smear starts from where the cursor really was, restated in
  ;;      the destination's pixels.  `windmove-down' and friends move no
  ;;      point at either end, so tracking rects per window alone saw
  ;;      nothing happen and animated nothing.
  (let ((started nil))
    (save-window-excursion
      (delete-other-windows)
      (let* ((top (selected-window))
             (bottom (split-window-below)))
        (cl-letf (((symbol-function 'smear-cursor--start)
                   (lambda (win old new) (push (list win old new) started)))
                  ((symbol-function 'smear-cursor--point-rect)
                   (lambda (_win) (vector 0.0 0.0 10.0 21.0)))
                  ((symbol-function 'window-edges)
                   (lambda (win &rest _)
                     (if (eq win top) '(0 0 800 400) '(0 400 800 800))))
                  ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'image-type-available-p) (lambda (_type) t)))
          (let ((smear-cursor-mode t)
                (smear-cursor--last-window nil)
                (smear-cursor--last-rects (make-hash-table :test 'eq)))
            (select-window top)
            (smear-cursor--sample)
            (should (null started))
            (select-window bottom)
            (smear-cursor--sample)
            (should (= (length started) 1))
            (let ((call (car started)))
              (should (eq (nth 0 call) bottom))
              ;; the cursor came from 400 px above this window's own top
              (should (= (aref (nth 1 call) 1) -400.0)))))))))

(ert-deftest smear-cursor-test-sample-still-smears-within-a-window ()
  ;; GIVEN point moving inside the one selected window
  ;; WHEN samples run
  ;; THEN the ordinary case still smears from the previous position,
  ;;      so the crossing case has not taken it over
  (let ((started nil) (rect (vector 0.0 0.0 10.0 21.0)))
    (cl-letf (((symbol-function 'smear-cursor--start)
               (lambda (win old new) (push (list win old new) started)))
              ((symbol-function 'smear-cursor--point-rect) (lambda (_win) rect))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (_type) t)))
      (let ((smear-cursor-mode t)
            (smear-cursor--last-window nil)
            (smear-cursor--last-rects (make-hash-table :test 'eq)))
        (smear-cursor--sample)
        (should (null started))
        (setq rect (vector 0.0 210.0 10.0 21.0))
        (smear-cursor--sample)
        (should (= (length started) 1))
        (should (equal (nth 1 (car started)) (vector 0.0 0.0 10.0 21.0)))))))

;;;; Blend lookup tables

(ert-deftest smear-cursor-test-blend-lut-gamma ()
  ;; GIVEN a background pixel and smear color
  ;; WHEN the 33-level gamma-corrected blend LUT is built
  ;; THEN endpoints are exact and the midpoint matches gamma-2.2 mixing
  (let ((lut (smear-cursor--make-lut [10 20 30] [200 100 50])))
    (should (= (length lut) 33))
    (should (= (aref lut 0) #xFF0A141E))
    (should (= (aref lut 32) #xFFC86432))
    ;; gamma mid: round(255 * ((0.5*(10/255)^2.2 + 0.5*(200/255)^2.2)^(1/2.2)))
    (let ((mid (aref lut 16)))
      (dolist (pair '((16 10 200) (8 20 100) (0 30 50)))
        (let* ((shift (nth 0 pair)) (bgc (nth 1 pair)) (fgc (nth 2 pair))
               (want (round (* 255.0
                               (expt (+ (* 0.5 (expt (/ bgc 255.0) 2.2))
                                        (* 0.5 (expt (/ fgc 255.0) 2.2)))
                                     (/ 1.0 2.2))))))
          (should (= (logand (ash mid (- shift)) #xFF) want)))))))

;;;; Reload safety

(ert-deftest smear-cursor-test-reset-state-clears-everything ()
  ;; GIVEN stray state from a previous incarnation of the package:
  ;;       pooled records, a live tick timer, tagged overlays
  ;; WHEN smear-cursor--reset-state runs (as it does at load time)
  ;; THEN the pool is empty, no tick/sample timers remain, and tagged
  ;;      overlays are deleted from all buffers
  (with-temp-buffer
    (let ((ov (make-overlay 1 1)))
      (overlay-put ov 'smear-cursor t)
      (puthash 8 (list 'stale-record) smear-cursor--pool)
      (setq smear-cursor--pool-size 17)
      (run-at-time 9 nil #'smear-cursor--tick)
      (smear-cursor--reset-state)
      (should (zerop (hash-table-count smear-cursor--pool)))
      (should (null smear-cursor--pool-size))
      (should (null smear-cursor--anim))
      (should-not (cl-find-if
                   (lambda (tm) (eq (timer--function tm)
                                    #'smear-cursor--tick))
                   timer-list))
      (should-not (overlay-buffer ov)))))

(ert-deftest smear-cursor-test-paint-y-base-offset-grid ()
  ;; GIVEN a grid anchored at y-base 7 with row pitch 18 and canvas
  ;;       height 17 (line-spacing setups shift row tops off multiples
  ;;       of the pitch)
  ;; WHEN a quad exactly covering grid cell (0,1) (pixels y 25..41)
  ;;      is painted
  ;; THEN that cell is claimed with 8x17 data, glyph pixels are the
  ;;      smear color, and no cell exists for the misaligned row 0
  (let ((anim (smear-cursor--anim-create
               :window nil :buffer nil
               :corners (make-vector 8 0.0)
               :target [0.0 25.0 8.0 17.0]
               :cells (make-hash-table :test 'eql)
               :pads (make-hash-table :test 'eql)
               :cw 8 :lh 18 :ch 17 :y-base 7
               :win-w 800 :win-h 420
               :color [255 0 0] :fade (vector 0.0 0.0 1.0)
               :frame 0)))
    (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                     [0.0 25.0 8.0 17.0])
    (smear-cursor-tests--with-stub-cells _refreshed
      (smear-cursor--paint-frame anim)
      (let ((cell (gethash (smear-cursor--cell-key 0 1)
                           (smear-cursor--anim-cells anim))))
        (should (smear-cursor--cell-p cell))
        (should (= (length (smear-cursor--cell-data cell)) (* 8 17)))
        ;; interior pixel: cell-local (4, 8) -> full smear color
        (should (= (aref (smear-cursor--cell-data cell) (+ (* 8 8) 4))
                   #xFFFF0000))
        (should-not (gethash (smear-cursor--cell-key 0 0)
                             (smear-cursor--anim-cells anim)))))))

;;;; Text moving under a live animation

(ert-deftest smear-cursor-test-revalidate-drops-cells-with-rows ()
  ;; GIVEN a live animation holding a resolved cell for row 0, whose x
  ;;       and w were measured against the buffer as it then stood
  ;; WHEN the buffer's text changes under it (a terminal redrawing
  ;;      itself) and the frame revalidates
  ;; THEN the cell cache is dropped along with the row cache: a cell
  ;;      records a glyph box, and the glyph boxes just moved
  (with-temp-buffer
    (insert "hello world\n")
    (let ((anim (smear-cursor--anim-create
                 :buffer (current-buffer)
                 :tick (buffer-chars-modified-tick)
                 :cells (make-hash-table :test 'eql)
                 :pads (make-hash-table :test 'eql)
                 :rows (make-hash-table :test 'eql)
                 :cw 8 :lh 18 :ch 18)))
      (cl-letf (((symbol-function 'canvas-refresh) #'ignore))
        (puthash (smear-cursor--cell-key 3 0)
                 (smear-cursor--cell-create
                  :data (make-vector (* 8 18) 0) :image nil :overlay nil
                  :bg [0 0 0] :bgpix #xFF000000 :stamp -1
                  :col 3 :row 0 :x 24 :w 8)
                 (smear-cursor--anim-cells anim))
        (puthash 0 '(1 12 0) (smear-cursor--anim-rows anim))
        (goto-char (point-min))
        (insert "> ")                   ; the line shifts right by two
        (smear-cursor--revalidate anim)
        (should (= (hash-table-count (smear-cursor--anim-rows anim)) 0))
        (should (= (hash-table-count (smear-cursor--anim-cells anim)) 0))))))

(ert-deftest smear-cursor-test-paint-skips-a-cell-that-misses-the-pixel ()
  ;; GIVEN a cell resolution that answers with a cell whose span does
  ;;       not contain the pixel asked about, which is how a stale
  ;;       cached cell looks once the row it was measured on has moved
  ;; WHEN a frame is painted over it
  ;; THEN the painter skips the pixel instead of indexing past the end
  ;;      of the canvas and tearing the animation down
  (let ((anim (smear-cursor-tests--fake-anim
               [0.0 0.0 10.0 21.0] [0.0 0.0 10.0 21.0])))
    (cl-letf (((symbol-function 'smear-cursor--row-info)
               (lambda (_anim _row) '(1 1000 0)))
              ((symbol-function 'canvas-refresh) #'ignore)
              ((symbol-function 'smear-cursor--resolve-cell)
               (lambda (anim col row &optional _no-pad)
                 ;; claimed when the glyph sat 9 pixels to the left
                 (let ((cw (smear-cursor--anim-cw anim)))
                   (smear-cursor--cell-create
                    :data (make-vector (* cw (smear-cursor--anim-ch anim)) 0)
                    :image (list 'image :type 'canvas) :overlay nil
                    :bg [0 0 0] :bgpix #xFF000000 :stamp -1
                    :col col :row row :x (- (* col cw) 9) :w cw)))))
      (should (smear-cursor--paint-frame anim)))))

;;;; Backend

(ert-deftest smear-cursor-test-backend-dispatch-defaults-to-cpu ()
  ;; GIVEN the default backend
  ;; WHEN a frame is painted
  ;; THEN the Lisp rasterizer runs and the X module is never consulted:
  ;;      the cpu path has to work on machines with no X at all
  (let ((smear-cursor-backend 'cpu) (ran nil) (asked nil))
    (cl-letf (((symbol-function 'smear-cursor--paint-frame-cpu)
               (lambda (_a) (setq ran t) 0))
              ((symbol-function 'smear-cursor--paint-frame-x11)
               (lambda (_a) (setq asked t) t)))
      (smear-cursor--paint-frame (smear-cursor--anim-create))
      (should ran)
      (should-not asked))))

(ert-deftest smear-cursor-test-the-backends-are-cpu-and-x11 ()
  ;; GIVEN the backend setting
  ;; WHEN its choices are listed
  ;; THEN they are the two renderers this package ships.  A choice
  ;;      that needs a module from somewhere else falls back the moment
  ;;      it is picked, which reads as a broken package.
  (should (equal '(cpu x11)
                 (mapcar (lambda (choice) (car (last choice)))
                         (cdr (get 'smear-cursor-backend 'custom-type))))))

(ert-deftest smear-cursor-test-an-unknown-backend-falls-back-to-cpu ()
  ;; GIVEN a backend the package does not know, such as `gpu' saved by
  ;;       an older version
  ;; WHEN a frame is painted
  ;; THEN the Lisp rasterizer draws it rather than nothing drawing it
  (let ((smear-cursor-backend 'gpu) (ran nil))
    (cl-letf (((symbol-function 'smear-cursor--paint-frame-cpu)
               (lambda (_a) (setq ran t) 0)))
      (smear-cursor--paint-frame (smear-cursor--anim-create))
      (should ran))))

(ert-deftest smear-cursor-test-face-bg-follows-a-remap ()
  ;; GIVEN a buffer where `lin' has remapped `hl-line', leaving the
  ;;       original name on the overlay and listing it among the
  ;;       replacements as (hl-line lin-blue hl-line)
  ;; WHEN the background of that face is read
  ;; THEN the colour the buffer really shows is returned, not the one
  ;;      `face-background' reports for the unremapped name.  A cell
  ;;      claimed against the wrong colour is a visible block sitting on
  ;;      the current line, and the self-reference must not loop.
  (with-temp-buffer
    (setq-local face-remapping-alist
                '((hl-line smear-cursor-test-lin hl-line)))
    (custom-declare-face 'smear-cursor-test-lin
                         '((t :background "#242679")) "")
    (should (equal (smear-cursor--face-bg 'hl-line) "#242679"))))

(ert-deftest smear-cursor-test-face-bg-handles-lists-and-inherit ()
  ;; GIVEN the shapes a `face' property actually takes
  ;; WHEN each is resolved
  ;; THEN the first entry specifying a background wins, an anonymous
  ;;      face falls back to what it inherits, and a legacy colour cons
  ;;      is understood
  (with-temp-buffer
    (custom-declare-face 'smear-cursor-test-base
                         '((t :background "#123456")) "")
    (should (equal (smear-cursor--face-bg
                    '(:foreground "red" :inherit smear-cursor-test-base))
                   "#123456"))
    (should (equal (smear-cursor--face-bg
                    (list '(:foreground "red") '(:background "#abcdef")))
                   "#abcdef"))
    (should (equal (smear-cursor--face-bg '(background-color . "#fedcba"))
                   "#fedcba"))
    (should-not (smear-cursor--face-bg nil))))

(ert-deftest smear-cursor-test-no-pad-refuses-to-lengthen-a-line ()
  ;; GIVEN a two-character line and a request for a cell well past its end
  ;; WHEN resolution is asked to do it without padding
  ;; THEN it declines, and no after-string overlay is created.  Padding a
  ;;      line hangs blank columns off it for as long as the animation
  ;;      runs, which is right for a smear crossing a short line and
  ;;      wrong for a stray ember to the right of one.
  (with-temp-buffer
    (insert "ab\n")
    (let ((anim (smear-cursor--anim-create
                 :buffer (current-buffer) :window nil
                 :cells (make-hash-table :test 'eql)
                 :pads (make-hash-table :test 'eql)
                 :cw 10 :lh 21 :ch 21 :y-base 0)))
      (cl-letf (((symbol-function 'smear-cursor--row-info)
                 (lambda (_a _r) (list 1 3 0)))   ; bol 1, eol 3, x 0
                ((symbol-function 'canvas-refresh) #'ignore))
        (should (eq (smear-cursor--resolve-cell anim 8 0 'no-pad) 'skip))
        (should (= (hash-table-count (smear-cursor--anim-pads anim)) 0))
        (should-not (cl-some (lambda (o) (overlay-get o 'after-string))
                             (overlays-in (point-min) (point-max))))))))

(ert-deftest smear-cursor-test-pads-do-not-pile-up-across-rows ()
  ;; GIVEN a smear that padded the end of one row, as it does when the
  ;;       cursor sits past the text
  ;; WHEN a later frame paints a different row instead, as moving down
  ;;       a line does, since that reuses the running animation and
  ;;       reuse does not flush cells when the row pitch is unchanged
  ;; THEN the abandoned row's padding is taken off screen.  Clearing its
  ;;      canvases is not enough: the background they hold was sampled
  ;;      when the pad was made, and the current-line highlight has since
  ;;      moved off that line, leaving a block of stale colour at its end.
  (let ((anim (smear-cursor-tests--fake-anim [0.0 0.0 10.0 21.0]
                                             [0.0 0.0 10.0 21.0]))
        (dead nil))
    (smear-cursor-tests--with-stub-cells _refreshed
      (cl-letf (((symbol-function 'delete-overlay)
                 (lambda (o) (push o dead))))
        ;; row 0 has padding and a cell painted on this frame
        (puthash 0 (cons 3 'row0-pad) (smear-cursor--anim-pads anim))
        (puthash 3 (cons 3 'row3-pad) (smear-cursor--anim-pads anim))
        (let ((cell (smear-cursor--cell-create
                     :data (make-vector 10 0) :image nil :overlay nil
                     :bg [0 0 0] :bgpix 0 :stamp 7 :col 0 :row 0 :x 0 :w 10)))
          (puthash (smear-cursor--cell-key 0 0) cell
                   (smear-cursor--anim-cells anim)))
        (cl-letf (((symbol-function 'overlayp) (lambda (o) (memq o '(row0-pad row3-pad)))))
          (smear-cursor--finish-frame anim 7))
        ;; row 0 still has a live cell; row 3 does not
        (should (gethash 0 (smear-cursor--anim-pads anim)))
        (should-not (gethash 3 (smear-cursor--anim-pads anim)))
        (should (equal dead '(row3-pad)))))))

(ert-deftest smear-cursor-test-line-ends-can-refuse-padding ()
  ;; GIVEN a short line and a smear reaching past its end
  ;; WHEN cells are resolved with padding refused
  ;; THEN no after-string is hung off the line.  One there puts blank
  ;;      columns between point and the text that follows it, so a
  ;;      character typed at the end of that line lands to the right of
  ;;      them.  That is visible, even though it is display-only.
  (with-temp-buffer
    (insert "ab\n")
    (let ((anim (smear-cursor--anim-create
                 :buffer (current-buffer) :window nil
                 :cells (make-hash-table :test 'eql)
                 :pads (make-hash-table :test 'eql)
                 :cw 10 :lh 21 :ch 21 :y-base 0)))
      (cl-letf (((symbol-function 'smear-cursor--row-info)
                 (lambda (_a _r) (list 1 3 0)))
                ((symbol-function 'canvas-refresh) #'ignore))
        ;; the smear pads by default; this test is about the refusal
        (should (default-value 'smear-cursor-pad-line-ends))
        (should (eq (smear-cursor--resolve-cell anim 8 0 t) 'skip))
        (should-not (cl-some (lambda (o) (overlay-get o 'after-string))
                             (overlays-in (point-min) (point-max))))
        ;; and with padding asked for, it appears
        (should (smear-cursor--cell-p (smear-cursor--resolve-cell anim 8 0 nil)))
        (should (cl-some (lambda (o) (overlay-get o 'after-string))
                         (overlays-in (point-min) (point-max))))))))

(ert-deftest smear-cursor-test-padding-never-touches-buffer-text ()
  ;; GIVEN a buffer and a smear that pads a line end
  ;; WHEN the padding is created
  ;; THEN the text is byte-for-byte what it was and the buffer is not
  ;;      modified: the blank columns are an overlay's after-string, not
  ;;      an insertion, whatever they look like on screen
  (with-temp-buffer
    (insert "ab\n")
    (set-buffer-modified-p nil)
    (let ((before (buffer-string))
          (tick (buffer-chars-modified-tick))
          (anim (smear-cursor--anim-create
                 :buffer (current-buffer) :window nil
                 :cells (make-hash-table :test 'eql)
                 :pads (make-hash-table :test 'eql)
                 :cw 10 :lh 21 :ch 21 :y-base 0)))
      (cl-letf (((symbol-function 'smear-cursor--row-info)
                 (lambda (_a _r) (list 1 3 0)))
                ((symbol-function 'canvas-refresh) #'ignore))
        (smear-cursor--resolve-cell anim 8 0 nil))
      (should (equal (buffer-string) before))
      (should (= (buffer-chars-modified-tick) tick))
      (should-not (buffer-modified-p)))))

(ert-deftest smear-cursor-test-pad-lets-typing-go-in-front-of-it ()
  ;; GIVEN padding hung off the end of a line
  ;; WHEN a character is typed there
  ;; THEN the padding ends up after it, not between point and the text.
  ;;      Without FRONT-ADVANCE the overlay stays put and what you type
  ;;      appears to the right of a run of blank columns.
  (with-temp-buffer
    (insert "ab\n")
    (let ((anim (smear-cursor--anim-create
                 :buffer (current-buffer) :window nil
                 :cells (make-hash-table :test 'eql)
                 :pads (make-hash-table :test 'eql)
                 :cw 10 :lh 21 :ch 21 :y-base 0)))
      (cl-letf (((symbol-function 'smear-cursor--row-info)
                 (lambda (_a _r) (list 1 3 0)))
                ((symbol-function 'canvas-refresh) #'ignore))
        (smear-cursor--resolve-cell anim 5 0)          ; pads out to column 5
        (let ((ov (cdr (gethash 0 (smear-cursor--anim-pads anim)))))
          (should (overlayp ov))
          (should (= (overlay-start ov) 3))
          (goto-char 3)
          (insert "c")
          ;; the overlay moved along with point rather than staying behind it
          (should (= (overlay-start ov) 4)))))))

(ert-deftest smear-cursor-test-pad-columns-stay-inside-the-window ()
  ;; GIVEN a horizontally scrolled window, where the line's end sits at a
  ;;       negative x
  ;; WHEN padding is worked out for it
  ;; THEN it starts at column zero rather than running off the left edge.
  ;;      Dividing a negative x by the cell width gives a negative first
  ;;      column, and the span from there to the one asked for is
  ;;      enormous: dozens of cells, none near the character asking.
  (with-temp-buffer
    (insert "abcdefghij\n")
    (let ((anim (smear-cursor--anim-create
                 :buffer (current-buffer) :window nil
                 :cells (make-hash-table :test 'eql)
                 :pads (make-hash-table :test 'eql)
                 :cw 10 :lh 21 :ch 21 :y-base 0 :win-w 400)))
      (cl-letf (((symbol-function 'canvas-refresh) #'ignore))
        (smear-cursor--ensure-pad anim 0 11 -60 3)
        (let ((n (car (gethash 0 (smear-cursor--anim-pads anim)))))
          (should (and n (<= n 4))))))))

(ert-deftest smear-cursor-test-every-kind-of-deletion-is-the-same-kind ()
  ;; GIVEN the commands people actually delete text with
  ;; WHEN each is run
  ;; THEN each is drawn, with exactly the text it removed, and none of
  ;;      them is named anywhere in the code.  A deletion is a change
  ;;      that put nothing where something was; which command did it is
  ;;      not a distinction worth making, and making it would mean a new
  ;;      case for every command that ever removes text.
  (let (fired)
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (_kind beg end &rest _)
                 (push (buffer-substring-no-properties beg end) fired)))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_k) 'fade))
              ((symbol-function 'smear-cursor--delete-again-p) (lambda () t))
              ((symbol-function 'smear-cursor--own-edit-p) (lambda (&rest _) t)))
      (let ((smear-cursor-mode t)
            (smear-cursor-delete-min-chars 1)
            (before-change-functions '(smear-cursor--delete-fire))
            (after-change-functions nil))
        (with-temp-buffer
          (dolist (case '(("one two three" 14 backward-kill-word "three")
                          ("one two three" 14 delete-backward-char "e")
                          ("one two three"  1 kill-word "one")
                          ("one two three"  5 kill-line "two three")))
            (erase-buffer)
            (insert (nth 0 case))
            (goto-char (nth 1 case))
            ;; after the setup: `erase-buffer' is itself a deletion, and
            ;; is drawn like any other, which is rather the point
            (setq fired nil)
            (funcall (nth 2 case) 1)
            (should (equal fired (list (nth 3 case)))))
          ;; and a region kill, which takes its bounds from the mark
          (erase-buffer) (insert "alpha beta gamma")
          (setq fired nil)
          (kill-region 7 11)
          (should (equal fired '("beta"))))))))

(ert-deftest smear-cursor-test-two-hiders-give-the-cursor-back ()
  ;; GIVEN the cursor hidden twice over: the smear and a deletion both
  ;;       do it, and they overlap, since deleting moves point and
  ;;       moving point starts a smear
  ;; WHEN both are restored, in either order
  ;; THEN the cursor is what it was.  Taking it away when it is already
  ;;      gone saves nil as the value to put back, and putting nil back
  ;;      hides it for good, which needs a reload of the file to undo.
  (with-temp-buffer
    (setq-local cursor-type 'bar)
    (let* ((first (smear-cursor--hide-cursor (current-buffer)))
           (second (smear-cursor--hide-cursor (current-buffer))))
      (should (equal first '(local . bar)))
      (should-not second)                      ; nothing left to take
      (should-not cursor-type)
      (smear-cursor--restore-cursor (current-buffer) second)
      (smear-cursor--restore-cursor (current-buffer) first)
      (should (eq cursor-type 'bar))))
  ;; and the other order, with no buffer-local value to begin with
  (with-temp-buffer
    (kill-local-variable 'cursor-type)
    (let* ((first (smear-cursor--hide-cursor (current-buffer)))
           (second (smear-cursor--hide-cursor (current-buffer))))
      (should (eq first 'global))
      (should-not second)
      (smear-cursor--restore-cursor (current-buffer) first)
      (smear-cursor--restore-cursor (current-buffer) second)
      (should-not (local-variable-p 'cursor-type)))))


(ert-deftest smear-cursor-test-face-attr-reads-foreground ()
  ;; GIVEN a face spec naming a foreground
  ;; WHEN the foreground is resolved
  ;; THEN it comes back, by the same rules the background already followed
  (should (equal (smear-cursor--face-attr '(:foreground "#ff0000") :foreground)
                 "#ff0000"))
  (should (equal (smear-cursor--face-attr '(foreground-color . "#00ff00")
                                          :foreground)
                 "#00ff00"))
  ;; and the first entry of a list that specifies one wins
  (should (equal (smear-cursor--face-attr
                  '((:background "#111111") (:foreground "#0000ff"))
                  :foreground)
                 "#0000ff")))

(ert-deftest smear-cursor-test-max-length-can-be-unlimited ()
  ;; GIVEN no cap set
  ;; WHEN the cap is asked for in pixels
  ;; THEN there is none, and the smear runs the whole way from where
  ;;      the cursor was to where it is.  The cap paid for laying the
  ;;      glyph block out in Lisp; the module builds it now.
  (let ((smear-cursor-max-length nil))
    (should (null (smear-cursor--max-length-px nil 8))))
  (let ((smear-cursor-max-length 25))
    (should (= (smear-cursor--max-length-px nil 8) 200.0))))

(ert-deftest smear-cursor-test-row-simple-p-walks-property-runs ()
  ;; GIVEN a line of plain ASCII, and one carrying a display property
  ;; WHEN each is checked for lying on the character grid
  ;; THEN the plain one passes and the other does not.  Walking
  ;;      property runs rather than characters gives the same answer in
  ;;      a seventeenth of the time.
  (with-temp-buffer
    (insert "signal output_ready : std_logic;")
    (should (smear-cursor--row-simple-p (point-min) (point-max)))
    (put-text-property (+ (point-min) 4) (+ (point-min) 6) 'display "XX")
    (should-not (smear-cursor--row-simple-p (point-min) (point-max))))
  (with-temp-buffer
    ;; anything outside printable ASCII
    (insert "signal café : std_logic;")
    (should-not (smear-cursor--row-simple-p (point-min) (point-max)))))

(ert-deftest smear-cursor-test-row-cannot-have-wrapped ()
  ;; GIVEN a window 20 cells wide
  ;; WHEN lines shorter and longer than that are asked about
  ;; THEN only the short one is known not to have wrapped.  That is
  ;;      what makes the next screen row provably the next logical
  ;;      line, so it can be resolved by arithmetic rather than by a
  ;;      0.8 ms `pos-visible-in-window-p'.
  (with-temp-buffer
    (insert "short line\n")
    (insert "a line that is very much longer than the window is wide\n")
    (let ((anim (smear-cursor--anim-create
                 :cw 8 :lh 18 :y-base 0 :win-w 160 :win-h 720
                 :buffer (current-buffer) :window nil)))
      (let* ((b1 (point-min))
             (e1 (save-excursion (goto-char b1) (line-end-position)))
             (b2 (save-excursion (goto-char (point-min)) (forward-line 1) (point)))
             (e2 (save-excursion (goto-char b2) (line-end-position))))
        (should (smear-cursor--row-unwrapped-p anim (list b1 e1 0)))
        (should-not (smear-cursor--row-unwrapped-p anim (list b2 e2 0)))
        ;; a line beginning past the left edge is the ordinary case with
        ;; `display-line-numbers' on.  It still cannot wrap, so long as
        ;; what follows the numbers fits
        (should (smear-cursor--row-unwrapped-p anim (list b1 e1 40)))
        ;; unless the numbers leave it too little room
        (should-not (smear-cursor--row-unwrapped-p anim (list b1 e1 120)))))))

(ert-deftest smear-cursor-test-grid-check-happens-once-per-animation ()
  ;; GIVEN a buffer with remapped faces, where the arithmetic that turns
  ;;       a pixel into a column has to be checked against a real glyph
  ;; WHEN many rows are resolved
  ;; THEN the check runs once and its answer is remembered.  It costs a
  ;;      display query, and a smear crossing eighteen rows was paying
  ;;      for eighteen of them.
  (with-temp-buffer
    (insert "hello world\n")
    (let ((anim (smear-cursor--anim-create
                 :cw 8 :lh 18 :y-base 0 :win-w 800 :win-h 720
                 :buffer (current-buffer) :window nil))
          (calls 0))
      (cl-letf (((symbol-function 'pos-visible-in-window-p)
                 (lambda (pos &rest _)
                   (setq calls (1+ calls))
                   (list (* 8 (- pos (point-min))) 0))))
        (dotimes (_ 5)
          (should (smear-cursor--grid-holds-p
                   anim nil (point-min) (+ (point-min) 5) 0 8)))
        (should (= calls 1))))))

(ert-deftest smear-cursor-test-frame-budget-does-not-charge-for-gc ()
  ;; GIVEN a frame that spent most of its time in garbage collection
  ;; WHEN its cost is weighed against the frame budget
  ;; THEN the collection is not counted against it.  A collection
  ;;      landing inside an animation costs 90 ms, which is three times
  ;;      the budget.  Counting it stops a long smear about a third of
  ;;      the time, on a pause that says nothing about whether the smear
  ;;      is affordable.
  (let ((smear-cursor-frame-budget 0.033))
    ;; 50 ms of frame, 40 ms of it collecting: affordable
    (should-not (smear-cursor--over-frame-budget-p 0.050 0.040))
    ;; 50 ms of frame, none of it collecting: not affordable
    (should (smear-cursor--over-frame-budget-p 0.050 0.0))
    ;; and no budget set means nothing is ever over it
    (let ((smear-cursor-frame-budget nil))
      (should-not (smear-cursor--over-frame-budget-p 9.0 0.0)))))

(ert-deftest smear-cursor-test-overshoot-is-capped ()
  ;; GIVEN a long jump, where the spring carries enough speed to throw a
  ;;       corner well past its destination
  ;; WHEN the cap is one cell
  ;; THEN no corner ever sits more than a cell beyond its own target.
  ;;      Uncapped, the overshoot grows with the distance jumped: 6.8
  ;;      cells across two dozen rows against 0.3 down a single line.
  ;;      A long jump then throws the head several characters past the
  ;;      cursor it is supposed to arrive with.
  (let* ((target [528.0 108.0 8.0 18.0])
         (c (make-vector 8 0.0)) (v (make-vector 8 0.0))
         (worst 0.0))
    (smear-cursor--corners-from-rect c [400.0 540.0 8.0 18.0])
    ;; the wind-up kick, as an animation starts with
    (dotimes (i 4)
      (aset v (* 2 i) (* -0.2 (- (smear-cursor--target-x target i)
                                 (aref c (* 2 i)))))
      (aset v (1+ (* 2 i)) (* -0.2 (- (smear-cursor--target-y target i)
                                      (aref c (1+ (* 2 i)))))))
    (dotimes (_ 60)
      (smear-cursor--ease-step c v target 1.0 0.2 0.85 3.0 1.0 '(8.0 . 8.0))
      (dotimes (i 4)
        ;; distance past the corner's own target, along each axis
        (let ((ox (- (aref c (* 2 i)) (smear-cursor--target-x target i)))
              (oy (- (aref c (1+ (* 2 i))) (smear-cursor--target-y target i))))
          ;; the quad travels up and right, so overshoot is +x and -y
          (setq worst (max worst ox (- oy))))))
    (should (<= worst 8.5))))

(ert-deftest smear-cursor-test-overshoot-uncapped-by-default-argument ()
  ;; GIVEN the same jump with no cap passed
  ;; WHEN the spring runs
  ;; THEN it overshoots as it always did, so the cap is opt-in and the
  ;;      easing tests that predate it still describe the same spring
  (let* ((target [528.0 108.0 8.0 18.0])
         (c (make-vector 8 0.0)) (v (make-vector 8 0.0))
         (worst 0.0))
    (smear-cursor--corners-from-rect c [400.0 540.0 8.0 18.0])
    (dotimes (i 4)
      (aset v (1+ (* 2 i)) (* -0.2 (- (smear-cursor--target-y target i)
                                      (aref c (1+ (* 2 i)))))))
    (dotimes (_ 60)
      (smear-cursor--ease-step c v target 1.0 0.2 0.85 3.0 1.0)
      (dotimes (i 4)
        (setq worst (max worst (- (smear-cursor--target-y target i)
                                  (aref c (1+ (* 2 i))))))))
    (should (> worst 20.0))))

(ert-deftest smear-cursor-test-overshoot-cap-is-per-axis ()
  ;; GIVEN the cap given as one fraction, as two, and as nil
  ;; WHEN it is turned into pixels for an 8 by 18 cell
  ;; THEN each axis is measured against its own side of the cursor.
  ;;      A cell is not square: half of one is four pixels across and
  ;;      nine down, and the same number of pixels means different
  ;;      things in the two directions.
  (let ((smear-cursor-max-overshoot 0.5))
    (should (equal (smear-cursor--overshoot-cap 8 18) '(4.0 . 9.0))))
  (let ((smear-cursor-max-overshoot '(0.0 . 0.5)))
    (should (equal (smear-cursor--overshoot-cap 8 18) '(0.0 . 9.0))))
  (let ((smear-cursor-max-overshoot nil))
    (should (null (smear-cursor--overshoot-cap 8 18)))))

(ert-deftest smear-cursor-test-sample-measures-with-its-own-pads-silent ()
  ;; GIVEN an animation still running, whose end-of-line padding hangs
  ;;       an after-string off the very line point sits at the end of
  ;; WHEN the next move samples where point is
  ;; THEN the padding goes quiet before the measurement.
  ;;
  ;;      An after-string is display width: asking the display engine
  ;;      where point is while one stands at that position answers with
  ;;      the string counted in.  Measured in a VHDL buffer, point at a
  ;;      line's end read 480 pixels instead of 400, ten cells out and
  ;;      the width of the padding.  The next smear is then aimed to the
  ;;      right of the cursor by however wide the last one was.
  (let ((order nil)
        (smear-cursor-mode t)
        (smear-cursor--anim
         (smear-cursor--anim-create
          :window nil :buffer (current-buffer)
          :cells (make-hash-table :test 'eql)
          :pads (make-hash-table :test 'eql))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'minibufferp) (lambda (&rest _) nil))
              ((symbol-function 'selected-window) (lambda () nil))
              ((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer)))
              ((symbol-function 'smear-cursor--without-pads)
               (lambda (_a fn)
                 (push 'pads-silenced order)
                 (prog1 (funcall fn) (push 'pads-back order))))
              ((symbol-function 'smear-cursor--point-rect)
               (lambda (_w) (push 'measured order) nil)))
      (smear-cursor--sample)
      ;; measured between the two, so nothing can redisplay without them
      (should (equal (nreverse order)
                     '(pads-silenced measured pads-back))))))

(ert-deftest smear-cursor-test-one-slow-frame-is-forgiven ()
  ;; GIVEN an animation whose setup frame overruns the budget
  ;; WHEN a later frame is affordable again
  ;; THEN it keeps running, and only a second overrun in a row stops it.
  ;;
  ;;      Setting a smear up costs what the whole journey costs: a pane
  ;;      for every row it crosses, and their glyphs.  That is 51 ms
  ;;      across forty-six rows against 1.5 ms a frame after it.  A
  ;;      retarget pays it again in the middle of a flight, so keying
  ;;      the forgiveness to the first frame is not enough.
  (let ((anim (smear-cursor--anim-create
               :cells (make-hash-table :test 'eql)
               :pads (make-hash-table :test 'eql))))
    (should (= (smear-cursor--anim-slow anim) 0))
    (cl-incf (smear-cursor--anim-slow anim))
    (should (< (smear-cursor--anim-slow anim) 2))   ; forgiven
    (setf (smear-cursor--anim-slow anim) 0)          ; an affordable frame
    (cl-incf (smear-cursor--anim-slow anim))
    (cl-incf (smear-cursor--anim-slow anim))
    (should (>= (smear-cursor--anim-slow anim) 2)))) ; two in a row: stop

(ert-deftest smear-cursor-test-a-late-frame-does-not-teleport-the-spring ()
  ;; GIVEN frames arriving on time, late, and very late
  ;; WHEN the spring is told how much time to advance
  ;; THEN a late one is advanced by a bounded amount.
  ;;
  ;;      Setting a long smear up costs tens of milliseconds, so the
  ;;      frame after it sees a gap of that size.  Advancing the spring
  ;;      by the whole of it collapses a thirty-one row trail to
  ;;      fourteen in a single step, before a redisplay has shown
  ;;      either, so a long jump appears to paint only the last few
  ;;      cells.  A smear that runs a little behind is better than one
  ;;      nobody sees.
  (should (= (smear-cursor--dt (/ 1.0 60.0)) 1.0))     ; on time
  (should (= (smear-cursor--dt 0.0) 0.5))              ; no time at all
  (should (= (smear-cursor--dt 0.05) smear-cursor--dt-max))  ; the slow setup frame
  (should (<= (smear-cursor--dt 10.0) smear-cursor--dt-max)) ; a stall
  (should (< smear-cursor--dt-max 3.0)))

(ert-deftest smear-cursor-test-length-cap-shortens-the-tail ()
  ;; GIVEN a quad stretched far behind its target
  ;; WHEN the length is capped
  ;; THEN the corners nearest the target stay where they are and the
  ;;      far ones are drawn in, so the head is still on the cursor and
  ;;      only the tail is shorter.
  ;;
  ;;      The old cap translated the whole quad instead, which put the
  ;;      smear that far behind the target with nothing joining the two.
  (let ((c (vector 100.0 0.0 108.0 0.0 108.0 900.0 100.0 900.0))
        (target [100.0 900.0 8.0 18.0]))
    (smear-cursor--shorten-corners c target 200.0)
    ;; the trailing corners came in to within the cap of the target centre
    (let ((cy (+ 900.0 9.0)))
      (dotimes (i 4)
        (let ((d (sqrt (+ (expt (- (aref c (* 2 i)) 104.0) 2)
                          (expt (- (aref c (1+ (* 2 i))) cy) 2)))))
          (should (<= d 200.5)))))
    ;; and the corners that were already at the target did not move
    (should (= (aref c 5) 900.0))
    (should (= (aref c 7) 900.0))))

(ert-deftest smear-cursor-test-wind-up-is-bounded-by-the-cursor ()
  ;; GIVEN a one-line move and a forty-five-line move
  ;; WHEN the wind-up kick is worked out for each
  ;; THEN the short one keeps its proportional flourish and the long
  ;;      one is held to a cursor's worth.
  ;;
  ;;      The kick is a fraction of the distance jumped, so it grows
  ;;      with it: 0.2 of one row is under four pixels and reads as a
  ;;      flourish, 0.2 of forty-five rows is 162.  That is nine rows
  ;;      of backward travel, which puts the start of the trail well
  ;;      above the line the cursor left.
  (let ((smear-cursor-anticipation 0.2)
        (smear-cursor-max-anticipation 1.0))
    ;; the kick is away from the target: a target one row below gives
    ;; 0.2 * 18 = 3.6 pixels upward, well inside a cursor
    (should (< (abs (- (smear-cursor--wind-up 18.0 18) -3.6)) 0.01))
    ;; forty-five rows below: held to one cursor height, not 162 pixels
    (should (= (smear-cursor--wind-up 810.0 18) -18.0))
    ;; and a target above kicks the other way, bounded the same
    (should (= (smear-cursor--wind-up -810.0 18) 18.0)))
  ;; unbounded, it is the plain fraction again
  (let ((smear-cursor-anticipation 0.2)
        (smear-cursor-max-anticipation nil))
    (should (= (smear-cursor--wind-up 810.0 18) -162.0))))

(ert-deftest smear-cursor-test-measuring-does-not-take-pads-off-screen ()
  ;; GIVEN a running animation whose end-of-line padding is on screen
  ;; WHEN point is sampled, which must not measure through it
  ;; THEN it is only silenced for the measurement and put back
  ;;      before anything can redisplay.
  ;;
  ;;      Deleting it instead leaves that stretch of the trail off the
  ;;      screen until the next frame paints, up to a frame's worth of
  ;;      nothing.  On a repeated key that is a flicker in the middle of
  ;;      the smear.
  (with-temp-buffer
    (insert "hello world")
    (let* ((ov (make-overlay (point-max) (point-max) nil t t))
           (anim (smear-cursor--anim-create
                  :buffer (current-buffer)
                  :cells (make-hash-table :test 'eql)
                  :pads (make-hash-table :test 'eql)))
           (during nil))
      (overlay-put ov 'after-string "XX")
      (puthash 0 (cons 2 ov) (smear-cursor--anim-pads anim))
      (smear-cursor--without-pads
       anim (lambda () (setq during (overlay-get ov 'after-string))))
      ;; silent while measuring, back afterwards, and still the same overlay
      (should (null during))
      (should (equal (overlay-get ov 'after-string) "XX"))
      (should (overlay-buffer ov))
      (should (eq (cdr (gethash 0 (smear-cursor--anim-pads anim))) ov)))))

(ert-deftest smear-cursor-test-scrolling-drops-the-row-cache ()
  ;; GIVEN an animation that has resolved rows for the view it started in
  ;; WHEN the window scrolls under it
  ;; THEN the cache goes, along with the panes built from it.
  ;;
  ;;      Row info is screen row -> buffer position, and a scroll moves
  ;;      every one of them.  Kept across a scroll it names the wrong
  ;;      lines: panes redraw text that is no longer there and sit at
  ;;      the x the old line began at, which is a trail in pieces at
  ;;      wrong offsets over half-erased text.
  (with-temp-buffer
    (dotimes (i 200) (insert (format "line %d\n" i)))
    (let ((anim (smear-cursor--anim-create
                 :window (selected-window) :buffer (current-buffer)
                 :cw 8 :lh 18 :y-base 0 :win-w 800 :win-h 720
                 :tick (buffer-chars-modified-tick)
                 :cells (make-hash-table :test 'eql)
                 :pads (make-hash-table :test 'eql)
                 :window-start 1)))
      (puthash 0 (list 1 7 40) (smear-cursor--anim-rows anim))
      (setf (smear-cursor--anim-anchor anim) (cons 0 1))
      ;; same view: the cache survives
      (cl-letf (((symbol-function 'window-start) (lambda (&rest _) 1)))
        (smear-cursor--revalidate anim)
        (should (gethash 0 (smear-cursor--anim-rows anim))))
      ;; scrolled: it does not
      (cl-letf (((symbol-function 'window-start) (lambda (&rest _) 500)))
        (smear-cursor--revalidate anim)
        (should-not (gethash 0 (smear-cursor--anim-rows anim)))
        (should-not (smear-cursor--anim-anchor anim))
        (should (eql (smear-cursor--anim-window-start anim) 500))))))

(ert-deftest smear-cursor-test-a-scroll-no-longer-refuses-an-origin ()
  ;; GIVEN a cursor position remembered in one view
  ;; WHEN the window has scrolled since
  ;; THEN the origin still stands.
  ;;
  ;;      A remembered rect is a pixel position, so a scroll moves the
  ;;      text under it and the same pixel is a different line
  ;;      afterwards.  That argues for dropping the origin, but it only
  ;;      covers a scroll leaving point on the same screen row, and that
  ;;      case needs no rule: the rects are then equal and
  ;;      `smear-cursor-min-distance' discards it anyway.
  ;;
  ;;      Where the cursor visibly crosses the screen, `M-<' from
  ;;      halfway down a buffer being the obvious case, the origin is
  ;;      what the trail is for.
  (let ((smear-cursor--last-rects (make-hash-table :test 'eq))
        (smear-cursor--last-window 'win))
    (puthash 'win (list 'buf 1 [10.0 500.0 8.0 18.0]) smear-cursor--last-rects)
    (cl-letf (((symbol-function 'window-start) (lambda (&rest _) 4321)))
      (should (smear-cursor--origin-rect 'win 'buf)))))

(ert-deftest smear-cursor-test-no-smear-while-a-selection-is-being-made ()
  ;; GIVEN a cursor that has moved
  ;; WHEN it moved while a region was live, or mid mouse-drag
  ;; THEN no animation starts.
  ;;
  ;;      Dragging out a region fires the sample sixty-nine times in one
  ;;      gesture, thirty-four of them starting a smear: a trail chasing
  ;;      the pointer across the selection, and its setup paid over and
  ;;      over.  A cursor defining a region is not travelling.
  ;;
  ;;      A live region outlives the gesture, so the position is
  ;;      recorded and the next real move smears from where the
  ;;      selection left the cursor.  A tracked mouse does not: the
  ;;      button comes up and the click has to be drawn, so recording
  ;;      there would eat the very move being waited for.  See
  ;;      `smear-cursor-test-a-click-smears-though-the-drag-ate-the-sample'.
  (let ((started nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'minibufferp) (lambda (&rest _) nil))
              ((symbol-function 'selected-window) (lambda () nil))
              ((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer)))
              ((symbol-function 'window-start) (lambda (&rest _) 1))
              ((symbol-function 'smear-cursor--point-rect)
               (lambda (_w) [8.0 0.0 8.0 18.0]))
              ((symbol-function 'smear-cursor--origin-rect)
               (lambda (&rest _) [200.0 0.0 8.0 18.0]))
              ((symbol-function 'smear-cursor--start)
               (lambda (&rest _) (setq started t))))
      (let ((smear-cursor-mode t)
            (smear-cursor-while-selecting nil))
        ;; selecting: no smear, but the position is kept
        (let ((mark-active t) (track-mouse nil))
          (cl-letf (((symbol-function 'region-active-p) (lambda () t)))
            (clrhash smear-cursor--last-rects)
            (setq started nil)
            (smear-cursor--sample)
            (should-not started)
            (should (= (hash-table-count smear-cursor--last-rects) 1))))
        ;; mid mouse-drag: no smear either, and nothing recorded, so
        ;; the move is still there to draw when the button comes up
        (let ((track-mouse 'drag-tracking))
          (cl-letf (((symbol-function 'region-active-p) (lambda () nil)))
            (clrhash smear-cursor--last-rects)
            (setq started nil)
            (smear-cursor--sample)
            (should-not started)
            (should (= (hash-table-count smear-cursor--last-rects) 0))))
        ;; neither: the smear runs as usual
        (let ((track-mouse nil))
          (cl-letf (((symbol-function 'region-active-p) (lambda () nil)))
            (setq started nil)
            (smear-cursor--sample)
            (should started)))
        ;; and the option turns the suppression off again
        (let ((smear-cursor-while-selecting t) (track-mouse 'drag-tracking))
          (cl-letf (((symbol-function 'region-active-p) (lambda () t)))
            (setq started nil)
            (smear-cursor--sample)
            (should started)))))))


;;;; The X compositing backend

;; These stub the module rather than loading it: the drawing itself is
;; tested in tests/smear-cursor-x11-tests.el, against a real frame.
;; What is worth testing here is the wiring: that the right numbers
;; reach the module, in the right coordinates, and that nothing is left
;; on screen.

(defmacro smear-cursor-tests--with-x11 (calls &rest body)
  "Run BODY with the x11 module stubbed, recording calls into CALLS."
  (declare (indent 1))
  `(let ((smear-cursor-backend 'x11)
         (smear-cursor--x11-loaded 'yes))
     (cl-letf (((symbol-function 'smear-cursor-x11-stage) (lambda (_f) 'stage))
               ((symbol-function 'smear-cursor-x11--trouble) (lambda (_s) nil))
               ((symbol-function 'window-frame) (lambda (&rest _) 'frame))
               ((symbol-function 'window-inside-pixel-edges)
                (lambda (&rest _) '(29 35 829 749)))
               ((symbol-function 'smear-cursor-x11--corners-in-frame)
                (lambda (win c) (smear-cursor-tests--shift win c)))
               ((symbol-function 'smear-cursor-x11--head-of)
                (lambda (_w r) (vector (aref r 0) (aref r 1))))
               ((symbol-function 'smear-cursor-x11--begin)
                (lambda (s) (push (list 'begin s) ,calls) t))
               ((symbol-function 'smear-cursor-x11--end)
                (lambda (s) (push (list 'end s) ,calls) t))
               ((symbol-function 'smear-cursor-x11--frame-begin)
                (lambda (s x y w h)
                  (push (list 'frame-begin s x y w h) ,calls) t))
               ((symbol-function 'smear-cursor-x11--frame-end)
                (lambda (s) (push (list 'frame-end s) ,calls) t))
               ((symbol-function 'smear-cursor-x11--draw)
                (lambda (s layer) (push (list 'draw s layer) ,calls) t)))
       ,@body)))

(defun smear-cursor-tests--shift (_win corners)
  "Stand-in for the real conversion: shift every corner by the edges."
  (let ((out (copy-sequence corners)))
    (dotimes (i 4)
      (aset out (* 2 i) (+ 29 (aref corners (* 2 i))))
      (aset out (1+ (* 2 i)) (+ 35 (aref corners (1+ (* 2 i))))))
    out))

(ert-deftest smear-cursor-test-x11-paints-through-the-module ()
  ;; GIVEN the x11 backend, with a stage the frame can carry
  ;; WHEN a frame of the smear is painted
  ;; THEN the module draws it, and not one cell is claimed.
  ;;
  ;;      Claiming a cell is what hides the character under it.  The
  ;;      whole point of this backend is that nothing is hidden, so a
  ;;      single claimed cell would be a bug rather than a detail.
  (let ((calls nil))
    (smear-cursor-tests--with-x11 calls
      (let ((anim (smear-cursor--anim-create
                   :window 'win
                   :corners [0.0 0.0 10.0 0.0 10.0 20.0 0.0 20.0]
                   :target [0.0 0.0 10.0 20.0]
                   :color [154 184 232]
                   :cells (make-hash-table :test 'eql)
                   :pads (make-hash-table :test 'eql))))
        (should (= 1 (smear-cursor--paint-frame anim)))
        (should (= 0 (hash-table-count (smear-cursor--anim-cells anim))))
        (should (= 0 (hash-table-count (smear-cursor--anim-pads anim))))
        (should (assq 'draw (mapcar (lambda (c) (cons (car c) t)) calls)))))))

(ert-deftest smear-cursor-test-x11-gets-frame-coordinates ()
  ;; GIVEN a quad measured in a window's text area
  ;; WHEN it goes to the module
  ;; THEN it has been moved into the frame's own pixels first.
  ;;
  ;;      The module draws on the frame's X window, which knows nothing
  ;;      about Emacs windows.  Sending text-area numbers straight
  ;;      through would put the trail up and to the left of the cursor
  ;;      by the width of the fringe and the height of the tool bar.
  (let ((calls nil))
    (smear-cursor-tests--with-x11 calls
      (let ((anim (smear-cursor--anim-create
                   :window 'win
                   :corners [0.0 0.0 10.0 0.0 10.0 20.0 0.0 20.0]
                   :target [0.0 0.0 10.0 20.0]
                   :color [154 184 232]
                   :cells (make-hash-table :test 'eql)
                   :pads (make-hash-table :test 'eql))))
        (smear-cursor--paint-frame anim)
        (let ((drawn (car (seq-filter (lambda (c) (eq (car c) 'draw)) calls))))
          (should drawn)
          ;; slot 1 of the layer vector is the corners
          (should (equal (aref (nth 2 drawn) 1)
                         [29.0 35.0 39.0 35.0 39.0 55.0 29.0 55.0])))))))

(ert-deftest smear-cursor-test-x11-opens-the-overlay-once-a-flight ()
  ;; GIVEN several frames of one smear
  ;; WHEN each is painted
  ;; THEN the overlay goes up once, not once a frame.  Putting it up
  ;;      recopies the whole window; doing that per frame would pay for
  ;;      the flight over and over.
  (let ((calls nil))
    (smear-cursor-tests--with-x11 calls
      (let ((anim (smear-cursor--anim-create
                   :window 'win
                   :corners [0.0 0.0 10.0 0.0 10.0 20.0 0.0 20.0]
                   :target [0.0 0.0 10.0 20.0]
                   :color [154 184 232]
                   :cells (make-hash-table :test 'eql)
                   :pads (make-hash-table :test 'eql))))
        (let ((smear-cursor-trail-style 'plain))   ; one layer, so one draw
          (dotimes (_ 4) (smear-cursor--paint-frame anim))
          (should (= 1 (seq-count (lambda (c) (eq (car c) 'begin)) calls)))
          ;; the backdrop is laid once a frame, not once a layer
          (should (= 4 (seq-count (lambda (c) (eq (car c) 'frame-begin)) calls)))
          (should (= 4 (seq-count (lambda (c) (eq (car c) 'draw)) calls))))))))

(ert-deftest smear-cursor-test-x11-overlay-comes-down-when-the-smear-stops ()
  ;; GIVEN a smear that has been drawing through the module
  ;; WHEN it stops
  ;; THEN the overlay is taken down.  Left up it is a still photograph
  ;;      of the buffer laid over the buffer, which looks like nothing
  ;;      at all until the text changes underneath it.
  (let ((calls nil))
    (smear-cursor-tests--with-x11 calls
      (let ((anim (smear-cursor--anim-create
                   :window 'win
                   :corners [0.0 0.0 10.0 0.0 10.0 20.0 0.0 20.0]
                   :target [0.0 0.0 10.0 20.0]
                   :color [154 184 232]
                   :cells (make-hash-table :test 'eql)
                   :pads (make-hash-table :test 'eql))))
        (smear-cursor--paint-frame anim)
        (let ((smear-cursor--anim anim))
          (smear-cursor--stop))
        (should (seq-find (lambda (c) (eq (car c) 'end)) calls))))))

(ert-deftest smear-cursor-test-x11-falls-back-rather-than-failing ()
  ;; GIVEN the x11 backend asked for where it cannot run: no module, a
  ;;       pgtk or macOS frame, or a display without Composite
  ;; WHEN a frame is painted
  ;; THEN the Lisp rasterizer draws it instead, quietly.
  ;;
  ;;      Configuration should not have to know what the display can do.
  (let ((smear-cursor-backend 'x11)
        (smear-cursor--x11-loaded 'no)
        (fell-back nil))
    (cl-letf (((symbol-function 'smear-cursor--paint-frame-cpu)
               (lambda (_a) (setq fell-back t) 0)))
      (smear-cursor--paint-frame
       (smear-cursor--anim-create :cells (make-hash-table :test 'eql)
                                  :pads (make-hash-table :test 'eql)))
      (should fell-back))))

(ert-deftest smear-cursor-test-x11-does-not-leave-two-painters-on-screen ()
  ;; GIVEN a smear drawing through the module
  ;; WHEN the stage stops being available mid-flight
  ;; THEN the overlay comes down before the cell painter draws anything.
  ;;
  ;;      Two painters on screen at once is not hypothetical here: it
  ;;      is what produced the trail-in-pieces bug that took four
  ;;      rounds to find.  One painter's work at a time.
  (let ((calls nil))
    (smear-cursor-tests--with-x11 calls
      (let ((anim (smear-cursor--anim-create
                   :window 'win
                   :corners [0.0 0.0 10.0 0.0 10.0 20.0 0.0 20.0]
                   :target [0.0 0.0 10.0 20.0]
                   :color [154 184 232]
                   :cells (make-hash-table :test 'eql)
                   :pads (make-hash-table :test 'eql))))
        (smear-cursor--paint-frame anim)
        (cl-letf (((symbol-function 'smear-cursor-x11-stage) (lambda (_f) nil))
                  ((symbol-function 'smear-cursor--paint-frame-cpu)
                   (lambda (_a) 0)))
          (smear-cursor--paint-frame anim))
        (should (seq-find (lambda (c) (eq (car c) 'end)) calls))))))


;;;; Trail styles

;; A style says what the trail is: layers, their shapes, and how their
;; alpha runs from head to tail.  It says nothing about how they are
;; drawn, so a second renderer can be put underneath without the styles
;; changing.

(ert-deftest smear-cursor-test-a-style-can-be-defined-and-found ()
  ;; GIVEN a trail defined by name
  ;; WHEN it is looked up
  ;; THEN its layers come back
  (let ((smear-cursor--trails (copy-hash-table smear-cursor--trails)))
    (smear-cursor-define-trail 'test-plain
      :doc "one flat quad"
      :layers '((:shape quad :alpha 0.5)))
    (let ((style (smear-cursor-trail 'test-plain)))
      (should style)
      (should (= 1 (length (plist-get style :layers)))))))

(ert-deftest smear-cursor-test-an-unknown-style-falls-back ()
  ;; GIVEN a style name nobody defined: a typo, or a style from a
  ;;       config that outlived the package version that had it
  ;; WHEN the trail is asked for
  ;; THEN the plain one is drawn rather than nothing at all.
  ;;
  ;;      A cursor that stops leaving a trail because of a misspelling
  ;;      is a worse failure than a plain trail.
  (should (smear-cursor-trail 'no-such-style-exists))
  (should (equal (smear-cursor-trail 'no-such-style-exists)
                 (smear-cursor-trail 'plain))))

(ert-deftest smear-cursor-test-alpha-shorthand-becomes-stops ()
  ;; GIVEN a layer written the short way, with one alpha
  ;; WHEN its stops are resolved
  ;; THEN it runs from that alpha at the head to nearly nothing at the
  ;;      tail, which is the common case and needs no spelling out
  (let ((stops (smear-cursor--layer-stops '(:shape quad :alpha 0.8))))
    (should (= 0.0 (car (nth 0 stops))))
    (should (= 0.8 (cdr (nth 0 stops))))
    (should (= 1.0 (car (car (last stops)))))
    (should (< (cdr (car (last stops))) 0.2))))

(ert-deftest smear-cursor-test-explicit-stops-win ()
  ;; GIVEN a layer that spells its own stops out
  ;; WHEN they are resolved
  ;; THEN they are used unchanged, however many there are
  (let ((stops (smear-cursor--layer-stops
                '(:shape quad :stops ((0.0 . 1.0) (0.4 . 0.6) (1.0 . 0.0))))))
    (should (= 3 (length stops)))
    (should (equal (nth 1 stops) '(0.4 . 0.6)))))

(ert-deftest smear-cursor-test-a-layer-becomes-the-vector-the-module-wants ()
  ;; GIVEN a layer and the geometry of one frame
  ;; WHEN the layer is turned into what the module reads
  ;; THEN it is the eight-slot vector, with the stops flattened
  (let* ((corners [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0])
         (head [9.0 10.0])
         (v (smear-cursor--layer-vector
             '(:shape quad :alpha 0.5 :grow 3 :blur 4)
             corners head [154 184 232])))
    (should (= 8 (length v)))
    (should (= 0 (aref v 0)))               ; quad
    (should (equal (aref v 1) corners))
    (should (equal (aref v 2) head))
    (should (= 3 (aref v 4)))               ; grow
    (should (= 4 (aref v 5)))               ; blur
    (should (vectorp (aref v 7)))))         ; stops, flattened

(ert-deftest smear-cursor-test-a-radial-layer-says-so ()
  ;; GIVEN a glow layer
  ;; WHEN it is turned into the module's vector
  ;; THEN its kind and radius come through
  (let ((v (smear-cursor--layer-vector
            '(:shape radial :alpha 0.6 :radius 18)
            [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0] [5.0 5.0] [1 2 3])))
    (should (= 1 (aref v 0)))
    (should (= 18 (aref v 6)))))

(ert-deftest smear-cursor-test-an-echo-layer-uses-an-older-shape ()
  ;; GIVEN a trail keeping a few frames of history
  ;; WHEN a layer asks for the shape two frames back
  ;; THEN that shape is drawn, not the current one.
  ;;
  ;;      This is how a motion-blurred trail is made: copies of the
  ;;      quad at its earlier positions.
  (let ((anim (smear-cursor--anim-create
               :corners [9.0 9.0 9.0 9.0 9.0 9.0 9.0 9.0]
               :cells (make-hash-table :test 'eql)
               :pads (make-hash-table :test 'eql))))
    (setf (smear-cursor--anim-history anim)
          (list [1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0]
                [2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0]
                [3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0]))
    ;; newest first, so element 0 is one frame back and :echo 2 is two
    (should (equal (smear-cursor--layer-corners anim '(:echo 1))
                   [1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0]))
    (should (equal (smear-cursor--layer-corners anim '(:echo 2))
                   [2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0]))
    ;; the current shape when no echo is asked for
    (should (equal (smear-cursor--layer-corners anim '(:shape quad))
                   [9.0 9.0 9.0 9.0 9.0 9.0 9.0 9.0]))
    ;; and an echo further back than we have keeps the oldest we do
    (should (equal (smear-cursor--layer-corners anim '(:echo 99))
                   [3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0]))))

(ert-deftest smear-cursor-test-history-is-bounded ()
  ;; GIVEN an animation running for many frames
  ;; WHEN each frame records its shape
  ;; THEN only the few an echo could ask for are kept.  A long smear is
  ;;      hundreds of frames, and an unbounded list of them is garbage
  ;;      the collector will come for mid-animation.
  (let ((anim (smear-cursor--anim-create
               :cells (make-hash-table :test 'eql)
               :pads (make-hash-table :test 'eql))))
    (dotimes (i 200)
      (setf (smear-cursor--anim-corners anim) (make-vector 8 (float i)))
      (smear-cursor--remember-shape anim))
    (should (<= (length (smear-cursor--anim-history anim))
                smear-cursor--history-max))))

(ert-deftest smear-cursor-test-the-builtin-styles-are-well-formed ()
  ;; GIVEN the styles that ship with the package
  ;; WHEN each is resolved
  ;; THEN every layer has a shape the renderer knows and stops that run
  ;;      from 0 to 1.  A malformed style would draw nothing, silently.
  (dolist (name '(plain comet ghost ribbon))
    (let ((style (smear-cursor-trail name)))
      (should style)
      (should (plist-get style :layers))
      (dolist (layer (plist-get style :layers))
        (should (memq (or (plist-get layer :shape) 'quad) '(quad radial)))
        (let ((stops (smear-cursor--layer-stops layer)))
          (should (>= (length stops) 2))
          (should (= 0.0 (car (nth 0 stops))))
          (should (= 1.0 (car (car (last stops))))))))))

(ert-deftest smear-cursor-test-the-frame-box-covers-every-layer ()
  ;; GIVEN layers that reach past the quad carrying them: a grown and
  ;;       blurred halo, and a glow centred on the head
  ;; WHEN the frame's rectangle is worked out
  ;; THEN it contains all of them, with room for the blur.
  ;;
  ;;      Only this rectangle is painted and only this much of the
  ;;      overlay is shown, so a box that is too small clips the trail
  ;;      and one the size of the window costs two million pixels of
  ;;      server work a frame.
  (let* ((quad [100.0 100.0 120.0 100.0 120.0 120.0 100.0 120.0])
         (head [110.0 110.0])
         (box (smear-cursor--trail-bbox
               '((:shape quad) (:shape quad :grow 4 :blur 5)
                 (:shape radial :radius 30))
               (list quad quad quad) head)))
    (cl-destructuring-bind (x y w h) box
      ;; the glow reaches 30 from the head, further than the quad
      (should (<= x (- 110 30)))
      (should (<= y (- 110 30)))
      (should (>= (+ x w) (+ 110 30)))
      (should (>= (+ y h) (+ 110 30)))
      ;; and it is not simply enormous
      (should (< w 200))
      (should (< h 200)))))

(ert-deftest smear-cursor-test-the-frame-box-allows-for-blur ()
  ;; GIVEN one blurred layer
  ;; WHEN its box is taken
  ;; THEN there is room around the quad for the blur to spread into,
  ;;      or the halo is cut off square at the edge of its own shape
  (let* ((quad [100.0 100.0 120.0 100.0 120.0 120.0 100.0 120.0])
         (plain (smear-cursor--trail-bbox '((:shape quad)) (list quad) [110.0 110.0]))
         (blurred (smear-cursor--trail-bbox '((:shape quad :blur 6))
                                            (list quad) [110.0 110.0])))
    (should (> (nth 2 blurred) (nth 2 plain)))
    (should (> (nth 3 blurred) (nth 3 plain)))))

(ert-deftest smear-cursor-test-a-style-may-set-the-spring ()
  ;; GIVEN a style that says how its trail should move
  ;; WHEN the spring is read
  ;; THEN the style's value is used, and the custom option elsewhere
  (let ((smear-cursor--trails (copy-hash-table smear-cursor--trails))
        (smear-cursor-stiffness-head 0.6)
        (smear-cursor-damping 0.85))
    (smear-cursor-define-trail 'test-springy
      :layers '((:shape quad)) :spring '(:head 0.95))
    (let ((smear-cursor-trail-style 'test-springy))
      (should (= 0.95 (smear-cursor--spring :head 0.6)))
      ;; anything the style is silent about keeps the configured value
      (should (= 0.85 (smear-cursor--spring :damping smear-cursor-damping))))
    (let ((smear-cursor-trail-style 'plain))
      (should (= 0.6 (smear-cursor--spring :head 0.6))))))

(ert-deftest smear-cursor-test-the-styles-are-told-apart-by-more-than-alpha ()
  ;; GIVEN the styles that ship
  ;; WHEN they are compared
  ;; THEN no two of them are the same shape at the same speed.
  ;;
  ;;      Four trails differing only in alpha measured as different and
  ;;      looked identical: a trail is on screen for about a fifth of a
  ;;      second, with nothing beside it to compare against.  What reads
  ;;      in that time is shape, brightness and how long it lasts.
  (let ((seen nil))
    (dolist (name '(plain comet ghost ribbon))
      (let* ((style (smear-cursor-trail name))
             (layers (plist-get style :layers))
             ;; a coarse fingerprint: how many layers, the boldest
             ;; alpha, whether anything is blurred or grown, and how
             ;; stiff the tail is
             (print (list (length layers)
                          (apply #'max (mapcar (lambda (l)
                                                 (cdr (nth 0 (smear-cursor--layer-stops l))))
                                               layers))
                          (and (seq-find (lambda (l) (plist-get l :blur)) layers) t)
                          (and (seq-find (lambda (l) (plist-get l :grow)) layers) t)
                          (plist-get (plist-get style :spring) :tail))))
        (should-not (member print seen))
        (push print seen)))))

(ert-deftest smear-cursor-test-a-style-may-choose-its-colour ()
  ;; GIVEN a style that is about being a particular colour, with one
  ;;       layer that is a different colour again
  ;; WHEN each layer's colour is resolved
  ;; THEN the layer's own wins, then the style's, then the cursor's.
  ;;
  ;;      A laser's dot is nearly white however red its bloom is.
  (let ((style '(:color [255 45 30]))
        (cursor [154 184 232]))
    (should (equal (smear-cursor--layer-color '(:shape quad) style cursor)
                   [255 45 30]))
    (should (equal (smear-cursor--layer-color
                    '(:shape radial :color [255 235 225]) style cursor)
                   [255 235 225]))
    ;; a style with no colour of its own leaves the cursor's alone
    (should (equal (smear-cursor--layer-color '(:shape quad) nil cursor)
                   cursor))))


;;;; What counts as the cursor moving

(ert-deftest smear-cursor-test-a-preview-behind-the-minibuffer-is-sampled ()
  ;; GIVEN the minibuffer selected, as `consult-line' leaves it, with a
  ;;       live window behind it
  ;; WHEN the window to sample is chosen
  ;; THEN it is the window behind.  Preview commands move point there
  ;;      while the minibuffer stays selected, and a jump nobody can
  ;;      follow is the case a trail is most for.
  (let ((back (selected-window))
        (mini (minibuffer-window)))
    (cl-letf (((symbol-function 'selected-window) (lambda () mini))
              ((symbol-function 'minibuffer-selected-window) (lambda () back)))
      (should (eq back (smear-cursor--sample-window))))))

(ert-deftest smear-cursor-test-the-minibuffer-is-never-sampled ()
  ;; GIVEN the minibuffer selected with no live window behind it
  ;; WHEN the window to sample is chosen
  ;; THEN the minibuffer window comes back, which `smear-cursor--sample'
  ;;      then refuses.  The minibuffer is one line tall and already
  ;;      under the eye, so a trail in it marks nothing.
  (let ((mini (minibuffer-window)))
    (cl-letf (((symbol-function 'selected-window) (lambda () mini))
              ((symbol-function 'minibuffer-selected-window) (lambda () nil)))
      (should (eq mini (smear-cursor--sample-window)))
      (should (minibufferp (window-buffer (smear-cursor--sample-window)))))))

(ert-deftest smear-cursor-test-an-ordinary-window-samples-itself ()
  ;; GIVEN an ordinary selected window
  ;; WHEN the window to sample is chosen
  ;; THEN it is that window
  (should (eq (selected-window) (smear-cursor--sample-window))))


(ert-deftest smear-cursor-test-changing-buffer-is-a-move ()
  ;; GIVEN a window whose buffer has just changed under it
  ;; WHEN the cursor's origin is asked for
  ;; THEN where it was on screen still counts.
  ;;
  ;;      The window did not go anywhere: those pixels are the same
  ;;      place, and the cursor really did travel from them to wherever
  ;;      point is in the new buffer.
  (let ((smear-cursor--last-rects (make-hash-table :test 'eq))
        (smear-cursor--last-window 'win))
    (puthash 'win (list 'old-buffer 1 [10.0 200.0 8.0 18.0])
             smear-cursor--last-rects)
    (cl-letf (((symbol-function 'window-start) (lambda (&rest _) 1)))
      (should (equal (smear-cursor--origin-rect 'win 'new-buffer)
                     [10.0 200.0 8.0 18.0])))))

(ert-deftest smear-cursor-test-scrolling-is-a-move-if-the-cursor-moved ()
  ;; GIVEN a window that has scrolled since the last sample
  ;; WHEN the origin is asked for
  ;; THEN it is still where the cursor was on screen.
  ;;
  ;;      A scroll moves the text and not the cursor, which argues for
  ;;      refusing the origin.  That holds only for a scroll leaving
  ;;      point on the same screen row, and that case needs no rule: the
  ;;      two rects are then equal and `smear-cursor-min-distance'
  ;;      already discards it.  Refusing it more widely also refuses
  ;;      `M-<' from halfway down a buffer, where the cursor plainly
  ;;      does cross the screen.
  (let ((smear-cursor--last-rects (make-hash-table :test 'eq))
        (smear-cursor--last-window 'win))
    (puthash 'win (list 'buf 1 [10.0 500.0 8.0 18.0]) smear-cursor--last-rects)
    (cl-letf (((symbol-function 'window-start) (lambda (&rest _) 4321)))
      (should (equal (smear-cursor--origin-rect 'win 'buf)
                     [10.0 500.0 8.0 18.0])))))

(ert-deftest smear-cursor-test-crossing-from-a-scrolled-window-still-counts ()
  ;; GIVEN the selection crossing from another window that has scrolled
  ;; WHEN the origin is asked for
  ;; THEN the crossing is still drawn: the cursor did travel across the
  ;;      frame, whatever the window it left has done since
  (let ((smear-cursor--last-rects (make-hash-table :test 'eq))
        (smear-cursor--last-window 'other))
    (puthash 'other (list 'buf 1 [10.0 40.0 8.0 18.0]) smear-cursor--last-rects)
    (cl-letf (((symbol-function 'window-live-p) (lambda (&rest _) t))
              ((symbol-function 'window-frame) (lambda (&rest _) 'frame))
              ((symbol-function 'window-start) (lambda (&rest _) 9999))
              ((symbol-function 'smear-cursor--rect-in-window)
               (lambda (rect _from _to) rect)))
      (should (equal (smear-cursor--origin-rect 'win 'buf)
                     [10.0 40.0 8.0 18.0])))))

(ert-deftest smear-cursor-test-a-scroll-that-moves-nothing-draws-nothing ()
  ;; GIVEN a scroll that leaves the cursor on the same screen row,
  ;;       `C-v' with point staying put relative to the window
  ;; WHEN the move is measured
  ;; THEN it is below the threshold and no smear starts.  No rule about
  ;;      scrolling is needed for this: the distance already says it.
  (let ((rect [10.0 200.0 8.0 18.0]))
    (should (< (smear-cursor--move-cells rect rect 8 18)
               smear-cursor-min-distance))))

(ert-deftest smear-cursor-test-no-style-may-run-away ()
  ;; GIVEN each style that ships
  ;; WHEN a long jump is simulated to convergence
  ;; THEN none of them takes longer than the cap allows.
  ;;
  ;;      A style may set its own spring, and a slack tail converges
  ;;      slowly: one shipped at 735 ms against another's 150 for the
  ;;      same jump.  That matters more than it sounds, because while a
  ;;      smear is in flight every movement retargets it rather than
  ;;      starting afresh, so on a long scroll it never ends.
  (dolist (name '(plain comet ghost ribbon laser))
    (let* ((smear-cursor-trail-style name)
           (target (vector 400.0 400.0 10.0 21.0))
           (corners (vector 0.0 0.0 10.0 0.0 10.0 21.0 0.0 21.0))
           (vel (make-vector 8 0.0))
           (n 0) (dist 1e9))
      (while (and (< n 600) (>= dist smear-cursor--eps))
        (setq dist (smear-cursor--ease-step
                    corners vel target
                    (smear-cursor--spring :head smear-cursor-stiffness-head)
                    (smear-cursor--spring :tail smear-cursor-stiffness-tail)
                    (smear-cursor--spring :damping smear-cursor-damping)
                    (smear-cursor--spring :exponent
                                          smear-cursor-trailing-exponent)
                    1.0 nil))
        (setq n (1+ n)))
      ;; at 60 fps, and with a little room under the cap itself
      (should (< (* n (/ 1.0 60)) smear-cursor-max-duration)))))

(ert-deftest smear-cursor-test-every-style-stretches-into-a-trail ()
  ;; GIVEN each style
  ;; WHEN the quad is stepped a few frames into a long jump
  ;; THEN it has stretched: the corners are further apart than the
  ;;      cursor rect they started as.
  ;;
  ;;      `laser' shipped with a tail stiff enough that the quad barely
  ;;      deformed, so it drew a moving red block and no streak.  A
  ;;      trail that does not stretch is not a trail.
  (dolist (name '(plain comet ghost ribbon laser))
    (let* ((smear-cursor-trail-style name)
           (target (vector 400.0 400.0 10.0 21.0))
           (corners (vector 0.0 0.0 10.0 0.0 10.0 21.0 0.0 21.0))
           (vel (make-vector 8 0.0))
           (tallest 0))
      ;; over the whole flight, not at some chosen frame: a stiff style
      ;; reaches its widest later than a slack one, and asking at frame
      ;; five said `ghost' did not stretch when in fact it reaches four
      ;; times the height of the cursor
      (dotimes (_ 20)
        (smear-cursor--ease-step
         corners vel target
         (smear-cursor--spring :head smear-cursor-stiffness-head)
         (smear-cursor--spring :tail smear-cursor-stiffness-tail)
         (smear-cursor--spring :damping smear-cursor-damping)
         (smear-cursor--spring :exponent smear-cursor-trailing-exponent)
         1.0 nil)
        (setq tallest (max tallest (abs (- (aref corners 1)
                                           (aref corners 5))))))
      ;; well past the 21 px cursor it began as
      (should (> tallest 60)))))

(ert-deftest smear-cursor-test-gap-stats-name-the-worst-frame ()
  ;; GIVEN a flight whose third frame arrives long after the rest
  ;; WHEN each gap is noted
  ;; THEN the stats say both how long the worst one was and where it
  ;; fell.  That distinguishes the overlay going up from Emacs being
  ;; busy in the middle of a smear.
  (let ((anim (smear-cursor--anim-create)))
    (dolist (gap '(0.0167 0.0166 0.1269 0.0168 0.0165))
      (smear-cursor--note-gap anim gap))
    (should (< (abs (- 0.1269 (smear-cursor--anim-gap-max anim))) 1e-9))
    (should (= 3 (smear-cursor--anim-gap-max-at anim)))
    (should (< (abs (- 0.1935 (smear-cursor--anim-gap-sum anim))) 1e-9))))

(ert-deftest smear-cursor-test-gap-stats-count-from-one ()
  ;; GIVEN a flight that stalls on its very first frame, the one that
  ;; puts the overlay up and copies the window behind it
  ;; WHEN the gaps are noted
  ;; THEN the worst is frame 1, not frame 0: a report the reader has to
  ;; adjust by one is a report that gets misread.
  (let ((anim (smear-cursor--anim-create)))
    (smear-cursor--note-gap anim 0.09)
    (smear-cursor--note-gap anim 0.016)
    (should (= 1 (smear-cursor--anim-gap-max-at anim)))))

(ert-deftest smear-cursor-test-report-tells-setup-from-interruption ()
  ;; GIVEN two flights with the same worst gap, one on frame 1 and one
  ;; in the middle
  ;; WHEN the verdict is drawn
  ;; THEN they read as different faults, because they are: frame 1 is
  ;; the overlay going up and is paid once a flight, and a stall in the
  ;; middle is Emacs doing something else while the trail ran.
  (let ((want 16.7))
    (should (string-match-p
             "overlay"
             (smear-cursor--report-verdict 0.9 16.7 126.9 1 want)))
    (should (string-match-p
             "Emacs"
             (smear-cursor--report-verdict 0.9 16.7 126.9 11 want)))))

(ert-deftest smear-cursor-test-report-blames-us-first ()
  ;; GIVEN a flight that both paints over budget and stalled on frame 1
  ;; WHEN the verdict is drawn
  ;; THEN it names the painting, which is the one thing a reader can
  ;; act on here.  A verdict pointing at Emacs while our own frames run
  ;; long sends the reader to the wrong place.
  (should (string-match-p
           "ours"
           (smear-cursor--report-verdict 20.0 30.0 126.9 1 16.7))))

(ert-deftest smear-cursor-test-report-stays-quiet-when-healthy ()
  ;; GIVEN a flight that kept its cadence and painted well inside budget
  ;; WHEN the verdict is drawn
  ;; THEN it says so plainly, so the command is worth running when
  ;; nothing is wrong.
  (should (equal "within budget"
                 (smear-cursor--report-verdict 0.9 16.7 20.0 5 16.7))))

(ert-deftest smear-cursor-test-typical-gap-ignores-one-stall ()
  ;; GIVEN a flight where fourteen frames were on time and one stalled
  ;; for a quarter of a second
  ;; WHEN the typical gap is taken
  ;; THEN it reads as on time.  The mean says 30 ms here, twice the
  ;; interval, which reads as chronically slow and sends the reader
  ;; hunting a cadence problem that fourteen of fifteen frames did not
  ;; have.
  (let ((gaps (append (make-list 14 0.0155) (list 0.235))))
    (should (< (abs (- 0.0155 (smear-cursor--median gaps))) 1e-9))
    (should (> (/ (apply #'+ gaps) 15) 0.028))))

(ert-deftest smear-cursor-test-median-handles-both-parities ()
  ;; GIVEN gap runs of even and odd length
  ;; WHEN the median is taken
  ;; THEN an even run averages the middle pair rather than falling off
  ;; the end of the list.
  (should (= 3.0 (smear-cursor--median '(1.0 5.0 3.0))))
  (should (= 4.0 (smear-cursor--median '(1.0 5.0 3.0 7.0))))
  (should (= 2.0 (smear-cursor--median '(2.0))))
  (should (= 0.0 (smear-cursor--median '()))))

(ert-deftest smear-cursor-test-late-frames-are-counted ()
  ;; GIVEN a flight with one badly late frame and one marginal one
  ;; WHEN late frames are counted against a 16.7 ms interval
  ;; THEN only the frame that actually missed is counted: a threshold
  ;; at the interval exactly would call ordinary timer jitter late and
  ;; every healthy flight would look broken.
  (should (= 1 (smear-cursor--late-frames '(0.0155 0.0171 0.235) 0.0167))))

(ert-deftest smear-cursor-test-report-names-the-wire-when-the-display-is-behind ()
  ;; GIVEN a flight whose frames were all queued inside budget, but
  ;; where one round trip after the last of them took 120 ms
  ;; WHEN the verdict is drawn
  ;; THEN it says the connection, not the painting.  `XFlush' does not
  ;; wait for the server, so on a forwarded display every frame can be
  ;; queued on time and arrive late.  That is the one fault the rest of
  ;; this report cannot see.
  (should (string-match-p
           "display\\|connection"
           (smear-cursor--report-verdict 0.5 16.7 17.0 5 16.7 0.120))))

(ert-deftest smear-cursor-test-report-puts-the-wire-before-the-paint ()
  ;; GIVEN a flight both over its paint budget and far behind on the
  ;; wire
  ;; WHEN the verdict is drawn
  ;; THEN it names the wire.  A blocking write to a backed-up
  ;; connection is *charged* to the paint, so naming the paint here
  ;; sends the reader to optimise a cost that is not theirs.
  (should (string-match-p
           "display\\|connection"
           (smear-cursor--report-verdict 40.0 16.7 17.0 5 16.7 0.120))))

(ert-deftest smear-cursor-test-report-ignores-an-ordinary-round-trip ()
  ;; GIVEN a healthy flight on a display that answers in half a
  ;; millisecond
  ;; WHEN the verdict is drawn
  ;; THEN the round trip is not mentioned: every local display pays one
  ;; and a report that flags it says nothing.
  (should (equal "within budget"
                 (smear-cursor--report-verdict 0.9 16.7 20.0 5 16.7 0.0005)))
  ;; and an unmeasured drain reads as no drain rather than as trouble
  (should (equal "within budget"
                 (smear-cursor--report-verdict 0.9 16.7 20.0 5 16.7 nil))))

(ert-deftest smear-cursor-test-play-stats-carries-the-drain ()
  ;; GIVEN the module's seven-element account of a flight
  ;; WHEN it is read into a plist
  ;; THEN the drain comes with it, so the report has the number at all.
  (cl-letf (((symbol-function 'smear-cursor-x11--play-stats)
             (lambda (_stage) (vector 12 0.02 0.017 3 0.2 [0.016 0.017] 0.12))))
    (let ((s (smear-cursor--play-stats 'stage)))
      (should (= 12 (plist-get s :frames)))
      (should (< (abs (- 0.12 (plist-get s :drain))) 1e-9)))))

(ert-deftest smear-cursor-test-report-says-when-the-flight-was ()
  ;; GIVEN a smear that was recorded
  ;; WHEN the record is kept
  ;; THEN it is stamped with when.  Someone running this because
  ;; nothing appeared needs to know whether they are reading the flight
  ;; that did not appear or the last one that did.  Without the stamp
  ;; the two look identical.
  (let ((anim (smear-cursor--anim-create :born (float-time) :gc0 gc-elapsed))
        (smear-cursor--last-stats nil))
    ;; an animation that flew, so the thread's account belongs to it
    (setf (smear-cursor--anim-paint-frames anim) 3)
    (smear-cursor--record-stats anim 'gl (list :frames 3 :paint-total 0.001
                                               :gap-max 0.017 :gap-max-at 1
                                               :gap-sum 0.03 :gaps '(0.016)))
    (should (numberp (plist-get smear-cursor--last-stats :at)))
    (should (< (abs (- (float-time) (plist-get smear-cursor--last-stats :at)))
               5.0))))

(ert-deftest smear-cursor-test-nothing-is-traced-unless-asked ()
  ;; GIVEN the trace off, which is how it ships
  ;; WHEN a sample would be traced
  ;; THEN no file is touched.  This runs on every cursor movement, so
  ;; the off case has to cost a variable test and nothing else.
  (let ((smear-cursor-trace nil)
        (file (make-temp-name "/tmp/smear-trace-")))
    (smear-cursor--trace "anything")
    (should-not (file-exists-p file))))

(ert-deftest smear-cursor-test-a-trace-line-says-what-was-decided ()
  ;; GIVEN the trace pointed at a file
  ;; WHEN two samples are traced
  ;; THEN both are there, in order, stamped.  Appended rather than
  ;; written: the point of it is the run of decisions, not the last.
  (let ((file (make-temp-file "smear-trace-")))
    (unwind-protect
        (let ((smear-cursor-trace file))
          (smear-cursor--trace "first %d" 1)
          (smear-cursor--trace "second %d" 2)
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "first 1" (buffer-string)))
            (should (string-match-p "second 2" (buffer-string)))
            (should (< (string-match "first" (buffer-string))
                       (string-match "second" (buffer-string))))))
      (delete-file file))))

(ert-deftest smear-cursor-test-why-a-sample-did-not-smear ()
  ;; GIVEN samples that cannot smear, each for a different reason
  ;; WHEN the reason is asked for
  ;; THEN each says which.  "No trail appeared" has four causes that
  ;; look identical from outside Emacs, and the whole use of a trace is
  ;; telling them apart.
  (let ((r (vector 0.0 100.0 10.0 20.0)))
    (should (string-match-p "not on screen"
                            (smear-cursor--why-not nil r nil)))
    (should (string-match-p "nothing recorded"
                            (smear-cursor--why-not nil nil r)))
    ;; and a move worth drawing has no reason not to
    (cl-letf (((symbol-function 'smear-cursor--selecting-p) (lambda () nil))
              ((symbol-function 'smear-cursor--move-cells)
               (lambda (&rest _) 40.0))
              ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
              ((symbol-function 'window-frame) (lambda (&rest _) nil)))
      (should-not (smear-cursor--why-not nil (vector 0.0 0.0 10.0 20.0) r)))))

(ert-deftest smear-cursor-test-a-cursor-that-stayed-put-does-not-smear ()
  ;; GIVEN a repeat of `end-of-buffer' with the cursor already there
  ;; WHEN the reason is asked for
  ;; THEN it is the distance, not a fault.  This is the case that reads
  ;; from outside as a trail that failed: the line still pulses,
  ;; because the pulse follows the command and the trail follows the
  ;; cursor, and the cursor did not go anywhere.
  (let ((r (vector 0.0 892.0 10.0 22.0)))
    (cl-letf (((symbol-function 'smear-cursor--selecting-p) (lambda () nil))
              ((symbol-function 'smear-cursor--move-cells) (lambda (&rest _) 0.0))
              ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
              ((symbol-function 'window-frame) (lambda (&rest _) nil)))
      (should (string-match-p "did not move"
                              (smear-cursor--why-not nil r r))))))

(ert-deftest smear-cursor-test-an-older-module-still-reports ()
  ;; GIVEN a module built before the drain existed, which is what a
  ;; session that reloaded the Lisp without restarting is holding
  ;; WHEN its six-element account is read
  ;; THEN everything it does have comes through, and the drain is
  ;; simply absent.  Refusing the whole vector makes the report say
  ;; "no smear yet" to someone who has just moved the cursor, which is
  ;; worse than the missing number because it reads as a fault.
  (cl-letf (((symbol-function 'smear-cursor-x11--play-stats)
             (lambda (_stage) (vector 12 0.02 0.017 3 0.2 [0.016 0.017]))))
    (let ((s (smear-cursor--play-stats 'stage)))
      (should (= 12 (plist-get s :frames)))
      (should-not (plist-get s :drain)))))

(ert-deftest smear-cursor-test-a-sample-that-saw-nothing-tries-again ()
  ;; GIVEN a sample taken before redisplay scrolled the window, so
  ;; point is not where the window can see it
  ;; WHEN the sample runs
  ;; THEN another is scheduled rather than the movement being dropped.
  ;; Traced in a real session, this lost roughly every other jump: the
  ;; keypress produced no trail and no pulse at all, and the next one,
  ;; with the cursor already arrived, produced only a pulse.
  (let ((scheduled 0) (smear-cursor--sample-timer nil))
    (cl-letf (((symbol-function 'smear-cursor--sample) (lambda () nil))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled (1+ scheduled)) 'timer)))
      (smear-cursor--sample-soon 3)
      (should (= 1 scheduled))
      (should smear-cursor--sample-timer))))

(ert-deftest smear-cursor-test-a-sample-gives-up-eventually ()
  ;; GIVEN a cursor genuinely off screen, scrolled away from rather
  ;; than merely not redisplayed yet
  ;; WHEN the last try is spent
  ;; THEN it stops.  Chasing it forever would mean a timer running for
  ;; as long as the window stays where it is.
  (let ((scheduled 0) (smear-cursor--sample-timer nil))
    (cl-letf (((symbol-function 'smear-cursor--sample) (lambda () nil))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled (1+ scheduled)) 'timer)))
      (smear-cursor--sample-soon 0)
      (should (= 0 scheduled)))))

(ert-deftest smear-cursor-test-a-sample-that-worked-schedules-nothing ()
  ;; GIVEN a sample that measured point
  ;; WHEN it returns
  ;; THEN nothing further is scheduled: the retry is for the one case
  ;; that fails, not a poll.
  (let ((scheduled 0) (smear-cursor--sample-timer nil))
    (cl-letf (((symbol-function 'smear-cursor--sample) (lambda () t))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled (1+ scheduled)) 'timer)))
      (smear-cursor--sample-soon 3)
      (should (= 0 scheduled)))))

(ert-deftest smear-cursor-test-a-pulse-with-nothing-to-mark-tries-again ()
  ;; GIVEN a pulse asked for before the landing line is on screen
  ;; WHEN it finds no rect to draw over
  ;; THEN it tries again.  The same redisplay race loses the pulse and
  ;; the trail together, traced as "0 rect(s) -- nowhere to draw it"
  ;; beside "point is not on screen" at the same instant.
  (let ((scheduled 0) (smear-cursor--pulse-timer nil))
    (cl-letf (((symbol-function 'smear-cursor-pulse-line) (lambda () nil))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled (1+ scheduled)) 'timer)))
      (smear-cursor--pulse-try 3)
      (should (= 1 scheduled)))
    (cl-letf (((symbol-function 'smear-cursor-pulse-line) (lambda () t))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled (1+ scheduled)) 'timer)))
      (smear-cursor--pulse-try 3)
      (should (= 1 scheduled)))))

(ert-deftest smear-cursor-test-a-sample-with-nothing-to-do-does-not-retry ()
  ;; GIVEN a sample in a minibuffer, where there is nothing to smear
  ;; WHEN it returns
  ;; THEN it reports success, not failure.  Retrying would spend the
  ;; whole allowance on every command typed in the minibuffer, and the
  ;; answer would be the same each time.
  (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t)))
    (let ((smear-cursor-mode t))
      (should (smear-cursor--sample)))))

(ert-deftest smear-cursor-test-a-package-rebuilding-its-buffer-is-not-an-edit ()
  ;; GIVEN a read-only buffer being rebuilt by the package that owns
  ;; it, such as magit-status refreshing, which erases and reinserts
  ;; the lot repeatedly while forge fills a section in the background
  ;; WHEN the change hooks ask whose edit it is
  ;; THEN not the user's.  Being the buffer on screen is not enough,
  ;; since that is exactly true of a buffer you are watching a package
  ;; rewrite.  You cannot type into a read-only buffer, so a typing
  ;; effect has no business firing in one.
  (with-temp-buffer
    (insert "one section\nanother\n")
    (setq buffer-read-only t)
    (goto-char (point-min))
    (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
      (should-not (smear-cursor--own-edit-p (point-min) (point-max))))))

(ert-deftest smear-cursor-test-an-edit-away-from-point-is-not-the-users ()
  ;; GIVEN a writable buffer changed somewhere the cursor is not, by
  ;; `delete-trailing-whitespace' on save, a formatter, or a package
  ;; keeping a section up to date
  ;; WHEN the change hooks ask
  ;; THEN only the change under the cursor counts.  Typing and deleting
  ;; happen where the cursor is; that is what makes them worth marking.
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (goto-char 5)
    (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
      (should-not (smear-cursor--own-edit-p 60 70))
      (should (smear-cursor--own-edit-p 4 5)))))

(ert-deftest smear-cursor-test-an-edit-in-an-unseen-buffer-is-not-the-users ()
  ;; GIVEN a change in a buffer nobody is looking at
  ;; WHEN the change hooks ask
  ;; THEN no.  This covers vhdl-mode rewriting a hidden work buffer.
  (with-temp-buffer
    (cl-letf (((symbol-function 'window-buffer)
               (lambda (&rest _) (get-buffer-create " *elsewhere*"))))
      (should-not (smear-cursor--own-edit-p (point-min) (point-max))))))

(ert-deftest smear-cursor-test-a-magit-refresh-fires-no-delete-effect ()
  ;; GIVEN magit erasing its own read-only buffer
  ;; WHEN `before-change-functions' runs, as it does for every entry
  ;; forge adds while the section fills
  ;; THEN nothing flashes.  Each of those was a region-fade over text
  ;; the user never touched, one per entry, while they waited.
  (let ((fired 0))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq fired (1+ fired))))
              ((symbol-function 'smear-cursor--effect-for)
               (lambda (_) 'region-fade)))
      (with-temp-buffer
        (insert "a section\nanother\n")
        (setq buffer-read-only t)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'window-buffer)
                   (lambda (&rest _) (current-buffer))))
          (let ((smear-cursor-mode t))
            (smear-cursor--delete-forget)
            (smear-cursor--delete-fire (point-min) (point-max))))
        (should (= 0 fired))))))

(ert-deftest smear-cursor-test-a-real-deletion-still-fires ()
  ;; GIVEN the user killing a region they selected
  ;; WHEN `before-change-functions' runs
  ;; THEN it still flashes.  The guard has to turn away a package's
  ;; edits without turning away the only case the effect exists for.
  (let ((fired 0))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq fired (1+ fired))))
              ((symbol-function 'smear-cursor--effect-for)
               (lambda (_) 'region-fade)))
      (with-temp-buffer
        (insert "some text the user selected\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'window-buffer)
                   (lambda (&rest _) (current-buffer))))
          (let ((smear-cursor-mode t))
            (smear-cursor--delete-forget)
            (smear-cursor--delete-fire (point-min) (point-max))))
        (should (= 1 fired))))))

(ert-deftest smear-cursor-test-an-effect-may-be-worked-out-when-it-plays ()
  ;; GIVEN an effect defined as a function rather than as a plist
  ;; WHEN it is looked up
  ;; THEN the function is called and its plist is what comes back.  An
  ;; effect whose settings are user options has to be built when it
  ;; plays: built once at definition time it would hold whatever the
  ;; options were before the user's init file got to them.
  (let ((smear-cursor--effects (make-hash-table :test 'eq))
        (calls 0))
    (smear-cursor-define-effect 'made-to-order
                                (lambda () (setq calls (1+ calls))
                                  (list :duration 0.5)))
    (should (equal '(:duration 0.5) (smear-cursor-effect 'made-to-order)))
    (should (equal '(:duration 0.5) (smear-cursor-effect 'made-to-order)))
    (should (= 2 calls))))

(ert-deftest smear-cursor-test-a-plain-setq-retunes-the-typing-blink ()
  ;; GIVEN the blink's knobs changed with `setq', as an init file does
  ;; WHEN the effect next plays
  ;; THEN it plays at the new settings.  A `defcustom' :set is the
  ;; obvious way to rebuild it, but `setq' does not run one, so the
  ;; option would read as ignored.
  (let ((smear-cursor-type-blink-duration 0.5)
        (smear-cursor-type-blink-strength 0.9)
        (smear-cursor-type-blink-radius 20))
    (let ((e (smear-cursor-effect 'type-blink)))
      (should (= 0.5 (plist-get e :duration)))
      (should (= 0.9 (plist-get (car (plist-get e :layers)) :alpha)))
      (should (= 20 (plist-get (car (plist-get e :layers)) :radius))))))

(ert-deftest smear-cursor-test-the-typing-blink-arrives-holds-and-goes ()
  ;; GIVEN the blink's envelope
  ;; WHEN it is read across its life
  ;; THEN it starts at nothing, ends at nothing, and *holds* near its
  ;; peak in between.
  ;;
  ;; The hold is what makes it visible.  Rising to full on one frame
  ;; and falling away over the rest spends a single frame, sixteen
  ;; milliseconds, at the brightness it was set to, and the eye
  ;; integrates the rest into a smudge.  The alpha is not the problem
  ;; there; the envelope is.
  (let* ((e (smear-cursor-effect 'type-blink))
         (env (plist-get (car (plist-get e :layers)) :envelope))
         (bright 0) (steps 40))
    (should (= 0.0 (smear-cursor--envelope-at env 0.0)))
    (should (= 0.0 (smear-cursor--envelope-at env 1.0)))
    (dotimes (i steps)
      (when (>= (smear-cursor--envelope-at env (/ (float i) steps)) 0.8)
        (setq bright (1+ bright))))
    ;; a third of its life at four fifths or better
    (should (>= bright (/ steps 3)))))

(ert-deftest smear-cursor-test-the-typing-blink-borrows-the-trails-colour ()
  ;; GIVEN no colour set for the blink
  ;; WHEN it plays
  ;; THEN it takes the trail's.  It took the cursor's first, which is
  ;; right in isolation and wrong beside a red laser: the two are one
  ;; thing to look at and should be one colour.
  (let ((smear-cursor-trail-style 'laser)
        (smear-cursor-color nil)
        (smear-cursor-effect-color nil)
        (smear-cursor-type-blink-color nil))
    (should (equal [255 45 30]
                   (smear-cursor--effect-color
                    (smear-cursor-effect 'type-blink)))))
  ;; and an explicit colour wins
  (let ((smear-cursor-effect-color nil)
        (smear-cursor-type-blink-color "#ff0000"))
    (should (equal (smear-cursor--color-rgb "#ff0000")
                   (smear-cursor--effect-color
                    (smear-cursor-effect 'type-blink))))))

(ert-deftest smear-cursor-test-setting-the-colour-beats-the-style ()
  ;; GIVEN a style that carries its own colour (`laser' is red on
  ;;       purpose) and a user who set `smear-cursor-color'
  ;; WHEN a layer is coloured
  ;; THEN the setting wins.  If the style won instead, setting the
  ;; colour on any style but `plain' would do nothing and say nothing
  ;; about it.
  (let ((style '(:color [255 45 30]))
        (smear-cursor-color "#00ff00"))
    (should (equal (smear-cursor--color-rgb "#00ff00")
                   (smear-cursor--layer-color '(:shape quad) style [1 2 3]))))
  ;; a layer's own colour still wins over both: the hot core of a laser
  ;; dot is near-white however red the rest of it is
  (let ((smear-cursor-color "#00ff00"))
    (should (equal [255 240 232]
                   (smear-cursor--layer-color '(:color [255 240 232])
                                              '(:color [255 45 30]) [1 2 3])))))

(ert-deftest smear-cursor-test-the-trail-colour-is-one-question-with-one-answer ()
  ;; GIVEN the three places a trail colour can come from
  ;; WHEN each is asked in turn
  ;; THEN the order holds: the setting, then the style, then the cursor.
  ;; Effects ask this too, so it had to stop being spelled out twice.
  (let ((smear-cursor-trail-style 'laser))
    (let ((smear-cursor-color "#00ff00"))
      (should (equal (smear-cursor--color-rgb "#00ff00")
                     (smear-cursor--trail-color))))
    (let ((smear-cursor-color nil))
      (should (equal [255 45 30] (smear-cursor--trail-color))))))

(ert-deftest smear-cursor-test-an-effect-can-wear-the-trails-colour ()
  ;; GIVEN an effect whose colour is `trail'
  ;; WHEN it is drawn
  ;; THEN it comes out the colour of the trail, so the two read as one
  ;; thing rather than as two packages.
  (let ((smear-cursor-trail-style 'laser)
        (smear-cursor-color nil)
        (smear-cursor-effect-color nil))
    (should (equal [255 45 30] (smear-cursor--effect-color '(:color trail)))))
  ;; and `smear-cursor-effect-color' says so for all of them at once
  (let ((smear-cursor-trail-style 'laser)
        (smear-cursor-color nil)
        (smear-cursor-effect-color 'trail))
    (should (equal [255 45 30] (smear-cursor--effect-color '(:color [1 2 3]))))))

(ert-deftest smear-cursor-test-effects-have-one-knob-for-how-loud ()
  ;; GIVEN a layer at four tenths solid
  ;; WHEN the strength is turned up
  ;; THEN its envelope is scaled by it.  One control covers every
  ;; layer, rather than a strength per effect that has to be kept in
  ;; balance with the others by hand.
  (let ((layer '(:alpha 0.4 :envelope ((0.0 . 1.0) (1.0 . 1.0)))))
    (let ((smear-cursor-effect-strength 1.0))
      (should (< (abs (- 1.0 (smear-cursor--effect-scale layer 0.5))) 1e-9)))
    (let ((smear-cursor-effect-strength 2.0))
      (should (< (abs (- 2.0 (smear-cursor--effect-scale layer 0.5))) 1e-9)))
    ;; and it cannot be turned up past solid: an effect that covers the
    ;; text it is marking has stopped marking it
    (let ((smear-cursor-effect-strength 99.0))
      (should (< (abs (- 2.5 (smear-cursor--effect-scale layer 0.5))) 1e-9)))))

(ert-deftest smear-cursor-test-a-copy-takes-its-bounds-from-the-command ()
  ;; GIVEN `M-w' with no active region, the "slick copy" case where a
  ;;       package hands `kill-ring-save' the current line instead
  ;; WHEN the copy effect fires
  ;; THEN it marks what was actually copied.  Asking the region gives
  ;; nothing at all in that case, so the commonest way of copying would
  ;; mark nothing and look broken.
  (let (marked (smear-cursor--copy-last nil))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (occasion beg end) (setq marked (list occasion beg end)))))
      (let ((smear-cursor-mode t))
        (smear-cursor--copy-effect 10 40)
        (should (equal '(copy 10 40) marked))
        ;; bounds either way round.  The debounce is cleared between
        ;; these: what is under test is which bounds are used, and it
        ;; has its own test next door.
        (setq marked nil smear-cursor--copy-last nil)
        (smear-cursor--copy-effect 40 10)
        (should (equal '(copy 10 40) marked))
        ;; and nothing at all for an empty copy
        (setq marked nil smear-cursor--copy-last nil)
        (smear-cursor--copy-effect 10 10)
        (should-not marked)))))

(ert-deftest smear-cursor-test-one-copy-is-marked-once ()
  ;; GIVEN `kill-ring-save', which calls `copy-region-as-kill', both of
  ;;       them advised so that either route is caught
  ;; WHEN one copy goes through both
  ;; THEN it is marked once.  Twice restarts the flight a few frames
  ;; in, so the flash visibly resets partway through.
  (let ((marked 0)
        (smear-cursor--copy-last nil))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq marked (1+ marked))))
              ((symbol-function 'float-time) (lambda (&rest _) 100.0)))
      (let ((smear-cursor-mode t))
        (smear-cursor--copy-effect 10 40)
        (smear-cursor--copy-effect 10 40)
        (should (= 1 marked))
        ;; a different region is a different copy
        (smear-cursor--copy-effect 50 60)
        (should (= 2 marked)))))
  ;; and the same region copied again later is marked again
  (let ((marked 0)
        (smear-cursor--copy-last nil)
        (now 100.0))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq marked (1+ marked))))
              ((symbol-function 'float-time) (lambda (&rest _) now)))
      (let ((smear-cursor-mode t))
        (smear-cursor--copy-effect 10 40)
        (setq now 105.0)
        (smear-cursor--copy-effect 10 40)
        (should (= 2 marked))))))

(ert-deftest smear-cursor-test-an-effect-is-lit-evenly-across-itself ()
  ;; GIVEN an effect layer with an alpha and no run of its own
  ;; WHEN it is prepared for drawing
  ;; THEN both ends carry the same alpha.  `smear-cursor--layer-stops'
  ;; ramps a layer to a twelfth of its alpha across the shape, which
  ;; suits a trail: bright where the cursor is, gone where it came
  ;; from.  An effect is not moving.  The same ramp over a copied
  ;; region lights one edge and leaves the other invisible, and turning
  ;; the alpha up does not help, because the far end stays at eight per
  ;; cent of whatever it is turned up to.
  (let* ((flat (smear-cursor--effect-layer '(:shape quad :alpha 0.6)))
         (stops (smear-cursor--layer-stops flat)))
    (should (< (abs (- 0.6 (cdr (car stops)))) 1e-9))
    (should (< (abs (- 0.6 (cdr (car (last stops))))) 1e-9))))

(ert-deftest smear-cursor-test-a-dot-keeps-its-soft-edge ()
  ;; GIVEN a radial effect layer, the typing blink
  ;; WHEN it is prepared
  ;; THEN it is bright in the middle, well over half that at the
  ;; halfway stop, and nothing at all at the rim.  Ending the gradient
  ;; part way up does not soften the edge, it draws one: nothing is
  ;; painted past the last stop, so a glow that ends at half opacity
  ;; ends in a hard circle at that radius.  The middle stop is what
  ;; keeps it from fading to nothing too early to see.
  (let* ((dot (smear-cursor--effect-layer '(:shape radial :alpha 0.6)))
         (stops (smear-cursor--layer-stops dot)))
    (should (< (abs (- 0.6 (cdr (car stops)))) 1e-9))
    (should (= 0.0 (cdr (car (last stops)))))
    (let ((mid (cdr (nth 1 stops))))
      (should (> mid 0.15))
      (should (< mid 0.6)))))

(ert-deftest smear-cursor-test-an-effect-that-asked-for-a-run-keeps-it ()
  ;; GIVEN a layer that set its own stops
  ;; WHEN it is prepared
  ;; THEN they are left alone.  Someone writing an effect with a
  ;; gradient in it meant the gradient.
  (let ((own '(:shape quad :stops ((0.0 . 0.9) (1.0 . 0.1)))))
    (should (equal own (smear-cursor--effect-layer own)))))

(ert-deftest smear-cursor-test-a-flash-marks-the-region-or-the-line ()
  ;; GIVEN a request to see an effect now
  ;; WHEN there is a region, and when there is not
  ;; THEN it plays over the region, or over the current line.  Tuning
  ;; these meant editing a number and then waiting for the occasion to
  ;; come round again, which is three rounds of "still too subtle" and
  ;; no way to tell a weak effect from one that never fired.
  (let (played)
    (cl-letf (((symbol-function 'smear-cursor--play-effect)
               (lambda (_w name rects _t) (setq played (list name (length rects)))))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor--region-rects)
               (lambda (_w beg end) (list (list 'region beg end))))
              ((symbol-function 'smear-cursor--line-rect)
               (lambda (_w) (list 'line))))
      (with-temp-buffer
        (insert "one two three\nfour five six\n")
        (goto-char 5)
        (cl-letf (((symbol-function 'region-active-p) (lambda () t))
                  ((symbol-function 'region-beginning) (lambda () 2))
                  ((symbol-function 'region-end) (lambda () 9)))
          (smear-cursor-flash 'region-flash)
          (should (equal '(region-flash 1) played)))
        (setq played nil)
        (cl-letf (((symbol-function 'region-active-p) (lambda () nil)))
          (smear-cursor-flash 'region-flash)
          (should (equal '(region-flash 1) played)))))))

(ert-deftest smear-cursor-test-a-flash-takes-bounds-when-it-is-given-them ()
  ;; GIVEN a caller that knows exactly what it wants marked, such as a
  ;;       package or a command someone wrote
  ;; WHEN it says so
  ;; THEN those bounds are used rather than the region or the line.
  ;; Everything else here decides for itself when to fire, so a caller
  ;; that wants to decide needs this entry point.
  (let (played)
    (cl-letf (((symbol-function 'smear-cursor--play-effect)
               (lambda (_w name rects _t &optional _f)
                 (setq played (list name rects))))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor--region-rects)
               (lambda (_w beg end) (list (list 'between beg end)))))
      (smear-cursor-flash 'region-fade 100 140)
      (should (equal '(region-fade ((between 100 140))) played)))))

(ert-deftest smear-cursor-test-a-deletion-is-photographed-before-it-goes ()
  ;; GIVEN text about to be deleted
  ;; WHEN the delete effect fires, from `before-change-functions'
  ;; THEN the pixels are photographed first, and the flight is told to
  ;; play over the photograph.
  ;;
  ;; Without it the flash marks the wrong thing, and visibly: the first
  ;; frame is painted a frame after the effect is handed over, by which
  ;; time the deletion has happened and the text has closed up, so the
  ;; region lights over whatever moved into the gap.
  (let (froze played)
    (cl-letf (((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage))
              ((symbol-function 'smear-cursor--rect-in-frame) (lambda (_w r) r))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor-x11--freeze)
               (lambda (&rest args) (setq froze args) t))
              ((symbol-function 'smear-cursor-x11--play)
               (lambda (&rest args) (setq played args) t)))
      (smear-cursor--play-effect nil 'region-fade
                                 (list (vector 10.0 20.0 100.0 18.0))
                                 smear-cursor--track-occasion t)
      ;; photographed, and at the effect's own box
      (should (eq 'stage (car froze)))
      (should (= 5 (length froze)))
      ;; and the flight knows to use it
      (should (nth 5 played)))))

(ert-deftest smear-cursor-test-an-ordinary-effect-photographs-nothing ()
  ;; GIVEN an effect over text that is staying put: a copy, or a pulse
  ;; WHEN it fires
  ;; THEN nothing is photographed and the flight is an ordinary one.
  ;; Freezing costs a pixmap and a flush, and every other effect wants
  ;; the live text underneath it anyway.
  (let (froze played)
    (cl-letf (((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage))
              ((symbol-function 'smear-cursor--rect-in-frame) (lambda (_w r) r))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor-x11--freeze)
               (lambda (&rest args) (setq froze args) t))
              ((symbol-function 'smear-cursor-x11--play)
               (lambda (&rest args) (setq played args) t)))
      (smear-cursor--play-effect nil 'region-flash
                                 (list (vector 10.0 20.0 100.0 18.0))
                                 smear-cursor--track-occasion)
      (should-not froze)
      (should-not (nth 5 played)))))

(ert-deftest smear-cursor-test-the-delete-effect-asks-to-be-frozen ()
  ;; GIVEN a deletion the user made
  ;; WHEN `before-change-functions' runs
  ;; THEN the effect is played frozen.  This is the only occasion that
  ;; marks something which will not be there to be marked.
  (let (args)
    (cl-letf (((symbol-function 'smear-cursor--play-effect)
               (lambda (&rest a) (setq args a)))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'region-fade))
              ((symbol-function 'smear-cursor--own-edit-p) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor--region-rects)
               (lambda (&rest _) (list (vector 0.0 0.0 10.0 10.0))))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (let ((smear-cursor-mode t))
        (smear-cursor--delete-forget)
        (smear-cursor--delete-fire 10 40)
        (should (nth 4 args))))))

(ert-deftest smear-cursor-test-one-character-going-is-not-worth-marking ()
  ;; GIVEN a backspace
  ;; WHEN `before-change-functions' runs
  ;; THEN nothing is marked.  A flash the size of a word over a
  ;; character that has gone reads as more having happened than did.
  ;; It also costs a photograph and a flush on every keystroke of a
  ;; held-down backspace, which is felt.
  (let (fired)
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq fired t)))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'region-fade))
              ((symbol-function 'smear-cursor--own-edit-p) (lambda (&rest _) t)))
      (let ((smear-cursor-mode t)
            (smear-cursor-delete-min-chars 2))
        (smear-cursor--delete-forget)
        (smear-cursor--delete-fire 10 11)
        (should-not fired)
        ;; two is a word going, or the start of a region
        (smear-cursor--delete-forget)
        (smear-cursor--delete-fire 10 12)
        (should fired)))))

(ert-deftest smear-cursor-test-the-deletion-threshold-is-a-setting ()
  ;; GIVEN someone who wants every deletion marked, or only large ones
  ;; WHEN the threshold is changed
  ;; THEN it is obeyed at both ends.  One is "mark everything", which
  ;; is what it did before there was a threshold at all.
  (let (fired)
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq fired t)))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'region-fade))
              ((symbol-function 'smear-cursor--own-edit-p) (lambda (&rest _) t)))
      (let ((smear-cursor-mode t))
        (let ((smear-cursor-delete-min-chars 1))
          (setq fired nil)
          (smear-cursor--delete-forget)
          (smear-cursor--delete-fire 10 11)
          (should fired))
        (let ((smear-cursor-delete-min-chars 40))
          (setq fired nil)
          (smear-cursor--delete-forget)
          (smear-cursor--delete-fire 10 30)
          (should-not fired))))))

(ert-deftest smear-cursor-test-the-pulse-wears-the-trails-colour ()
  ;; GIVEN a red laser trail
  ;; WHEN the line pulse is built
  ;; THEN it is red.  It was amber whatever the trail was, which beside
  ;; a red laser reads as two packages that happen to be installed
  ;; together.
  (let ((smear-cursor-trail-style 'laser)
        (smear-cursor-color nil)
        (smear-cursor-effect-color nil))
    (should (equal [255 45 30]
                   (smear-cursor--effect-color
                    (smear-cursor-effect 'line-pulse)))))
  ;; and it follows the trail's colour when that is set, rather than
  ;; the style's
  (let ((smear-cursor-trail-style 'laser)
        (smear-cursor-color "#00ff00")
        (smear-cursor-effect-color nil))
    (should (equal (smear-cursor--color-rgb "#00ff00")
                   (smear-cursor--effect-color
                    (smear-cursor-effect 'line-pulse))))))

(ert-deftest smear-cursor-test-a-lingering-trail-gets-a-lingering-pulse ()
  ;; GIVEN two styles whose springs differ: `laser' hangs about with a
  ;;       tail stiffness of a sixth, `ghost' snaps at three quarters
  ;; WHEN each one's pulse is built
  ;; THEN the pulse lasts longer for the one that lingers.  How long a
  ;; trail hangs about is as much of its character as its colour (see
  ;; `smear-cursor--spring'), so an effect meaning to feel like the
  ;; trail has to read it rather than pick its own pace.
  (let ((slack (let ((smear-cursor-trail-style 'laser))
                 (plist-get (smear-cursor-effect 'line-pulse) :duration)))
        (stiff (let ((smear-cursor-trail-style 'ghost))
                 (plist-get (smear-cursor-effect 'line-pulse) :duration))))
    (should (> slack stiff))))

(ert-deftest smear-cursor-test-the-pulse-is-as-soft-as-the-trail ()
  ;; GIVEN a soft style and a hard-edged one
  ;; WHEN their pulses are built
  ;; THEN the soft one's pulse is blurred and the other's is not.
  ;; `comet' is a wide soft smudge and `ribbon' a thin filament; a
  ;; pulse that looked the same beside both would belong to neither.
  (let ((soft (let ((smear-cursor-trail-style 'comet))
                (plist-get (car (plist-get (smear-cursor-effect 'line-pulse)
                                           :layers))
                           :blur)))
        (hard (let ((smear-cursor-trail-style 'ribbon))
                (plist-get (car (plist-get (smear-cursor-effect 'line-pulse)
                                           :layers))
                           :blur))))
    (should (> soft 0))
    (should (= 0 hard))))

(ert-deftest smear-cursor-test-the-pulses-pace-is-bounded ()
  ;; GIVEN springs at absurd settings, which a user may well write
  ;; WHEN the pace is derived
  ;; THEN it stays within half and one and a half.  A pulse of four
  ;; seconds is not a pulse, and one of ten milliseconds is not seen.
  (let ((smear-cursor-trail-style 'plain))
    (let ((smear-cursor-stiffness-tail 0.0001))
      (should (<= (smear-cursor--trail-linger) 1.5)))
    (let ((smear-cursor-stiffness-tail 50.0))
      (should (>= (smear-cursor--trail-linger) 0.5)))))

(ert-deftest smear-cursor-test-the-trails-strength-is-readable ()
  ;; GIVEN styles that differ in how solid they get
  ;; WHEN their strongest layer is asked for
  ;; THEN it is found, through `:alpha' and through `:stops' alike.
  ;; `laser' reaches solid at its core; `ribbon' is a filament.
  (let ((smear-cursor-trail-style 'laser))
    (should (= 1.0 (smear-cursor--trail-strongest))))
  (let ((smear-cursor-trail-style 'ribbon))
    ;; its one layer carries stops rather than an alpha
    (should (= 1.0 (smear-cursor--trail-strongest))))
  (let ((smear-cursor-trail-style 'ghost))
    (should (= 0.85 (smear-cursor--trail-strongest)))))

(ert-deftest smear-cursor-test-the-pulse-is-as-strong-as-the-trail-allows ()
  ;; GIVEN a trail that reaches solid
  ;; WHEN the pulse is built
  ;; THEN it is a fraction of that rather than a fixed quarter.
  ;;
  ;; It cannot simply match: the trail is four layers stacked over a
  ;; few hundred pixels and the pulse is one layer over a whole line,
  ;; and a line at solid is a line you cannot read.  A fixed quarter
  ;; against a trail reaching one makes the pulse the weaker of the
  ;; two.
  (let* ((smear-cursor-trail-style 'laser)
         (smear-cursor-pulse-strength 0.45)
         (a (plist-get (car (plist-get (smear-cursor-effect 'line-pulse)
                                       :layers))
                       :alpha)))
    (should (< (abs (- 0.45 a)) 1e-6)))
  ;; and the setting scales it
  (let* ((smear-cursor-trail-style 'laser)
         (smear-cursor-pulse-strength 0.8)
         (a (plist-get (car (plist-get (smear-cursor-effect 'line-pulse)
                                       :layers))
                       :alpha)))
    (should (< (abs (- 0.8 a)) 1e-6))))

(ert-deftest smear-cursor-test-a-pulse-cannot-hide-the-line-it-marks ()
  ;; GIVEN someone turning it all the way up
  ;; WHEN the pulse is built
  ;; THEN it stops short of solid.  The point of compositing over the
  ;; text rather than replacing its face is that the text stays
  ;; readable; an opaque wash gives that away for nothing.
  (let* ((smear-cursor-trail-style 'laser)
         (smear-cursor-pulse-strength 5.0)
         (a (plist-get (car (plist-get (smear-cursor-effect 'line-pulse)
                                       :layers))
                       :alpha)))
    (should (<= a 0.85))))

(ert-deftest smear-cursor-test-the-pulse-holds-near-its-peak ()
  ;; GIVEN the pulse's envelope
  ;; WHEN it is read across its life
  ;; THEN it holds high for a good part of it rather than touching the
  ;; top and falling away.  The same fault as the typing blink had:
  ;; over thirty frames it spent three near full and twenty-seven
  ;; fading, which the eye reads as faint.
  (let* ((env (plist-get (car (plist-get (smear-cursor-effect 'line-pulse)
                                         :layers))
                         :envelope))
         (bright 0) (steps 40))
    (should (= 0.0 (smear-cursor--envelope-at env 0.0)))
    (should (= 0.0 (smear-cursor--envelope-at env 1.0)))
    (dotimes (i steps)
      (when (>= (smear-cursor--envelope-at env (/ (float i) steps)) 0.8)
        (setq bright (1+ bright))))
    (should (>= bright (/ steps 3)))))

(ert-deftest smear-cursor-test-a-style-with-a-hot-middle-lends-it ()
  ;; GIVEN `laser', whose dot carries its own near-white colour while
  ;;       the rest of it is red
  ;; WHEN the trail's core colour is asked for
  ;; THEN that is what comes back.  A style says its middle is a
  ;; different colour by giving one layer a `:color' of its own, and
  ;; that is the only thing that distinguishes a beam from a bar.
  (let ((smear-cursor-trail-style 'laser))
    (should (equal [255 240 232] (smear-cursor--trail-core-color))))
  ;; and a style whose layers are all one colour has no core to lend
  (let ((smear-cursor-trail-style 'comet))
    (should-not (smear-cursor--trail-core-color))))

(ert-deftest smear-cursor-test-the-laser-pulse-is-a-beam ()
  ;; GIVEN the laser trail
  ;; WHEN the line pulse is built
  ;; THEN it is two layers: the red beam, and a brighter near-white
  ;; band inset into the middle of it.  Which is what a laser pointer
  ;; looks like, and what the pulse beside one should look like.
  (let* ((smear-cursor-trail-style 'laser)
         (layers (plist-get (smear-cursor-effect 'line-pulse) :layers))
         (beam (nth 0 layers))
         (core (nth 1 layers)))
    (should (= 2 (length layers)))
    (should (equal [255 240 232] (plist-get core :color)))
    ;; inset, so it is a band down the middle rather than the whole line
    (should (< (plist-get core :grow) 0))
    ;; and hotter than the beam around it
    (should (> (plist-get core :alpha) (plist-get beam :alpha)))))

(ert-deftest smear-cursor-test-a-plain-trail-gets-a-plain-pulse ()
  ;; GIVEN a style with no colour of its own anywhere in it
  ;; WHEN the pulse is built
  ;; THEN it is the single layer it always was.  A core invented for a
  ;; style that never asked for one is a look nobody chose.
  (let ((smear-cursor-trail-style 'comet))
    (should (= 1 (length (plist-get (smear-cursor-effect 'line-pulse)
                                    :layers))))))

(ert-deftest smear-cursor-test-the-beams-core-cannot-hide-the-line ()
  ;; GIVEN the strength turned all the way up
  ;; WHEN the beam is built
  ;; THEN the core stops short of solid.  It lies across the middle of
  ;; the glyphs, which is the part that carries their shape.
  (let* ((smear-cursor-trail-style 'laser)
         (smear-cursor-pulse-strength 5.0)
         (core (nth 1 (plist-get (smear-cursor-effect 'line-pulse) :layers))))
    (should (<= (plist-get core :alpha) 0.8))))

(ert-deftest smear-cursor-test-an-effect-layer-keeps-its-own-colour ()
  ;; GIVEN an effect whose layers are not all one colour, such as the
  ;;       laser pulse, red with a near-white core
  ;; WHEN the layers are handed to the renderer
  ;; THEN each carries its own.  A trail resolves this per layer
  ;; already (`smear-cursor--layer-color').  One colour for the whole
  ;; effect gives the core the same red as the beam around it, leaving
  ;; no core at all.
  (should (equal [255 240 232]
                 (smear-cursor--effect-layer-color
                  '(:shape quad :color [255 240 232]) [255 45 30])))
  (should (equal [255 45 30]
                 (smear-cursor--effect-layer-color
                  '(:shape quad) [255 45 30])))
  ;; a name works as well as a triple
  (should (equal (smear-cursor--color-rgb "#ffffff")
                 (smear-cursor--effect-layer-color
                  '(:shape quad :color "#ffffff") [255 45 30]))))

(ert-deftest smear-cursor-test-a-flight-that-never-flew-reports-nothing ()
  ;; GIVEN an animation torn down without ever handing a flight over
  ;; WHEN its stats are recorded, with the thread still holding the
  ;;      last real flight's account
  ;; THEN nothing is recorded, and the previous reading stands.
  ;;
  ;; Otherwise the report puts one flight's frames against another's
  ;; clock and prints things like "17 frames in 2 ms", which is not a
  ;; measurement of anything.
  (let* ((anim (smear-cursor--anim-create :born (float-time) :gc0 gc-elapsed))
         (stale (list :frames 17 :paint-total 0.037 :gap-max 0.025
                      :gap-max-at 17 :gap-sum 0.28 :gaps '(0.016)))
         (smear-cursor--last-stats 'untouched))
    (should (= 0 (smear-cursor--anim-paint-frames anim)))
    (smear-cursor--record-stats anim 'gl stale)
    (should (eq 'untouched smear-cursor--last-stats))))

(ert-deftest smear-cursor-test-a-flight-that-flew-is-reported ()
  ;; GIVEN an animation that did hand a flight over
  ;; WHEN its stats are recorded
  ;; THEN the thread's account is used, since Lisp never saw those
  ;; frames go by and its own counters would call the flight nothing.
  (let* ((anim (smear-cursor--anim-create :born (float-time) :gc0 gc-elapsed))
         (played (list :frames 17 :paint-total 0.037 :gap-max 0.025
                       :gap-max-at 17 :gap-sum 0.28 :gaps '(0.016)))
         (smear-cursor--last-stats nil))
    (setf (smear-cursor--anim-paint-frames anim) 17)
    (smear-cursor--record-stats anim 'gl played)
    (should (= 17 (plist-get smear-cursor--last-stats :frames)))
    (should (plist-get smear-cursor--last-stats :threaded))))

(ert-deftest smear-cursor-test-effects-run-at-the-frame-rate-by-default ()
  ;; GIVEN the shipped setting
  ;; WHEN an effect's rate is asked for
  ;; THEN it is the frame rate, not half of it.
  ;;
  ;; It was half for a while, when every frame of an effect was a box
  ;; uploaded to the display and halving them halved the cost.  A still
  ;; effect is uploaded once now whatever its length, so the frames
  ;; after the first are free and there is nothing left to buy by
  ;; dropping them.  Sixty is what the eye wants.
  (let ((smear-cursor-fps 60)
        (smear-cursor-effect-fps nil))
    (should (= 60.0 (smear-cursor--effect-rate)))))

(ert-deftest smear-cursor-test-an-effect-runs-at-its-own-frame-rate ()
  ;; GIVEN a trail at sixty frames a second and effects at thirty
  ;; WHEN an effect is played
  ;; THEN it is laid out and played at thirty.
  ;;
  ;; A trail is motion and wants every frame the screen has.  An effect
  ;; holds still and moves its alpha, so a fade at thirty looks like a
  ;; fade at sixty and costs half as many frames to play.
  (let (played)
    (cl-letf (((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage))
              ((symbol-function 'smear-cursor--rect-in-frame) (lambda (_w r) r))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor-x11--play)
               (lambda (_s _t layers frames fps &rest _)
                 (setq played (list (/ (length frames)
                                       (smear-cursor--flight-stride
                                        (length layers)))
                                    fps)))))
      (let ((smear-cursor-fps 60)
            (smear-cursor-effect-fps 30)
            (smear-cursor-trail-style 'plain))
        (smear-cursor--play-effect nil 'region-flash
                                   (list (vector 0.0 0.0 100.0 20.0))
                                   smear-cursor--track-occasion)
        ;; region-flash lasts 0.26 s: eight frames at thirty, not sixteen
        (should (= 8 (nth 0 played)))
        (should (= 30.0 (nth 1 played)))))))

(ert-deftest smear-cursor-test-an-effect-never-outruns-the-trail ()
  ;; GIVEN someone who set the effect rate above the frame rate
  ;; WHEN an effect is played
  ;; THEN it is capped at the frame rate.  Frames the screen cannot
  ;; show are bytes pushed for nothing.
  (let ((smear-cursor-fps 30)
        (smear-cursor-effect-fps 120))
    (should (= 30.0 (smear-cursor--effect-rate)))))

(ert-deftest smear-cursor-test-an-effect-of-one-envelope-is-still ()
  ;; GIVEN effects whose layers all rise and fall together, and one
  ;;       whose layers do not
  ;; WHEN each is asked whether it holds still
  ;; THEN only the first kind says yes.
  ;;
  ;; A still flight is rendered and uploaded once and composited from
  ;; there, at one alpha for the whole stamp.  Layers on different
  ;; envelopes cannot share one alpha, so they have to be drawn the
  ;; ordinary way.  A flight that said otherwise would show its first
  ;; frame for the whole of its life.
  (should (smear-cursor--effect-still-p
           '(:layers ((:shape quad :envelope ((0.0 . 0.0) (1.0 . 1.0)))
                      (:shape quad :envelope ((0.0 . 0.0) (1.0 . 1.0)))))))
  (should-not (smear-cursor--effect-still-p
               '(:layers ((:shape quad :envelope ((0.0 . 0.0) (1.0 . 1.0)))
                          (:shape quad :envelope ((0.0 . 1.0) (1.0 . 0.0)))))))
  ;; one layer is trivially of one mind
  (should (smear-cursor--effect-still-p '(:layers ((:shape quad))))))

(ert-deftest smear-cursor-test-the-effects-that-ship-hold-still ()
  ;; GIVEN the effects that hold one shape
  ;; WHEN each is asked
  ;; THEN all of them say so, which is what makes the saving worth
  ;; having: each is drawn and uploaded once and then stamped again at
  ;; another alpha.  The two ring effects change size instead and are
  ;; covered by `smear-cursor-test-the-ring-effects-do-not-claim-to-hold-still'.
  (dolist (name '(line-pulse region-flash region-fade spark-dot type-blink))
    (should (smear-cursor--effect-still-p (smear-cursor-effect name)))))

(ert-deftest smear-cursor-test-a-still-effect-says-so-to-the-module ()
  ;; GIVEN a still effect
  ;; WHEN it is played
  ;; THEN the module is told, in the seventh argument.
  (let (played)
    (cl-letf (((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage))
              ((symbol-function 'smear-cursor--rect-in-frame) (lambda (_w r) r))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor-x11--play)
               (lambda (&rest args) (setq played args) t)))
      (smear-cursor--play-effect nil 'region-flash
                                 (list (vector 10.0 20.0 100.0 18.0))
                                 smear-cursor--track-occasion)
      (should (= 7 (length played)))
      (should (nth 6 played)))))

(ert-deftest smear-cursor-test-nothing-is-marked-in-the-minibuffer ()
  ;; GIVEN typing and deleting in the minibuffer
  ;; WHEN the effects would fire
  ;; THEN they do not.
  ;;
  ;; It is one line tall and you are already looking at it: nothing
  ;; there needs marking to be found, and a flash under a completion
  ;; prompt is in the way of the thing you went there to read.  The
  ;; smear keeps out of the minibuffer for the same reason.  See
  ;; `smear-cursor--sample'.
  (let ((fired 0))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq fired (1+ fired))))
              ((symbol-function 'smear-cursor--play-effect)
               (lambda (&rest _) (setq fired (1+ fired))))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'region-fade))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (&rest _) (vector 0.0 0.0 10.0 20.0)))
              ((symbol-function 'minibufferp) (lambda (&rest _) t))
              ((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
      (with-temp-buffer
        (insert "some text being typed at a prompt")
        (goto-char (point-max))
        (let ((smear-cursor-mode t))
          (smear-cursor--delete-forget)
          (smear-cursor--delete-fire (- (point) 5) (point))
          (smear-cursor--insert-effect)
          (should (= 0 fired)))))))

(ert-deftest smear-cursor-test-an-ordinary-buffer-is-still-marked ()
  ;; GIVEN the same two edits in a buffer that is not the minibuffer
  ;; WHEN they fire
  ;; THEN both are marked.  The guard has to turn away the minibuffer
  ;; without turning away everywhere else.
  (let ((fired 0))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq fired (1+ fired))))
              ((symbol-function 'smear-cursor--play-effect)
               (lambda (&rest _) (setq fired (1+ fired))))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'region-fade))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (&rest _) (vector 0.0 0.0 10.0 20.0)))
              ((symbol-function 'minibufferp) (lambda (&rest _) nil))
              ((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
      (with-temp-buffer
        (insert "some text in an ordinary buffer")
        (goto-char (point-max))
        (let ((smear-cursor-mode t))
          (smear-cursor--delete-forget)
          (smear-cursor--delete-fire (- (point) 5) (point))
          (smear-cursor--insert-effect)
          (should (= 2 fired)))))))

(ert-deftest smear-cursor-test-nothing-is-marked-under-a-child-frame ()
  ;; GIVEN a completion popup showing: corfu, company-box, or anything
  ;;       built on a child frame
  ;; WHEN typing or deleting would be marked
  ;; THEN it is not.
  ;;
  ;; The overlay is an override-redirect window above everything,
  ;; including child frames, and changing its shape makes the server
  ;; expose whatever the shape stopped covering, which Emacs then
  ;; repaints.  Over a popup that is a repaint of the popup, thirteen
  ;; times a keystroke, exactly while the thing is being filtered by
  ;; the keystrokes, with slivers of it left behind wherever a frame
  ;; landed between the two.
  (let ((fired 0))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq fired (1+ fired))))
              ((symbol-function 'smear-cursor--play-effect)
               (lambda (&rest _) (setq fired (1+ fired))))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'region-fade))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (&rest _) (vector 0.0 0.0 10.0 20.0)))
              ((symbol-function 'minibufferp) (lambda (&rest _) nil))
              ((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer)))
              ((symbol-function 'smear-cursor--child-frame-showing-p)
               (lambda () t)))
      (with-temp-buffer
        (insert "completing something")
        (goto-char (point-max))
        (let ((smear-cursor-mode t))
          (smear-cursor--delete-forget)
          (smear-cursor--delete-fire (- (point) 5) (point))
          (smear-cursor--insert-effect)
          (should (= 0 fired)))))))

(ert-deftest smear-cursor-test-a-child-frame-is-noticed-only-while-shown ()
  ;; GIVEN frames with and without a parent, visible and not
  ;; WHEN the question is asked
  ;; THEN only a visible child frame counts.  Corfu keeps its frame
  ;; between uses and hides it, so testing for existence alone would
  ;; turn the effects off for the rest of the session after one
  ;; completion.
  (cl-letf (((symbol-function 'frame-list) (lambda () '(parent child)))
            ((symbol-function 'frame-parent)
             (lambda (f) (and (eq f 'child) 'parent)))
            ((symbol-function 'frame-visible-p) (lambda (_f) nil)))
    (should-not (smear-cursor--child-frame-showing-p)))
  (cl-letf (((symbol-function 'frame-list) (lambda () '(parent child)))
            ((symbol-function 'frame-parent)
             (lambda (f) (and (eq f 'child) 'parent)))
            ((symbol-function 'frame-visible-p) (lambda (f) (eq f 'child))))
    (should (smear-cursor--child-frame-showing-p)))
  ;; and a session with no child frames at all pays one list walk
  (cl-letf (((symbol-function 'frame-list) (lambda () '(parent)))
            ((symbol-function 'frame-parent) (lambda (_f) nil)))
    (should-not (smear-cursor--child-frame-showing-p))))

(ert-deftest smear-cursor-test-a-failing-effect-never-signals-out ()
  ;; GIVEN an effect that signals, from a bug here or a display query
  ;;       answering oddly at the wrong moment
  ;; WHEN it fires from a change hook
  ;; THEN nothing escapes.
  ;;
  ;; `before-change-functions' and `after-change-functions' are shared
  ;; with everything that watches the buffer: eglot pairs a before with
  ;; an after and sends the difference to a language server, undo-hl
  ;; and diff-hl keep their own records.  Emacs removes a hook function
  ;; that signals, so ours would vanish for the session, and anything
  ;; after it in the chain would be skipped for that change.  A
  ;; flourish has no business costing anyone that.
  (let ((smear-cursor--complained nil)
        (messages 0))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (error "no")))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'region-fade))
              ((symbol-function 'smear-cursor--own-edit-p) (lambda (&rest _) t))
              ((symbol-function 'message)
               (lambda (&rest _) (setq messages (1+ messages)))))
      (let ((smear-cursor-mode t))
        (should-not (smear-cursor--delete-fire 10 40))
        ;; and said so, once, rather than silently or on every keystroke
        (should (= 1 messages))
        (smear-cursor--delete-forget)
        (smear-cursor--delete-fire 10 40)
        (should (= 1 messages))))))

(ert-deftest smear-cursor-test-a-failing-insert-effect-never-signals-out ()
  ;; GIVEN the same, on `post-self-insert-hook'
  ;; WHEN it fires
  ;; THEN nothing escapes there either.  `post-self-insert-hook' is how
  ;; electric-pair, aggressive-indent and the rest do their work.
  (let ((smear-cursor--complained nil))
    (cl-letf (((symbol-function 'smear-cursor--play-effect)
               (lambda (&rest _) (error "no")))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'type-blink))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (&rest _) (vector 0.0 0.0 10.0 20.0)))
              ((symbol-function 'smear-cursor--minibuffer-p) (lambda () nil))
              ((symbol-function 'smear-cursor--child-frame-showing-p)
               (lambda () nil))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (insert "abc")
        (let ((smear-cursor-mode t))
          (should-not (smear-cursor--insert-effect)))))))

(ert-deftest smear-cursor-test-a-refused-flight-is-counted ()
  ;; GIVEN a module that refuses a flight: the player wedged, its lock
  ;;       not free within the five milliseconds Emacs will wait
  ;; WHEN the trail is handed over
  ;; THEN it is counted, and `smear-cursor-report' can say so.
  ;;
  ;; Returning t whatever the module says makes a smear that was never
  ;; played look exactly like one that was, which leaves nothing to
  ;; look at when someone reports that it stopped working.
  (let ((smear-cursor--refused 0))
    (cl-letf (((symbol-function 'smear-cursor-x11--play) (lambda (&rest _) nil)))
      (smear-cursor--note-handover nil)
      (smear-cursor--note-handover nil)
      (should (= 2 smear-cursor--refused)))
    (smear-cursor--note-handover t)
    (should (= 2 smear-cursor--refused))))

(ert-deftest smear-cursor-test-the-report-says-when-flights-were-refused ()
  ;; GIVEN flights the module would not take
  ;; WHEN the verdict is drawn
  ;; THEN it says so, ahead of everything else: a flight never played
  ;; makes every other figure a description of some earlier one.
  (should (string-match-p "would not take"
                          (smear-cursor--report-verdict 0.5 16.7 17.0 5 16.7 nil 3)))
  (should (equal "within budget"
                 (smear-cursor--report-verdict 0.9 16.7 20.0 5 16.7 nil 0))))

(ert-deftest smear-cursor-test-a-yank-marks-what-arrived ()
  ;; GIVEN text yanked into a buffer
  ;; WHEN the effect fires
  ;; THEN it marks from the mark to point, which is what `yank' leaves
  ;; around the text it inserted.  The region is not active there and
  ;; would give nothing at all.
  (let (marked)
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (occasion beg end) (setq marked (list occasion beg end))))
              ((symbol-function 'smear-cursor--minibuffer-p) (lambda () nil))
              ((symbol-function 'smear-cursor--child-frame-showing-p)
               (lambda () nil))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'region-arrive)))
      (with-temp-buffer
        (insert "before ")
        (push-mark (point) t)
        (insert "yanked text")
        (let ((smear-cursor-mode t))
          (smear-cursor--yank-effect)
          (should (equal (list 'yank 8 19) marked)))))))

(ert-deftest smear-cursor-test-a-yank-of-nothing-marks-nothing ()
  ;; GIVEN a yank that inserted nothing, or no mark to measure from
  ;; WHEN the effect fires
  ;; THEN nothing is marked.  An empty flash is a flicker with no
  ;; meaning attached.
  (let ((fired 0))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq fired (1+ fired))))
              ((symbol-function 'smear-cursor--minibuffer-p) (lambda () nil))
              ((symbol-function 'smear-cursor--child-frame-showing-p)
               (lambda () nil))
              ((symbol-function 'smear-cursor--effect-for) (lambda (_) 'region-arrive)))
      (with-temp-buffer
        (insert "text")
        (push-mark (point) t)            ; mark and point together
        (let ((smear-cursor-mode t))
          (smear-cursor--yank-effect)
          (should (= 0 fired)))))))

(ert-deftest smear-cursor-test-the-three-occasions-are-told-apart ()
  ;; GIVEN copy, delete and yank
  ;; WHEN their colours are read
  ;; THEN each is different.  At a glance the colour is the whole of
  ;; what says which happened: nothing taken, text gone, text arrived.
  (should (smear-cursor-effect 'region-arrive))
  (let ((smear-cursor-effect-color nil))
    (let ((copy   (smear-cursor--effect-color (smear-cursor-effect 'region-flash)))
          (delete (smear-cursor--effect-color (smear-cursor-effect 'region-fade)))
          (yank   (smear-cursor--effect-color (smear-cursor-effect 'region-arrive))))
      (should-not (equal copy delete))
      (should-not (equal copy yank))
      (should-not (equal delete yank)))))

(ert-deftest smear-cursor-test-a-yank-effect-is-still ()
  ;; GIVEN the yank effect
  ;; WHEN it is asked whether it holds still
  ;; THEN it does, so it is uploaded once like the others.
  (should (smear-cursor-effect 'region-arrive))
  (should (smear-cursor--effect-still-p (smear-cursor-effect 'region-arrive))))

(ert-deftest smear-cursor-test-the-drain-is-reported-with-its-age ()
  ;; GIVEN a drain measured a while ago, since it is sampled every
  ;;       couple of seconds: taking it is a round trip, and on the
  ;;       display it diagnoses a round trip is 52 ms
  ;; WHEN the report prints it
  ;; THEN it says when.  A figure from twenty seconds back, read as
  ;; this flight's, sends the reader hunting a backlog that has been
  ;; and gone.
  (cl-letf (((symbol-function 'smear-cursor-x11--play-stats)
             (lambda (_stage) (vector 12 0.02 0.017 3 0.2 [0.016] 0.328 19.4))))
    (let ((s (smear-cursor--play-stats 'stage)))
      (should (< (abs (- 0.328 (plist-get s :drain))) 1e-9))
      (should (< (abs (- 19.4 (plist-get s :drain-age))) 1e-9))))
  ;; an older module says nothing about the age, and that is not an age
  (cl-letf (((symbol-function 'smear-cursor-x11--play-stats)
             (lambda (_stage) (vector 12 0.02 0.017 3 0.2 [0.016] 0.328))))
    (should-not (plist-get (smear-cursor--play-stats 'stage) :drain-age))))

(ert-deftest smear-cursor-test-an-unsampled-drain-does-not-read-as-nought ()
  ;; GIVEN a flight that ended before the drain had ever been sampled
  ;; WHEN the report prints it
  ;; THEN it says so, rather than "0.0 ms behind".
  ;;
  ;; Nought and never-asked are the same number and opposite news: one
  ;; says the display is keeping up perfectly, the other says nobody
  ;; has looked.  The age tells them apart (negative for never), and
  ;; the report has to use it.
  (cl-letf (((symbol-function 'smear-cursor-x11--play-stats)
             (lambda (_stage) (vector 12 0.02 0.017 3 0.2 [0.016] 0.0 -1.0))))
    (let ((s (smear-cursor--play-stats 'stage)))
      (should (< (plist-get s :drain-age) 0))
      (should (string-match-p
               "not sampled"
               (smear-cursor--drain-line (plist-get s :drain)
                                         (plist-get s :drain-age))))))
  ;; freshly taken, and nought really is nought
  (should (string-match-p "0.0 ms behind" (smear-cursor--drain-line 0.0 0.0)))
  ;; and an old one still says how old
  (should (string-match-p "sampled 19 s ago"
                          (smear-cursor--drain-line 0.328 19.4))))

(ert-deftest smear-cursor-test-report-tells-chronic-from-one-off ()
  ;; GIVEN a flight slow on every frame, and one slow on a single frame
  ;; WHEN the verdict is drawn
  ;; THEN they read differently.  They average the same, and only one
  ;; of them is a cadence problem.
  (should (string-match-p
           "busy elsewhere"
           (smear-cursor--report-verdict 0.9 34.0 40.0 5 16.7)))
  (should (string-match-p
           "mid-flight"
           (smear-cursor--report-verdict 0.9 15.5 235.0 8 16.7))))


;;;; Flights: the whole animation worked out in advance

;; The player thread paints frames without Emacs, so Lisp has to hand
;; it every frame at once.  These check that what it hands over is the
;; same animation the per-frame path drew, and that a retarget can pick
;; the spring up from wherever the thread has actually got to.

(defun smear-cursor-tests--flight-anim ()
  "An animation set up to fly from one rect to another."
  (let ((anim (smear-cursor--anim-create
               :corners (make-vector 8 0.0)
               :target (vector 400.0 300.0 10.0 21.0)
               :cw 10 :lh 21 :ch 21
               :color [200 200 200]
               :born (float-time))))
    (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                     (vector 40.0 60.0 10.0 21.0))
    anim))

(ert-deftest smear-cursor-test-flight-is-bounded-by-max-duration ()
  ;; GIVEN the frame budget a flight is allowed
  ;; WHEN it is asked for
  ;; THEN it is `smear-cursor-max-duration' worth of frames and no more,
  ;;      and never more than the module can hold.  A flight longer
  ;;      than its array is silently cut off at the far end, which is
  ;;      the half the viewer actually watches.
  (let ((smear-cursor-max-duration 0.45)
        (smear-cursor-fps 60))
    (should (= 27 (smear-cursor--flight-limit))))
  (let ((smear-cursor-max-duration 100.0)
        (smear-cursor-fps 60))
    (should (<= (smear-cursor--flight-limit) smear-cursor--flight-max))))

(ert-deftest smear-cursor-test-flight-steps-match-the-per-frame-path ()
  ;; GIVEN the same spring stepped twice: once frame by frame on a
  ;;       timer, and once all at once as a flight
  ;; WHEN the corners are compared at each frame
  ;; THEN they agree.  The flight is not a second animation that merely
  ;;      resembles the first.  If it drifted, the trail would change
  ;;      shape the moment the thread took over.
  (let* ((a (smear-cursor-tests--flight-anim))
         (b (smear-cursor-tests--flight-anim))
         (springs (nth 1 (smear-cursor--flight-springs b 8))))
    (dotimes (k 8)
      (smear-cursor--flight-step a)
      (should (equal (smear-cursor--anim-corners a) (car (aref springs k)))))))

(ert-deftest smear-cursor-test-flight-stops-when-the-spring-settles ()
  ;; GIVEN a move of one cell, which the spring finishes quickly
  ;; WHEN the flight is worked out
  ;; THEN it is shorter than the limit.  Handing over a full budget of
  ;;      frames for a one-cell move would keep the overlay up long
  ;;      after the trail had stopped moving.
  (let* ((anim (smear-cursor--anim-create
                :corners (make-vector 8 0.0)
                :target (vector 50.0 60.0 10.0 21.0)
                :cw 10 :lh 21 :ch 21 :color [200 200 200]
                :born (float-time))))
    (smear-cursor--corners-from-rect (smear-cursor--anim-corners anim)
                                     (vector 40.0 60.0 10.0 21.0))
    (let ((n (nth 0 (smear-cursor--flight-springs anim 60))))
      (should (> n 0))
      (should (< n 60)))))

(ert-deftest smear-cursor-test-retarget-reads-history-out-of-the-flight ()
  ;; GIVEN a flight the thread has played eight frames of
  ;; WHEN the echo history at that frame is reconstructed
  ;; THEN it is the shapes of the frames just before it, newest first.
  ;;
  ;;      Echoes are drawn at where the trail *was*.  Rebuilding them
  ;;      from the flight costs nothing, since frame k-1 is the shape
  ;;      one frame ago by construction.  Keeping a separate history
  ;;      per frame would store the same data twice.
  (let* ((anim (smear-cursor-tests--flight-anim))
         (springs (nth 1 (smear-cursor--flight-springs anim 20)))
         (h (smear-cursor--springs-history springs 8)))
    (should (equal (car h) (car (aref springs 7))))
    (should (equal (nth 1 h) (car (aref springs 6))))
    (should (<= (length h) smear-cursor--history-max))))

(ert-deftest smear-cursor-test-flight-frames-carry-box-and-every-layer ()
  ;; GIVEN a style of three layers
  ;; WHEN a flight is laid out for the module
  ;; THEN each frame is the box followed by every layer's eight corners,
  ;;      two head coordinates and its alpha for that frame, in one flat
  ;;      vector.  The module reads it by stride, so a wrong one
  ;;      silently shifts every frame after the first.
  (should (= 37 (smear-cursor--flight-stride 3)))
  (should (= 15 (smear-cursor--flight-stride 1))))

(ert-deftest smear-cursor-test-flight-keeps-the-history-it-was-given ()
  ;; GIVEN a retarget, which restores the echo history from where the
  ;;       player thread had actually got to
  ;; WHEN the replacement flight is laid out
  ;; THEN that history is still there.  Clearing it would restart the
  ;;      echoes from nothing on every retarget, and on a scroll, where
  ;;      every movement retargets, the echo layers would never have
  ;;      any history to draw at all.
  (let* ((anim (smear-cursor-tests--flight-anim))
         (springs (nth 1 (smear-cursor--flight-springs anim 12)))
         (history (smear-cursor--springs-history springs 6)))
    (setf (smear-cursor--anim-history anim) history)
    (cl-letf (((symbol-function 'smear-cursor-x11--frame-offset)
               (lambda (_win) (cons 0 0)))
              ((symbol-function 'smear-cursor-x11--corners-offset)
               (lambda (corners _dx _dy) corners)))
      (smear-cursor--flight-frames
       anim nil '((:shape quad :alpha 0.7)) springs 4 (vector 0.0 0.0)))
    ;; The four frames just laid out are on the front of it; what has
    ;; to still be there is what was behind them.  Asserting only that
    ;; the history is non-empty proves nothing, since laying out frames
    ;; refills it either way.
    (should (equal (car (last (smear-cursor--anim-history anim)))
                   (car (last history))))))

(ert-deftest smear-cursor-test-flight-does-not-alias-its-own-springs ()
  ;; GIVEN a flight laid out for the module
  ;; WHEN the spring is stepped again afterwards, as a retarget does
  ;; THEN the recorded frames do not move.  Pointing the animation's
  ;;      corners at a stored frame rather than copying them would let
  ;;      the next flight rewrite the one a retarget still has to read.
  (let* ((anim (smear-cursor-tests--flight-anim))
         (springs (nth 1 (smear-cursor--flight-springs anim 10))))
    (cl-letf (((symbol-function 'smear-cursor-x11--frame-offset)
               (lambda (_win) (cons 0 0)))
              ((symbol-function 'smear-cursor-x11--corners-offset)
               (lambda (corners _dx _dy) corners)))
      (smear-cursor--flight-frames
       anim nil '((:shape quad :alpha 0.7)) springs 6 (vector 0.0 0.0)))
    (let ((before (copy-sequence (car (aref springs 5)))))
      (smear-cursor--flight-step anim)
      (should (equal before (car (aref springs 5)))))))


;;;; Effects: flights that mark an occasion rather than a movement

(ert-deftest smear-cursor-test-envelope-rises-and-falls ()
  ;; GIVEN an envelope that comes up fast and goes down slow
  ;; WHEN it is read at points along the effect's life
  ;; THEN it interpolates between the stops, and starts and ends at
  ;;      nothing.  An effect that begins at full alpha appears as a
  ;;      block rather than a pulse.
  (let ((env '((0.0 . 0.0) (0.2 . 1.0) (1.0 . 0.0))))
    (should (= 0.0 (smear-cursor--envelope-at env 0.0)))
    (should (= 1.0 (smear-cursor--envelope-at env 0.2)))
    (should (= 0.0 (smear-cursor--envelope-at env 1.0)))
    (should (< 0.4 (smear-cursor--envelope-at env 0.1) 0.6))
    (should (< 0.4 (smear-cursor--envelope-at env 0.6) 0.6))))

(ert-deftest smear-cursor-test-envelope-clamps-outside-its-range ()
  ;; GIVEN a normalised time outside 0..1, which rounding can produce
  ;; WHEN the envelope is read
  ;; THEN it holds at the ends rather than extrapolating to a negative
  ;;      alpha, which the renderer would take as a hole.
  (let ((env '((0.0 . 0.2) (1.0 . 0.9))))
    (should (= 0.2 (smear-cursor--envelope-at env -0.5)))
    (should (= 0.9 (smear-cursor--envelope-at env 1.5)))))

(ert-deftest smear-cursor-test-one-line-region-is-one-rect ()
  ;; GIVEN a region inside a single screen line
  ;; WHEN it is broken into rectangles
  ;; THEN there is one, spanning the two ends
  (let ((rects (smear-cursor--rects-between
                (vector 100.0 40.0 10.0 20.0)     ; beg
                (vector 260.0 40.0 10.0 20.0)     ; end, same row
                0.0 800.0)))
    (should (= 1 (length rects)))
    (should (equal (nth 0 rects) (vector 100.0 40.0 160.0 20.0)))))

(ert-deftest smear-cursor-test-multi-line-region-is-three-rects ()
  ;; GIVEN a region running from the middle of one line to the middle
  ;;       of a line two below
  ;; WHEN it is broken into rectangles
  ;; THEN it is the tail of the first line, the block between, and the
  ;;      head of the last, which is the shape a selection has.  One
  ;;      bounding box instead would light up text on the left of the
  ;;      first line and the right of the last that is not in it.
  (let ((rects (smear-cursor--rects-between
                (vector 100.0 40.0 10.0 20.0)
                (vector 260.0 80.0 10.0 20.0)
                0.0 800.0)))
    (should (= 3 (length rects)))
    ;; first line, from the start to the right edge
    (should (equal (nth 0 rects) (vector 100.0 40.0 700.0 20.0)))
    ;; the whole line between
    (should (equal (nth 1 rects) (vector 0.0 60.0 800.0 20.0)))
    ;; last line, from the left edge to the end
    (should (equal (nth 2 rects) (vector 0.0 80.0 260.0 20.0)))))

(ert-deftest smear-cursor-test-two-line-region-has-no-middle ()
  ;; GIVEN a region over two adjacent lines
  ;; WHEN it is broken up
  ;; THEN there are two rects and no empty block between them
  (let ((rects (smear-cursor--rects-between
                (vector 100.0 40.0 10.0 20.0)
                (vector 260.0 60.0 10.0 20.0)
                0.0 800.0)))
    (should (= 2 (length rects)))))

(ert-deftest smear-cursor-test-effect-flight-holds-still-and-fades ()
  ;; GIVEN an effect over one rectangle
  ;; WHEN its flight is laid out
  ;; THEN every frame carries the same corners and a different alpha.
  ;;      That is the whole difference between an effect and a trail:
  ;;      the trail moves and holds its alpha, this holds still and
  ;;      moves its alpha.
  (let* ((effect (smear-cursor-effect 'line-pulse))
         (rects (list (vector 10.0 20.0 100.0 18.0)))
         (flight (smear-cursor--effect-frames effect rects 12))
         (stride (smear-cursor--flight-stride 1)))
    (should (= (* 12 stride) (length flight)))
    ;; corners identical between frame 3 and frame 9
    (dotimes (c 8)
      (should (= (aref flight (+ (* 3 stride) 4 c))
                 (aref flight (+ (* 9 stride) 4 c)))))
    ;; alpha is not
    (should-not (= (aref flight (+ (* 0 stride) 4 10))
                   (aref flight (+ (* 6 stride) 4 10))))))

(ert-deftest smear-cursor-test-effect-layers-cover-every-rect ()
  ;; GIVEN a three-rect region and an effect of one layer
  ;; WHEN the layers are built
  ;; THEN there is a layer per rect: the region is drawn as its own
  ;;      shape, not as one box around it.
  (let* ((effect (smear-cursor-effect 'region-flash))
         (rects (list (vector 0.0 0.0 10.0 10.0)
                      (vector 0.0 10.0 10.0 10.0)
                      (vector 0.0 20.0 10.0 10.0))))
    (should (= 3 (length (smear-cursor--effect-layers effect rects))))))

(ert-deftest smear-cursor-test-effect-layers-are-capped ()
  ;; GIVEN a region so tall it would need more layers than the module
  ;;       can hold
  ;; WHEN the layers are built
  ;; THEN they are capped rather than overflowing.  The module drops
  ;;      what will not fit, so an uncapped region would silently lose
  ;;      its far end.
  (let* ((effect (smear-cursor-effect 'region-flash))
         (rects (cl-loop for i below 40
                         collect (vector 0.0 (* 10.0 i) 10.0 10.0))))
    (should (<= (length (smear-cursor--effect-layers effect rects))
                smear-cursor--effect-max-layers))))

(ert-deftest smear-cursor-test-unknown-effect-is-nothing-not-an-error ()
  ;; GIVEN an occasion pointed at an effect nobody defined
  ;; WHEN it is looked up
  ;; THEN the answer is nil and the caller does nothing.  A trail falls
  ;;      back to `plain' because there must always be a cursor; an
  ;;      effect nobody defined should simply not fire.
  (should-not (smear-cursor-effect 'no-such-effect-at-all))
  (should (smear-cursor-effect 'line-pulse)))

(ert-deftest smear-cursor-test-a-long-jump-gets-a-longer-flight ()
  ;; GIVEN one move of a couple of rows and one across the window
  ;; WHEN each is allowed its frames
  ;; THEN the long one gets more.  A jump off the screen is the case
  ;;      where the trail is the only sign of where the cursor came
  ;;      from, and it is exactly the case the shared cap cut shortest.
  (let ((smear-cursor-fps 60)
        (smear-cursor-max-duration 0.45)
        (smear-cursor-long-jump-duration 0.85)
        (smear-cursor-long-jump-rows 12))
    (let ((short (smear-cursor--anim-create
                  :corners (vector 0.0 100.0 10.0 100.0 10.0 120.0 0.0 120.0)
                  :target (vector 0.0 140.0 10.0 20.0) :lh 20))
          (far (smear-cursor--anim-create
                :corners (vector 0.0 100.0 10.0 100.0 10.0 120.0 0.0 120.0)
                :target (vector 0.0 900.0 10.0 20.0) :lh 20)))
      (should-not (smear-cursor--long-jump-p short))
      (should (smear-cursor--long-jump-p far))
      (should (= 27 (smear-cursor--flight-limit short)))
      (should (= 51 (smear-cursor--flight-limit far))))))

(ert-deftest smear-cursor-test-long-jumps-can-be-turned-off ()
  ;; GIVEN the longer duration set to nil
  ;; WHEN a jump across the window is measured
  ;; THEN it is not a long jump and takes the ordinary cap
  (let ((smear-cursor-long-jump-duration nil))
    (should-not (smear-cursor--long-jump-p
                 (smear-cursor--anim-create
                  :corners (vector 0.0 100.0 10.0 100.0 10.0 120.0 0.0 120.0)
                  :target (vector 0.0 900.0 10.0 20.0) :lh 20)))))


;;; Point's rect and a display that has not caught up

(defmacro smear-cursor-tests--with-rows (fresh box &rest body)
  "Run BODY with point's glyph at FRESH and the cursor's row at BOX.
FRESH is (X Y) as `pos-visible-in-window-p' reports it, computed
afresh; BOX is what `window-line-height' says, which comes from the
last redisplay."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'pos-visible-in-window-p)
              (lambda (&rest _) ,fresh))
             ((symbol-function 'window-line-height)
              (lambda (&rest _) ,box))
             ((symbol-function 'line-pixel-height) (lambda () 21))
             ((symbol-function 'frame-char-width) (lambda (&rest _) 10)))
     ,@body))

(ert-deftest smear-cursor-test-point-rect-ignores-a-stale-row ()
  ;; GIVEN point has moved to a glyph at y=105, and the display has not
  ;;       been updated since the cursor was drawn on the row at y=0
  ;; WHEN point's rect is asked for
  ;; THEN it is on the row point is on, not the one the cursor was
  ;;      last drawn on
  ;;
  ;;      `window-line-height' answers for the cursor of the last
  ;;      redisplay.  `pos-visible-in-window-p' answers for point now.
  ;;      Mixing the two aimed the trail at the row the cursor had
  ;;      left, with the column it had arrived at.
  (smear-cursor-tests--with-rows '(10 105) '(21 0 0 0)
    (should (equal (smear-cursor--point-rect (selected-window))
                   [10.0 105.0 10.0 21.0]))))

(ert-deftest smear-cursor-test-point-rect-keeps-the-row-that-holds-point ()
  ;; GIVEN a proportional row from y=100 of height 30, and point's
  ;;       glyph sitting at y=105 within it
  ;; WHEN point's rect is asked for
  ;; THEN the row's own box is used, so the cell grid keeps its rows
  (smear-cursor-tests--with-rows '(10 105) '(30 5 100 0)
    (should (equal (smear-cursor--point-rect (selected-window))
                   [10.0 100.0 10.0 30.0]))))

;;; Landing: the display can move point's row after the trail is aimed

(defmacro smear-cursor-tests--with-landing (aim now &rest body)
  "Run BODY with a flight aimed at AIM in the selected window and
point measuring as NOW, recording every re-aim in `starts'."
  (declare (indent 2))
  `(let* ((win (selected-window))
          (starts nil)
          (smear-cursor-mode t)
          (smear-cursor--sample-serial 7)
          (smear-cursor--last-rects (make-hash-table :test 'eq))
          (smear-cursor--anim (smear-cursor--anim-create
                               :window win :pads (make-hash-table :test (quote eql)))))
     (puthash win (list (window-buffer win) (window-start win) ,aim)
              smear-cursor--last-rects)
     (cl-letf (((symbol-function 'smear-cursor--point-rect)
                (lambda (_win) ,now))
               ((symbol-function 'smear-cursor--start)
                (lambda (w old new) (push (list w old new) starts)))
               ((symbol-function 'smear-cursor--land-soon) #'ignore))
       ,@body)))

(ert-deftest smear-cursor-test-landing-re-aims-when-point-moved ()
  ;; GIVEN a flight aimed at y=400, and point's row has since moved to
  ;;       y=362 through fontification, a header line growing, a side
  ;;       window appearing, or anything else that lays the text out
  ;;       again
  ;; WHEN the landing is checked
  ;; THEN the flight is retargeted from where it was aimed to where
  ;;      point is, and that is what the next movement starts from
  (smear-cursor-tests--with-landing [10.0 400.0 10.0 19.0] [10.0 362.0 10.0 19.0]
    (smear-cursor--land win 7 nil)
    (should (equal starts (list (list win [10.0 400.0 10.0 19.0]
                                      [10.0 362.0 10.0 19.0]))))
    (should (equal (nth 2 (gethash win smear-cursor--last-rects))
                   [10.0 362.0 10.0 19.0]))))

(ert-deftest smear-cursor-test-landing-re-aims-after-a-window-change-stopped-it ()
  ;; GIVEN a flight aimed at y=400, then a header line appearing or a
  ;;       side window opening, which is a window change: it stops the
  ;;       animation in Lisp while the thread plays the flight it was
  ;;       already handed to its old target
  ;; WHEN the landing is checked
  ;; THEN a fresh flight goes from where the old one is landing to
  ;;      where point now is
  (smear-cursor-tests--with-landing [10.0 400.0 10.0 19.0] [10.0 362.0 10.0 19.0]
    (setq smear-cursor--anim nil)
    (smear-cursor--land win 7 nil)
    (should (equal starts (list (list win [10.0 400.0 10.0 19.0]
                                      [10.0 362.0 10.0 19.0]))))))

(ert-deftest smear-cursor-test-landing-leaves-a-window-showing-another-buffer ()
  ;; GIVEN the window has switched buffers since the aim
  ;; WHEN the landing is checked
  ;; THEN nothing is retargeted: those pixels belong to other text now
  (smear-cursor-tests--with-landing [10.0 400.0 10.0 19.0] [10.0 362.0 10.0 19.0]
    (puthash win (list (get-buffer-create " *elsewhere*") (window-start win)
                       [10.0 400.0 10.0 19.0])
             smear-cursor--last-rects)
    (smear-cursor--land win 7 nil)
    (should (null starts))))

(ert-deftest smear-cursor-test-landing-leaves-a-flight-that-landed-right ()
  ;; GIVEN a flight aimed where point still is
  ;; WHEN the landing is checked
  ;; THEN nothing is retargeted
  (smear-cursor-tests--with-landing [10.0 400.0 10.0 19.0] [10.0 400.0 10.0 19.0]
    (smear-cursor--land win 7 nil)
    (should (null starts))))

(ert-deftest smear-cursor-test-landing-yields-to-a-newer-sample ()
  ;; GIVEN point moved after the aim, but a newer sample has run
  ;;       since: the user pressed another key, and that movement has
  ;;       its own flight and its own landing checks
  ;; WHEN the older landing check fires
  ;; THEN it does nothing
  (smear-cursor-tests--with-landing [10.0 400.0 10.0 19.0] [10.0 362.0 10.0 19.0]
    (smear-cursor--land win 6 nil)
    (should (null starts))))

(ert-deftest smear-cursor-test-sample-schedules-a-landing-check ()
  ;; GIVEN a movement worth a trail
  ;; WHEN it is sampled
  ;; THEN a landing check is scheduled for the sample just taken
  (let* ((win (selected-window))
         (scheduled nil)
         (smear-cursor-mode t)
         (smear-cursor--sample-serial 0)
         (smear-cursor--last-window win)
         (smear-cursor--anim nil)
         (smear-cursor--last-rects (make-hash-table :test 'eq)))
    (puthash win (list (current-buffer) (window-start win)
                       [10.0 0.0 10.0 21.0])
             smear-cursor--last-rects)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor--point-rect)
               (lambda (_win) [10.0 105.0 10.0 21.0]))
              ((symbol-function 'smear-cursor--why-not) (lambda (&rest _) nil))
              ((symbol-function 'smear-cursor--start) #'ignore)
              ((symbol-function 'smear-cursor--land-soon)
               (lambda (w serial delays) (setq scheduled (list w serial delays)))))
      (smear-cursor--sample)
      (should (equal scheduled
                     (list win 1 smear-cursor--landing-checks))))))

;;; A ring around the cursor, and the breath that widens it

(ert-deftest smear-cursor-test-a-ring-edge-keeps-off-the-character ()
  ;; GIVEN a cursor cell and the four edges of a ring around it
  ;; WHEN each edge is worked out
  ;; THEN none of them covers the middle of the cell.  The character
  ;;      just typed is under that middle, and an effect that painted
  ;;      over it would hide the thing it is pointing at.
  (let ((cell (vector 100.0 200.0 10.0 20.0))
        (mx 105.0) (my 210.0))
    (dolist (edge '(top bottom left right))
      (let ((r (smear-cursor--edge-rect cell edge 3.0)))
        (should (> (aref r 2) 0.0))
        (should (> (aref r 3) 0.0))
        (should-not (and (<= (aref r 0) mx (+ (aref r 0) (aref r 2)))
                         (<= (aref r 1) my (+ (aref r 1) (aref r 3)))))))))

(ert-deftest smear-cursor-test-a-ring-widens-away-from-the-cell ()
  ;; GIVEN a cell grown by four pixels on every side
  ;; WHEN the top edge of the ring is worked out
  ;; THEN it sits above the cell and is wider than it, so a breath
  ;;      moves the ring outward and never inward over the text.
  (let* ((cell (vector 100.0 200.0 10.0 20.0))
         (top (smear-cursor--edge-rect (smear-cursor--rect-grown cell 4.0)
                                       'top 2.0)))
    (should (= 96.0 (aref top 0)))
    (should (= 196.0 (aref top 1)))
    (should (= 18.0 (aref top 2)))
    (should (= 2.0 (aref top 3)))))

(ert-deftest smear-cursor-test-a-breathing-layer-moves-its-corners ()
  ;; GIVEN a layer that grows and shrinks over its life
  ;; WHEN its flight is laid out
  ;; THEN the corners differ from frame to frame.  Every other effect
  ;;      holds its corners and moves only its alpha; this is the one
  ;;      kind that has to be laid out afresh per frame.
  (let* ((effect '(:duration 1.0 :shape point
                   :layers ((:shape quad :alpha 0.6 :edge top :thickness 2.0
                             :grow-envelope ((0.0 . 0.0) (0.5 . 8.0) (1.0 . 0.0))
                             :envelope ((0.0 . 1.0) (1.0 . 1.0))))))
         (rects (list (vector 10.0 20.0 100.0 18.0)))
         (stride (smear-cursor--flight-stride 1))
         (flight (smear-cursor--effect-frames effect rects 11)))
    ;; frame 5 is the widest breath and frame 0 the narrowest
    (should (< (aref flight (+ (* 5 stride) 4))
               (aref flight (+ (* 0 stride) 4))))
    ;; and it comes back: the last frame is as narrow as the first
    (should (= (aref flight (+ (* 10 stride) 4))
               (aref flight (+ (* 0 stride) 4))))))

(ert-deftest smear-cursor-test-an-effect-that-breathes-is-not-still ()
  ;; GIVEN an effect whose layers change size over time
  ;; WHEN it is asked whether it holds still
  ;; THEN it says no.  A still flight is drawn once and stamped again
  ;;      at another alpha, which would hold the ring at its first size
  ;;      for the whole animation.
  (should-not (smear-cursor--effect-still-p
               '(:layers ((:shape quad
                           :grow-envelope ((0.0 . 0.0) (1.0 . 6.0))))))))

(ert-deftest smear-cursor-test-the-box-holds-the-widest-breath ()
  ;; GIVEN a layer that grows to eight pixels at its widest
  ;; WHEN the bounding box is worked out
  ;; THEN the box holds that frame.  The box is the only area the
  ;;      module may touch, so one measured from the first frame would
  ;;      clip every frame after it.
  (let* ((layer '(:shape quad :grow-envelope ((0.0 . 0.0) (1.0 . 8.0))))
         (box (smear-cursor--effect-box
               (list (cons layer (vector 100.0 100.0 10.0 20.0))))))
    (should (<= (nth 0 box) 92))
    (should (>= (+ (nth 0 box) (nth 2 box)) 118))))

(ert-deftest smear-cursor-test-the-glow-is-a-ring-of-four-edges ()
  ;; GIVEN the glow effect
  ;; WHEN its layers are read
  ;; THEN there is one per side of the cursor.  Four edges are what
  ;;      leave the middle clear; a single quad would fill it.
  (let ((effect (smear-cursor-effect 'cursor-glow)))
    (should effect)
    (should (equal '(top bottom left right)
                   (mapcar (lambda (l) (plist-get l :edge))
                           (plist-get effect :layers))))
    (should (cl-every (lambda (l) (plist-get l :grow-envelope))
                      (plist-get effect :layers)))))

(ert-deftest smear-cursor-test-the-ring-effects-do-not-claim-to-hold-still ()
  ;; GIVEN the two effects that breathe
  ;; WHEN each is asked
  ;; THEN neither holds still, unlike every other effect that ships.
  (dolist (name '(cursor-glow cursor-breathe))
    (should-not (smear-cursor--effect-still-p (smear-cursor-effect name)))))

(ert-deftest smear-cursor-test-nothing-plays-when-idle-until-asked-for ()
  ;; GIVEN the default settings
  ;; WHEN the idle effect is checked
  ;; THEN there is none.  Something moving on its own is a change to
  ;;      how Emacs looks at rest, so it waits to be asked for.
  (should-not smear-cursor-idle-effect))

(ert-deftest smear-cursor-test-a-still-cursor-breathes ()
  ;; GIVEN breathing turned on and a cursor that has been left alone
  ;; WHEN the idle timer comes round
  ;; THEN the breath is played at the cursor.
  (let ((smear-cursor-idle-effect 'cursor-breathe)
        (smear-cursor-mode t)
        (smear-cursor--idle-next nil)
        played asked)
    (cl-letf (((symbol-function 'current-idle-time) (lambda () '(0 2)))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'queued))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (w p) (setq asked (list w p)) (vector 1.0 2.0 3.0 4.0)))
              ((symbol-function 'smear-cursor--idle-prompting-p) (lambda () nil))
              ((symbol-function 'smear-cursor--play-effect)
               (lambda (_win name _rects _track &optional _f)
                 (setq played name))))
      (smear-cursor--idle-play)
      (should (eq 'cursor-breathe played))
      ;; measured at the window's own point, not at the point of
      ;; whatever buffer the timer happened to run in
      (should (equal (list (selected-window) (window-point (selected-window)))
                     asked)))))

(ert-deftest smear-cursor-test-breathing-stays-out-of-a-prompt-when-told ()
  ;; GIVEN breathing turned on, point in the minibuffer, and the idle
  ;;       effect told to stay out of prompts
  ;; WHEN the idle timer comes round
  ;; THEN nothing is played.
  ;;
  ;;      With `smear-cursor-idle-while-prompting' on, which is the
  ;;      default, it plays on the prompt line itself; see
  ;;      `smear-cursor-test-a-roamer-plays-in-the-prompt'.
  (let ((smear-cursor-idle-effect 'cursor-breathe)
        (smear-cursor-mode t)
        (smear-cursor-idle-while-prompting nil)
        (smear-cursor--idle-next nil)
        played)
    (cl-letf (((symbol-function 'current-idle-time) (lambda () '(0 2)))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'queued))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (_w _p) (vector 1.0 2.0 3.0 4.0)))
              ((symbol-function 'smear-cursor--idle-prompting-p) (lambda () t))
              ((symbol-function 'smear-cursor--play-effect)
               (lambda (&rest _) (setq played t))))
      (smear-cursor--idle-play)
      (should-not played))))

(ert-deftest smear-cursor-test-the-breath-timer-follows-the-setting ()
  ;; GIVEN breathing turned off and then on
  ;; WHEN the timer is set up each time
  ;; THEN it exists only while the setting is on.  An idle timer left
  ;;      behind would go on drawing after the feature was switched off.
  (unwind-protect
      (let ((smear-cursor--idle-next nil))
        (let ((smear-cursor-idle-effect nil))
          (smear-cursor--idle-setup)
          (should-not smear-cursor--idle-timer))
        (let ((smear-cursor-idle-effect 'cursor-breathe))
          (smear-cursor--idle-setup)
          (should smear-cursor--idle-timer)))
    (smear-cursor--idle-stop)))

(ert-deftest smear-cursor-test-an-effect-may-ask-for-fewer-frames ()
  ;; GIVEN an effect that asks for thirty frames a second
  ;; WHEN its rate is worked out with nothing set by hand
  ;; THEN it gets thirty and not the trail's sixty.  The breath repeats
  ;;      for as long as the cursor is left alone, so it is the one
  ;;      effect whose cost is paid while nothing is happening, and a
  ;;      slow fade looks the same at half the frames.
  (let ((smear-cursor-fps 60)
        (smear-cursor-effect-fps nil))
    (should (= 30.0 (smear-cursor--effect-rate '(:fps 30))))
    ;; a rate set by hand still wins over the effect's own
    (let ((smear-cursor-effect-fps 45))
      (should (= 45.0 (smear-cursor--effect-rate '(:fps 30)))))
    ;; and an effect that asks for nothing is unchanged
    (should (= 60.0 (smear-cursor--effect-rate '(:duration 0.2))))))

(ert-deftest smear-cursor-test-a-breath-queues-the-next-one ()
  ;; GIVEN a breath played while Emacs sits idle
  ;; WHEN it finishes
  ;; THEN the next one is queued a delay from now.  Emacs runs a
  ;;      repeating idle timer once per idle period and then waits for
  ;;      the next one, so a timer alone gives a single breath after
  ;;      typing stops and nothing after that.
  (let ((smear-cursor-idle-effect 'cursor-breathe)
        (smear-cursor-mode t)
        (smear-cursor-idle-delay 1.5)
        (smear-cursor--idle-next nil)
        queued)
    (cl-letf (((symbol-function 'current-idle-time) (lambda () '(0 2)))
              ((symbol-function 'smear-cursor--minibuffer-p) (lambda () nil))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (_w _p) (vector 1.0 2.0 3.0 4.0)))
              ((symbol-function 'smear-cursor--play-effect) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (secs repeat fn) (setq queued (list secs repeat fn)) 'timer)))
      (smear-cursor--idle-play)
      ;; a delay after the end of the one just played, so a long
      ;; animation does not run into itself
      (should (equal (list (+ 1.5 smear-cursor-breathe-duration)
                           nil #'smear-cursor--idle-play)
                     queued)))))

(ert-deftest smear-cursor-test-breathing-stops-when-the-idling-does ()
  ;; GIVEN a queued breath that comes due after the user has typed
  ;; WHEN it runs
  ;; THEN nothing is played and nothing is queued.  This is what ends
  ;;      the chain: no hook is needed to stop it, because a breath
  ;;      only follows another while Emacs is still idle.
  (let ((smear-cursor-idle-effect 'cursor-breathe)
        (smear-cursor-mode t)
        (smear-cursor--idle-next nil)
        played queued)
    (cl-letf (((symbol-function 'current-idle-time) (lambda () nil))
              ((symbol-function 'smear-cursor--minibuffer-p) (lambda () nil))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (_w _p) (vector 1.0 2.0 3.0 4.0)))
              ((symbol-function 'smear-cursor--play-effect)
               (lambda (&rest _) (setq played t)))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (setq queued t) 'timer)))
      (smear-cursor--idle-play)
      (should-not played)
      (should-not queued))))

(ert-deftest smear-cursor-test-turning-breathing-off-drops-the-queued-breath ()
  ;; GIVEN a breath queued behind the one just played
  ;; WHEN breathing is stopped
  ;; THEN the queued one is cancelled too.  Cancelling only the idle
  ;;      timer would leave one more breath to arrive after the
  ;;      feature was switched off.
  (let ((cancelled nil))
    (cl-letf (((symbol-function 'cancel-timer)
               (lambda (timer) (push timer cancelled))))
      (let ((smear-cursor--idle-timer 'idle-timer)
            (smear-cursor--idle-next 'next-timer))
        (smear-cursor--idle-stop)
        (should (memq 'idle-timer cancelled))
        (should (memq 'next-timer cancelled))))))

(ert-deftest smear-cursor-test-a-radial-fades-out-at-its-rim ()
  ;; GIVEN a round glow
  ;; WHEN its opacity stops are prepared
  ;; THEN the last of them is nothing at all.  A gradient that ends
  ;;      part way up stops being painted there, and the rim comes out
  ;;      as a hard circle rather than a glow.
  (let* ((layer (smear-cursor--effect-layer '(:shape radial :alpha 0.8)))
         (stops (plist-get layer :stops)))
    (should (= 1.0 (car (car (last stops)))))
    (should (= 0.0 (cdr (car (last stops)))))
    ;; and it is still bright in the middle
    (should (= 0.8 (cdr (car stops))))))

(ert-deftest smear-cursor-test-a-quad-keeps-its-opacity-across-itself ()
  ;; GIVEN a quadrilateral rather than a round glow
  ;; WHEN its stops are prepared
  ;; THEN it is one opacity throughout.  Only the round glows fade,
  ;;      since a quad's edge is where the shape ends.
  (let ((stops (plist-get (smear-cursor--effect-layer
                           '(:shape quad :alpha 0.6))
                          :stops)))
    (should (= 0.6 (cdr (car stops))))
    (should (= 0.6 (cdr (car (last stops)))))))

(ert-deftest smear-cursor-test-the-same-seed-gives-the-same-blob ()
  ;; GIVEN two clusters built from one seed, and one from another
  ;; WHEN they are compared
  ;; THEN the first two match and the third does not.  The seed is a
  ;;      setting, so the same seed has to give the same shape every
  ;;      time it is drawn rather than a new one per keystroke.
  (let ((env '((0.0 . 1.0) (1.0 . 0.0))))
    (should (equal (smear-cursor--radial-cluster 12 0.8 0.5 7 env)
                   (smear-cursor--radial-cluster 12 0.8 0.5 7 env)))
    (should-not (equal (smear-cursor--radial-cluster 12 0.8 0.5 7 env)
                       (smear-cursor--radial-cluster 12 0.8 0.5 8 env)))))

(ert-deftest smear-cursor-test-without-noise-a-blob-is-one-round-glow ()
  ;; GIVEN noise turned down to nothing
  ;; WHEN the cluster is built
  ;; THEN it is a single centred glow, which is what it was before
  ;;      noise existed.
  (let ((layers (smear-cursor--radial-cluster 12 0.8 0.0 1
                                              '((0.0 . 1.0) (1.0 . 0.0)))))
    (should (= 1 (length layers)))
    (should-not (plist-get (car layers) :offset))))

(ert-deftest smear-cursor-test-noise-pushes-blobs-off-centre ()
  ;; GIVEN noise turned up
  ;; WHEN the cluster is built
  ;; THEN there is more than one glow and the extras are off centre,
  ;;      which is what stops the outline reading as a circle.
  (let ((layers (smear-cursor--radial-cluster 20 0.8 0.6 3
                                              '((0.0 . 1.0) (1.0 . 0.0)))))
    (should (> (length layers) 1))
    (should (cl-every (lambda (l) (plist-get l :offset)) (cdr layers)))
    (should (cl-some (lambda (l)
                       (let ((off (plist-get l :offset)))
                         (> (+ (abs (car off)) (abs (cdr off))) 0.5)))
                     (cdr layers)))))

(ert-deftest smear-cursor-test-an-off-centre-blob-is-drawn-off-centre ()
  ;; GIVEN a layer pushed off centre
  ;; WHEN the flight is laid out
  ;; THEN its head carries the offset while the centred one does not.
  ;;      The head is where a round glow is drawn, so an offset that
  ;;      never reached the flight would leave every blob stacked in
  ;;      the same place.
  (let* ((effect '(:duration 0.2 :shape point
                   :layers ((:shape radial :alpha 0.8 :radius 10)
                            (:shape radial :alpha 0.8 :radius 6
                             :offset (5.0 . -3.0)))))
         (rects (list (vector 100.0 200.0 10.0 20.0)))
         (stride (smear-cursor--flight-stride 2))
         (flight (smear-cursor--effect-frames effect rects 4)))
    ;; head x and y sit at 8 and 9 of each layer's eleven numbers
    (should (= (+ 5.0 (aref flight (+ 4 8)))
               (aref flight (+ 4 11 8))))
    (should (= (+ -3.0 (aref flight (+ 4 9)))
               (aref flight (+ 4 11 9))))))

(ert-deftest smear-cursor-test-the-box-holds-a-blob-pushed-off-centre ()
  ;; GIVEN a glow pushed well off centre
  ;; WHEN the bounding box is worked out
  ;; THEN the box reaches it.  The box is all the module may touch, so
  ;;      a blob outside it would be cut off along a straight edge.
  (let* ((layer '(:shape radial :radius 10 :offset (30.0 . 0.0)))
         (box (smear-cursor--effect-box
               (list (cons layer (vector 100.0 100.0 10.0 20.0))))))
    (should (>= (+ (nth 0 box) (nth 2 box)) 140))))

(ert-deftest smear-cursor-test-a-bolt-lands-on-the-cursor ()
  ;; GIVEN a bolt sixty pixels tall in six lengths
  ;; WHEN it is built
  ;; THEN the last length ends level with the cursor and centred on
  ;;      it.  The bolt strikes the character just typed, so however
  ;;      far it wanders on the way down it has to arrive there.
  (let* ((quads (smear-cursor--bolt-quads 60.0 3.0 6 1 0.5))
         (end (car (last quads))))
    (should (= 6 (length quads)))
    ;; corners run clockwise from the top left, so 2 and 3 are the foot
    (should (< (abs (aref end 5)) 1e-9))
    (should (< (abs (aref end 7)) 1e-9))
    (should (< (abs (+ (aref end 4) (aref end 6))) 1e-6))))

(ert-deftest smear-cursor-test-a-bolt-starts-above-the-line ()
  ;; GIVEN the same bolt
  ;; WHEN its first length is read
  ;; THEN it starts the full height above the cursor, which is what
  ;;      makes it come down from somewhere rather than appear on the
  ;;      character.
  (let ((top (car (smear-cursor--bolt-quads 60.0 3.0 6 1 0.5))))
    (should (= -60.0 (aref top 1)))
    (should (= -60.0 (aref top 3)))))

(ert-deftest smear-cursor-test-a-bolt-wanders-on-the-way-down ()
  ;; GIVEN a spread to wander by
  ;; WHEN the bolt is built
  ;; THEN some length of it is well off the straight line down.  A
  ;;      bolt that did not wander would be a bar.
  (let ((quads (smear-cursor--bolt-quads 60.0 3.0 6 1 0.6)))
    (should (cl-some (lambda (q) (> (abs (aref q 0)) 4.0)) quads))))

(ert-deftest smear-cursor-test-a-bolt-with-no-spread-is-straight ()
  ;; GIVEN no spread at all
  ;; WHEN the bolt is built
  ;; THEN every length is centred, so the wandering is all the noise
  ;;      and nothing else.
  (let ((quads (smear-cursor--bolt-quads 60.0 3.0 6 1 0.0)))
    (should (cl-every (lambda (q) (< (abs (+ (aref q 0) (aref q 2))) 1e-6))
                      quads))))

(ert-deftest smear-cursor-test-the-same-seed-strikes-the-same-way ()
  ;; GIVEN one seed and then another
  ;; WHEN bolts are built from them
  ;; THEN the seed decides the shape.  That is what lets the menu show
  ;;      an arrangement and keep it.
  (should (equal (smear-cursor--bolt-quads 60.0 3.0 6 4 0.5)
                 (smear-cursor--bolt-quads 60.0 3.0 6 4 0.5)))
  (should-not (equal (smear-cursor--bolt-quads 60.0 3.0 6 4 0.5)
                     (smear-cursor--bolt-quads 60.0 3.0 6 5 0.5))))

(ert-deftest smear-cursor-test-a-layer-may-carry-its-own-shape ()
  ;; GIVEN a layer holding its own four corners
  ;; WHEN they are worked out over a cell
  ;; THEN they are placed around the middle of it.  A rectangle cannot
  ;;      express a bolt, so a layer that is not a rectangle carries
  ;;      its corners itself.
  (let* ((rect (vector 100.0 200.0 10.0 20.0))     ; middle at 105, 210
         (layer '(:shape quad :quad [0.0 -10.0 2.0 -10.0 2.0 0.0 0.0 0.0]))
         (c (smear-cursor--effect-corners layer rect 0.0)))
    (should (= 105.0 (aref c 0)))
    (should (= 200.0 (aref c 1)))
    (should (= 107.0 (aref c 2)))
    (should (= 210.0 (aref c 5)))))

(ert-deftest smear-cursor-test-the-box-holds-a-bolt-above-the-line ()
  ;; GIVEN a shape reaching sixty pixels above the cell
  ;; WHEN the box is worked out
  ;; THEN it reaches up to it.  The box bounds what the module may
  ;;      touch, so a bolt outside it is cut off in a straight line
  ;;      across the middle.
  (let* ((layer '(:shape quad :quad [0.0 -60.0 2.0 -60.0 2.0 0.0 0.0 0.0]))
         (box (smear-cursor--effect-box
               (list (cons layer (vector 100.0 200.0 10.0 20.0))))))
    (should (<= (nth 1 box) 150))))

(ert-deftest smear-cursor-test-lightning-fits-what-the-module-holds ()
  ;; GIVEN the lightning effect
  ;; WHEN its layers are counted
  ;; THEN they fit the module's limit, and there is both a bolt and a
  ;;      flash where it lands.  The module drops layers past its
  ;;      capacity, which would take the end off the bolt.
  (let* ((effect (smear-cursor-effect 'lightning))
         (layers (plist-get effect :layers)))
    (should (<= (length layers) smear-cursor--effect-max-layers))
    (should (cl-some (lambda (l) (plist-get l :quad)) layers))
    (should (cl-some (lambda (l) (eq 'radial (plist-get l :shape))) layers))))

(ert-deftest smear-cursor-test-every-strike-is-a-new-bolt ()
  ;; GIVEN the effect asked for twice, as two keystrokes would
  ;; WHEN the two are compared
  ;; THEN the bolts differ.  Lightning that struck the same shape
  ;;      every time would read as a picture of a bolt.
  (should-not (equal (plist-get (smear-cursor-effect 'lightning) :layers)
                     (plist-get (smear-cursor-effect 'lightning) :layers))))

(ert-deftest smear-cursor-test-a-bolt-can-reach-the-top-of-the-window ()
  ;; GIVEN the height set to the window rather than a number of lines
  ;; WHEN a bolt is built
  ;; THEN it starts level with the top of the window, wherever the
  ;;      cursor happens to be sitting.  A fixed number of lines
  ;;      reaches off the top when the cursor is high up and stops
  ;;      short when it is low down.
  (let ((smear-cursor-lightning-lines 'window))
    (cl-letf (((symbol-function 'smear-cursor--point-rect)
               (lambda (_w) (vector 0.0 300.0 10.0 20.0))))
      (let* ((effect (smear-cursor--lightning))
             (top (cl-find-if (lambda (l) (plist-get l :quad))
                              (plist-get effect :layers))))
        ;; 300 down the window, plus half the line: the bolt reaches back up
        (should (< (abs (+ 310.0 (aref (plist-get top :quad) 1))) 1e-6))))))

(ert-deftest smear-cursor-test-a-bolt-is-built-for-the-window-it-plays-in ()
  ;; GIVEN an effect about to play in a particular window
  ;; WHEN it is built
  ;; THEN that window is the one it is measured against.  The preview
  ;;      panel plays in a window of its own, and a bolt measured
  ;;      against the selected window would be the wrong length there.
  (let (seen)
    (cl-letf (((symbol-function 'smear-cursor--effect-stage) (lambda (_w) nil))
              ((symbol-function 'smear-cursor-effect)
               (lambda (_name) (setq seen smear-cursor--effect-window) nil)))
      (smear-cursor--play-effect 'the-window 'lightning
                                 (list (vector 0.0 0.0 1.0 1.0)) 1)
      (should (eq 'the-window seen)))))

(ert-deftest smear-cursor-test-a-bolt-still-takes-a-plain-number-of-lines ()
  ;; GIVEN the height left as a number
  ;; WHEN a bolt is built
  ;; THEN it is that many lines tall, as it was before the window
  ;;      height was an option.
  (let ((smear-cursor-lightning-lines 2))
    (let* ((effect (smear-cursor--lightning))
           (top (cl-find-if (lambda (l) (plist-get l :quad))
                            (plist-get effect :layers))))
      (should (< (abs (+ (* 2.0 (frame-char-height))
                         (aref (plist-get top :quad) 1)))
                 1e-6)))))

(ert-deftest smear-cursor-test-a-fixed-bolt-strikes-the-same-way-twice ()
  ;; GIVEN the variation turned off
  ;; WHEN the effect is built twice
  ;; THEN both strikes are the same bolt.  With it on, the shape moves
  ;;      on with every strike, which is what makes rolling the seed
  ;;      show nothing for lightning: turning it off is what lets a
  ;;      seed be chosen and kept.
  (let ((smear-cursor-lightning-vary nil))
    (should (equal (plist-get (smear-cursor--lightning) :layers)
                   (plist-get (smear-cursor--lightning) :layers))))
  (let ((smear-cursor-lightning-vary t))
    (should-not (equal (plist-get (smear-cursor--lightning) :layers)
                       (plist-get (smear-cursor--lightning) :layers)))))

(ert-deftest smear-cursor-test-a-fixed-bolt-follows-the-seed ()
  ;; GIVEN the variation off and two different seeds
  ;; WHEN bolts are built
  ;; THEN the seed decides the shape, so the menu can show one and
  ;;      keep it.
  (let ((smear-cursor-lightning-vary nil))
    (let ((a (let ((smear-cursor-noise-seed 3))
               (plist-get (smear-cursor--lightning) :layers)))
          (b (let ((smear-cursor-noise-seed 4))
               (plist-get (smear-cursor--lightning) :layers))))
      (should-not (equal a b)))))

(ert-deftest smear-cursor-test-a-bolt-can-reach-the-top-of-the-frame ()
  ;; GIVEN the height set to the frame
  ;; WHEN a bolt is built
  ;; THEN it is measured from the top of the frame rather than the
  ;;      window, so with the frame split the bolt comes down across
  ;;      whatever windows are above rather than starting at the edge
  ;;      of this one.
  (let ((smear-cursor-lightning-lines 'frame))
    (cl-letf (((symbol-function 'smear-cursor--point-rect)
               (lambda (_w) (vector 0.0 100.0 10.0 20.0)))
              ((symbol-function 'smear-cursor-x11--frame-xy)
               (lambda (_w _x y) (cons 0 (+ y 400))))   ; window sits 400 down
              ((symbol-function 'smear-cursor--rect-in-frame)
               (lambda (_w r) (vector (aref r 0) (+ 400.0 (aref r 1))
                                      (aref r 2) (aref r 3)))))
      (let* ((effect (smear-cursor--lightning))
             (top (cl-find-if (lambda (l) (plist-get l :quad))
                              (plist-get effect :layers))))
        ;; 400 down to the window, 100 into it, plus half the line
        (should (< (abs (+ 510.0 (aref (plist-get top :quad) 1))) 1e-6))))))

(ert-deftest smear-cursor-test-a-frame-bolt-falls-back-to-the-window ()
  ;; GIVEN the frame asked for while the X module is not loaded
  ;; WHEN a bolt is built
  ;; THEN it is measured against the window instead of failing.  The
  ;;      conversion to frame pixels lives in the module, and an
  ;;      effect is not worth an error when it is missing.
  (let ((smear-cursor-lightning-lines 'frame))
    (cl-letf* ((real (symbol-function 'fboundp))
               ((symbol-function 'smear-cursor--point-rect)
                (lambda (_w) (vector 0.0 100.0 10.0 20.0)))
               ((symbol-function 'fboundp)
                (lambda (sym) (and (not (eq sym 'smear-cursor-x11--frame-xy))
                                   (funcall real sym)))))
      (let* ((effect (smear-cursor--lightning))
             (top (cl-find-if (lambda (l) (plist-get l :quad))
                              (plist-get effect :layers))))
        (should (< (abs (+ 110.0 (aref (plist-get top :quad) 1))) 1e-6))))))

(ert-deftest smear-cursor-test-an-arc-leaves-the-cursor ()
  ;; GIVEN an arc going out to the right
  ;; WHEN it is built
  ;; THEN its first length starts on the cursor and its last ends out
  ;;      at the reach.  A spark that started away from the character
  ;;      would look like something else happening nearby.
  (let* ((quads (smear-cursor--arc-quads 0.0 40.0 2.0 2 1 0.4))
         (base (car quads))
         (tip (car (last quads))))
    (should (= 2 (length quads)))
    ;; the base pair straddles the cursor, so their x's are about zero
    (should (< (abs (aref base 0)) 1.0))
    (should (< (abs (aref base 2)) 1.0))
    ;; and the far end has travelled the reach
    (should (> (aref tip 4) 30.0))))

(ert-deftest smear-cursor-test-an-arc-goes-the-way-it-is-pointed ()
  ;; GIVEN two arcs, one to the right and one straight down
  ;; WHEN each is built
  ;; THEN each ends up along its own angle.  Arcs are spread around
  ;;      the cursor, so the angle has to be what decides where one
  ;;      goes.
  (let ((right (car (last (smear-cursor--arc-quads 0.0 40.0 2.0 2 1 0.0))))
        (down (car (last (smear-cursor--arc-quads (/ float-pi 2) 40.0 2.0 2 1 0.0))))) 
    (should (> (aref right 4) 30.0))
    (should (< (abs (aref right 5)) 5.0))
    (should (> (aref down 5) 30.0))
    (should (< (abs (aref down 4)) 5.0))))

(ert-deftest smear-cursor-test-a-crackle-changes-shape-as-it-plays ()
  ;; GIVEN a layer holding one shape per phase
  ;; WHEN its corners are worked out early and late in the flight
  ;; THEN they differ.  That is the crackle: a spark that held one
  ;;      shape and only faded would read as a drawing of a spark.
  (let* ((layer '(:shape quad
                  :quads [[0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0]
                          [9.0 9.0 10.0 9.0 10.0 10.0 9.0 10.0]]))
         (rect (vector 0.0 0.0 0.0 0.0))
         (early (smear-cursor--effect-corners layer rect 0.0))
         (late (smear-cursor--effect-corners layer rect 0.9)))
    (should-not (equal early late))))

(ert-deftest smear-cursor-test-a-crackling-effect-is-not-still ()
  ;; GIVEN an effect whose layers change shape over time
  ;; WHEN it is asked whether it holds still
  ;; THEN it says no, so it is laid out for each frame rather than
  ;;      drawn once and stamped again at another opacity.
  (should-not (smear-cursor--effect-still-p
               '(:layers ((:shape quad
                           :quads [[0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0]
                                   [2.0 2.0 3.0 2.0 3.0 3.0 2.0 3.0]]))))))

(ert-deftest smear-cursor-test-the-box-holds-every-phase-of-a-crackle ()
  ;; GIVEN a spark that reaches further in its second phase
  ;; WHEN the box is worked out
  ;; THEN it holds the furthest one.  The box is fixed for the whole
  ;;      flight, so one measured from the first phase would cut the
  ;;      later ones off.
  (let* ((layer '(:shape quad
                  :quads [[0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0]
                          [0.0 0.0 40.0 0.0 40.0 1.0 0.0 1.0]]))
         (box (smear-cursor--effect-box
               (list (cons layer (vector 100.0 100.0 10.0 20.0))))))
    (should (>= (+ (nth 0 box) (nth 2 box)) 145))))

(ert-deftest smear-cursor-test-plasma-fits-what-the-module-holds ()
  ;; GIVEN the plasma effect
  ;; WHEN its layers are counted
  ;; THEN they fit the module's limit, there is a core to spark from,
  ;;      and the arcs crackle rather than hold one shape.
  (let* ((effect (smear-cursor-effect 'plasma))
         (layers (plist-get effect :layers)))
    (should (<= (length layers) smear-cursor--effect-max-layers))
    (should (cl-some (lambda (l) (eq 'radial (plist-get l :shape))) layers))
    (should (cl-some (lambda (l) (plist-get l :quads)) layers))
    (should-not (smear-cursor--effect-still-p effect))))

(ert-deftest smear-cursor-test-plasma-sparks-anew-each-time ()
  ;; GIVEN the effect built twice, as two keystrokes would
  ;; WHEN they are compared
  ;; THEN the sparks differ, and holding the shape still is what
  ;;      `smear-cursor-plasma-vary' is for.
  (should-not (equal (plist-get (smear-cursor--plasma) :layers)
                     (plist-get (smear-cursor--plasma) :layers)))
  (let ((smear-cursor-plasma-vary nil))
    (should (equal (plist-get (smear-cursor--plasma) :layers)
                   (plist-get (smear-cursor--plasma) :layers)))))

(ert-deftest smear-cursor-test-an-arc-can-start-away-from-the-cursor ()
  ;; GIVEN an origin above the cursor
  ;; WHEN an arc is built from it
  ;; THEN it starts there rather than on the character.  Flames rise,
  ;;      so each lick starts higher than the one before it.
  (let ((base (car (smear-cursor--arc-quads 0.0 20.0 2.0 2 1 0.0
                                            (cons 0.0 -30.0)))))
    (should (< (abs (+ 30.0 (aref base 1))) 2.5))))

(ert-deftest smear-cursor-test-fire-has-a-hotter-core ()
  ;; GIVEN the fire effect
  ;; WHEN its layers are read
  ;; THEN one of them carries a colour of its own.  A flame is not one
  ;;      colour throughout, and the base is the bright part.
  (let ((layers (plist-get (smear-cursor--fire) :layers)))
    (should (cl-some (lambda (l) (and (eq 'radial (plist-get l :shape))
                                      (plist-get l :color)))
                     layers))))

(ert-deftest smear-cursor-test-fire-fits-what-the-module-holds ()
  ;; GIVEN the fire effect
  ;; WHEN its layers are counted
  ;; THEN they fit the module's limit and the flames crackle rather
  ;;      than hold one shape.
  (let ((effect (smear-cursor-effect 'fire)))
    (should (<= (length (plist-get effect :layers))
                smear-cursor--effect-max-layers))
    (should-not (smear-cursor--effect-still-p effect))))

(ert-deftest smear-cursor-test-fire-burns-differently-each-time ()
  ;; GIVEN the effect built twice
  ;; WHEN they are compared
  ;; THEN the flames differ, unless the variation is turned off.
  (should-not (equal (plist-get (smear-cursor--fire) :layers)
                     (plist-get (smear-cursor--fire) :layers)))
  (let ((smear-cursor-fire-vary nil))
    (should (equal (plist-get (smear-cursor--fire) :layers)
                   (plist-get (smear-cursor--fire) :layers)))))

(ert-deftest smear-cursor-test-the-noise-rearranges-rather-than-slides ()
  ;; GIVEN a run of seeds
  ;; WHEN the same index is taken from each
  ;; THEN the steps between them vary in size and nothing comes round
  ;;      again within the run.  A hash that moved every value by one
  ;;      fixed step would slide the whole arrangement rather than
  ;;      rearrange it, so the menu's seed would look like it did
  ;;      nothing, and a short period would bring the same shape back
  ;;      after a few presses.
  (let* ((vals (mapcar (lambda (s) (smear-cursor--noise s 0))
                       (number-sequence 1 16)))
         (steps (cl-loop for (a b) on vals while b collect (abs (- b a)))))
    (should (> (- (apply #'max steps) (apply #'min steps)) 0.3))
    (should (= 16 (length (delete-dups
                           (mapcar (lambda (v) (round (* 100 v))) vals)))))))

(ert-deftest smear-cursor-test-one-seed-moves-every-index ()
  ;; GIVEN two neighbouring seeds
  ;; WHEN a whole row of indexes is taken from each
  ;; THEN the rows differ all over rather than by one shared step.
  ;;      Every index moving together is what made a new seed look
  ;;      like the same shape nudged sideways.
  (let* ((a (mapcar (lambda (i) (smear-cursor--noise 3 i))
                    (number-sequence 0 7)))
         (b (mapcar (lambda (i) (smear-cursor--noise 4 i))
                    (number-sequence 0 7)))
         (diffs (cl-mapcar #'- b a)))
    (should (> (- (apply #'max diffs) (apply #'min diffs)) 0.5))))

(ert-deftest smear-cursor-test-noise-stays-in-its-range ()
  ;; GIVEN any seed and index
  ;; WHEN the noise is taken
  ;; THEN it is between minus one and one, which is what everything
  ;;      using it scales by.
  (dolist (seed (number-sequence 1 24))
    (dolist (i (number-sequence 0 8))
      (let ((v (smear-cursor--noise seed i)))
        (should (<= -1.0 v))
        (should (<= v 1.0))))))

(ert-deftest smear-cursor-test-more-arcs-are-drawn-straighter ()
  ;; GIVEN a number of arcs asked for
  ;; WHEN the budget is worked out
  ;; THEN a few are drawn in five lengths each, and asking for more
  ;;      spends the layers on arcs instead of on their shape.  The
  ;;      module holds sixteen layers for one effect and the core
  ;;      takes one, so three jagged arcs is what fits; past that the
  ;;      choice is fewer arcs or straighter ones.
  (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 16)))
    (should (equal '(1 . 5) (smear-cursor--arc-budget 1)))
    (should (equal '(3 . 5) (smear-cursor--arc-budget 3)))
    (should (equal '(5 . 3) (smear-cursor--arc-budget 5)))
    (should (equal '(7 . 2) (smear-cursor--arc-budget 7)))
    (should (equal '(15 . 1) (smear-cursor--arc-budget 20)))))

(ert-deftest smear-cursor-test-plenty-of-sparks-still-fit ()
  ;; GIVEN more sparks than the module could hold as bent arcs
  ;; WHEN the effect is built
  ;; THEN every one of them is drawn and the layers still fit.  The
  ;;      module drops what it cannot hold, so sparks over the limit
  ;;      would simply not appear.
  (let ((smear-cursor-plasma-arcs 6))
    (let* ((layers (plist-get (smear-cursor--plasma) :layers))
           (quads (seq-filter (lambda (l) (plist-get l :quads)) layers))
           (budget (smear-cursor--arc-budget 6)))
      (should (<= (length layers) smear-cursor--effect-max-layers))
      ;; all six are drawn, each in as many lengths as there was room for
      (should (= 6 (car budget)))
      (should (= (* (car budget) (cdr budget)) (length quads))))))

(ert-deftest smear-cursor-test-a-layer-may-stand-on-the-bottom-of-the-cell ()
  ;; GIVEN a layer anchored to the bottom rather than the middle
  ;; WHEN its corners are worked out over a cell
  ;; THEN they are placed from the foot of the cell.  Fire stands on
  ;;      the character rather than growing out of the middle of it,
  ;;      which is where a shape measured from the centre starts.
  (let* ((rect (vector 100.0 200.0 10.0 20.0))    ; foot at 220, middle at 210
         (mid '(:shape quad :quad [0.0 0.0 1.0 0.0 1.0 0.0 0.0 0.0]))
         (foot '(:shape quad :anchor bottom
                 :quad [0.0 0.0 1.0 0.0 1.0 0.0 0.0 0.0])))
    (should (= 210.0 (aref (smear-cursor--effect-corners mid rect 0.0) 1)))
    (should (= 220.0 (aref (smear-cursor--effect-corners foot rect 0.0) 1)))))

(ert-deftest smear-cursor-test-the-box-follows-the-anchor ()
  ;; GIVEN a bottom-anchored shape reaching up from the cell's foot
  ;; WHEN the box is worked out
  ;; THEN it is measured from the foot too.  A box measured from the
  ;;      middle would sit half a line above the shape and cut off its
  ;;      base.
  (let* ((rect (vector 100.0 200.0 10.0 20.0))
         (layer '(:shape quad :anchor bottom
                  :quad [0.0 0.0 1.0 0.0 1.0 -10.0 0.0 -10.0]))
         (box (smear-cursor--effect-box (list (cons layer rect)))))
    ;; the shape spans 210 to 220, so the box must hold that
    (should (<= (nth 1 box) 210))
    (should (>= (+ (nth 1 box) (nth 3 box)) 220))))

(ert-deftest smear-cursor-test-a-glow-can-sit-at-the-foot-of-the-cell ()
  ;; GIVEN a round glow anchored to the bottom
  ;; WHEN the flight is laid out
  ;; THEN it is drawn at the foot of the cell rather than its middle.
  ;;      The hot part of a fire is where it meets the character, so a
  ;;      core left in the middle would glow through the glyph while
  ;;      the flames rose from below it.
  (let* ((effect '(:duration 0.2 :shape point
                   :layers ((:shape radial :alpha 0.8 :radius 6)
                            (:shape radial :alpha 0.8 :radius 6
                             :anchor bottom))))
         (rects (list (vector 100.0 200.0 10.0 20.0)))   ; middle 210, foot 220
         (stride (smear-cursor--flight-stride 2))
         (flight (smear-cursor--effect-frames effect rects 3)))
    ;; head y sits at 9 of each layer's eleven numbers
    (should (= 210.0 (aref flight (+ 4 9))))
    (should (= 220.0 (aref flight (+ 4 11 9))))))

(ert-deftest smear-cursor-test-the-fire-core-sits-where-the-flames-do ()
  ;; GIVEN the fire effect
  ;; WHEN its core is read
  ;; THEN it is anchored like the flames, so the bright part is where
  ;;      they leave the character.
  (let ((core (cl-find-if (lambda (l) (eq 'radial (plist-get l :shape)))
                          (plist-get (smear-cursor--fire) :layers))))
    (should (eq 'bottom (plist-get core :anchor)))))

(ert-deftest smear-cursor-test-embers-rise-off-the-character ()
  ;; GIVEN the fire effect
  ;; WHEN an ember's places are read
  ;; THEN each is higher than the one before and all of them are above
  ;;      the foot of the cell.  Fire is drawn as embers going up
  ;;      rather than as tongues, which came out looking like limbs.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21)))
    (let* ((embers (seq-filter (lambda (l) (plist-get l :offsets))
                               (plist-get (smear-cursor--fire) :layers)))
           (offs (append (plist-get (car embers) :offsets) nil)))
      (should embers)
      (should (cl-every (lambda (o) (<= (cdr o) 0.0)) offs))
      (should (< (cdr (car (last offs))) (cdr (car offs)))))))

(ert-deftest smear-cursor-test-embers-stand-on-the-character ()
  ;; GIVEN the fire effect
  ;; WHEN its embers and core are read
  ;; THEN all of them are anchored to the foot of the cell, so the
  ;;      fire sits on the character rather than halfway up it.
  (let ((layers (plist-get (smear-cursor--fire) :layers)))
    (should (cl-every (lambda (l) (eq 'bottom (plist-get l :anchor))) layers))))

(ert-deftest smear-cursor-test-embers-cool-as-they-go-up ()
  ;; GIVEN the fire effect
  ;; WHEN the colours of its embers are read
  ;; THEN more than one colour is used.  A fire is hottest where it
  ;;      meets the character and darker further up, and one flat
  ;;      colour throughout is part of what made it look like a
  ;;      drawing of something else.
  (let* ((layers (plist-get (smear-cursor--fire) :layers))
         (colors (delete-dups (mapcar (lambda (l) (plist-get l :color)) layers))))
    (should (> (length colors) 1))))

(ert-deftest smear-cursor-test-plenty-of-embers-still-fit ()
  ;; GIVEN more embers than the module can hold
  ;; WHEN the fire is built
  ;; THEN the layers fit.  The module drops what it cannot hold, so
  ;;      embers over the limit would simply not be drawn.
  (let ((smear-cursor-fire-embers 20))
    (should (<= (length (plist-get (smear-cursor--fire) :layers))
                smear-cursor--effect-max-layers))))

(ert-deftest smear-cursor-test-an-arc-zigzags-rather-than-curving ()
  ;; GIVEN an arc of several lengths
  ;; WHEN the sides its joints fall on are read
  ;; THEN they alternate.  A random walk of the same size wanders to
  ;;      one side and back, which draws a curve, and a curve with a
  ;;      taper on it reads as a limb rather than a spark.
  (let* ((quads (smear-cursor--arc-quads 0.0 40.0 1.0 5 3 0.6))
         ;; the middle of each joint, across the arc
         (joints (mapcar (lambda (q) (/ (+ (aref q 5) (aref q 7)) 2.0)) quads))
         (signs (mapcar (lambda (y) (if (> y 0) 1 -1)) joints)))
    (should (cl-loop for (a b) on signs while b always (/= a b)))))

(ert-deftest smear-cursor-test-a-spark-is-drawn-in-several-lengths ()
  ;; GIVEN the plasma effect
  ;; WHEN its layers are counted
  ;; THEN each spark has enough lengths to be jagged, and the whole
  ;;      thing still fits the module.
  (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 16)))
    (let* ((effect (smear-cursor-effect 'plasma))
           (layers (plist-get effect :layers))
           (arcs (seq-filter (lambda (l) (plist-get l :quads)) layers)))
      (should (<= (length layers) 16))
      (should (>= (length arcs) 9)))))

(ert-deftest smear-cursor-test-an-effect-is-built-to-what-the-module-holds ()
  ;; GIVEN a module that holds only eight layers, as one loaded before
  ;;       the limit was raised does
  ;; WHEN the effects are built
  ;; THEN they are built to eight and still play.  The module refuses
  ;;      a whole flight that asks for more than it holds, so an effect
  ;;      built to the newer limit against an older module does not
  ;;      come out plainer, it does not appear at all.
  (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 8)))
    (should (equal '(3 . 2) (smear-cursor--arc-budget 3)))
    (dolist (name '(plasma fire lightning))
      (should (<= (length (plist-get (smear-cursor-effect name) :layers)) 8)))))

(ert-deftest smear-cursor-test-the-capacity-comes-from-the-module ()
  ;; GIVEN a module reporting its own capacity
  ;; WHEN the limit is asked for
  ;; THEN it is that, held to what this package would use anyway, and
  ;;      the old limit is assumed when the module is too old to say.
  (cl-letf (((symbol-function 'smear-cursor-x11--max-layers) (lambda () 8)))
    (should (= 8 (smear-cursor--max-layers))))
  (cl-letf (((symbol-function 'smear-cursor-x11--max-layers) (lambda () 64)))
    (should (= smear-cursor--effect-max-layers (smear-cursor--max-layers))))
  (cl-letf* ((real (symbol-function 'fboundp))
             ((symbol-function 'fboundp)
              (lambda (sym) (and (not (eq sym 'smear-cursor-x11--max-layers))
                                 (funcall real sym)))))
    (should (= smear-cursor--effect-max-layers-was (smear-cursor--max-layers)))))

(ert-deftest smear-cursor-test-the-stage-comes-before-the-effect ()
  ;; GIVEN the first effect of a session
  ;; WHEN it plays
  ;; THEN the stage is opened before the effect is built.  Opening the
  ;;      stage is what loads the module, and the module is what says
  ;;      how many layers an effect may have, so building first means
  ;;      building to a guess.
  (let (order)
    (cl-letf (((symbol-function 'smear-cursor--effect-stage)
               (lambda (_w) (push 'stage order) nil))
              ((symbol-function 'smear-cursor-effect)
               (lambda (_n) (push 'effect order) nil)))
      (smear-cursor--play-effect 'win 'plasma (list (vector 0.0 0.0 1.0 1.0)) 1)
      (should (equal '(stage effect) (nreverse order))))))

;;; Pacman

(ert-deftest smear-cursor-test-pacman-turns-as-he-goes ()
  ;; GIVEN room on every side
  ;; WHEN his walk is worked out
  ;; THEN he moves along rows and between them, rather than running in
  ;;      one straight line.  Roaming is the whole point of him; a
  ;;      single run across one row is a wipe.
  (let* ((walk (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 40 7))
         (xs (mapcar #'car walk))
         (ys (mapcar #'cdr walk)))
    (should (= 40 (length walk)))
    (should (> (- (apply #'max xs) (apply #'min xs)) 10.0))
    (should (> (- (apply #'max ys) (apply #'min ys)) 10.0))))

(ert-deftest smear-cursor-test-pacman-stays-in-the-window ()
  ;; GIVEN a cursor with a little room to the right and none above
  ;; WHEN he roams
  ;; THEN he keeps inside what he was given.  Wandering off the window
  ;;      would have him eating nothing for part of the run, and over
  ;;      the mode line for the rest.
  ;; thirty pixels of room to the left, a hundred and fifty to the
  ;; right, no rows above and three below
  (let ((walk (smear-cursor--roam-walk 10.0 21.0 30.0 150.0 0 3 40 5)))
    (should (cl-every (lambda (p) (<= -30.0 (car p) 150.0)) walk))
    (should (cl-every (lambda (p) (<= -1.0 (cdr p) 63.0)) walk))))

(ert-deftest smear-cursor-test-pacman-walks-the-same-way-for-a-seed ()
  ;; GIVEN one seed and then another
  ;; WHEN the walks are compared
  ;; THEN the seed decides the route, so a route can be kept.
  (should (equal (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 20 3)
                 (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 20 3)))
  (should-not (equal (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 20 3)
                     (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 20 4))))

(ert-deftest smear-cursor-test-pacman-eats-a-band-on-every-row-he-visits ()
  ;; GIVEN a walk crossing more than one row
  ;; WHEN the eaten bands are worked out
  ;; THEN there is one for each row, and each grows as he covers it.
  ;;      Text is laid out in rows, so what he has eaten is a band per
  ;;      row rather than one rectangle around the whole route.
  (let* ((walk (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 40 7))
         (bands (smear-cursor--eaten-bands walk 21.0 4 9.0))
         (width (lambda (q) (abs (- (aref q 2) (aref q 0))))))
    (should (> (length bands) 1))
    (dolist (band bands)
      (let ((quads (append (plist-get band :quads) nil)))
        (should (equal (smear-cursor--background-color) (plist-get band :color)))
        (should (<= (funcall width (car quads))
                    (funcall width (car (last quads)))))))))

(ert-deftest smear-cursor-test-pacman-turns-round-when-he-doubles-back ()
  ;; GIVEN a route that goes right and then left
  ;; WHEN the drawing is read at each end
  ;; THEN it is mirrored between them.
  ;;
  ;;      One drawing faces both ways.  He faces along the row only:
  ;;      the arcade turns him up and down as well, which a mirror
  ;;      cannot do and which is not worth a second set of frames.
  (let ((walk '((0.0 . 0.0) (10.0 . 0.0) (20.0 . 0.0) (10.0 . 0.0) (0.0 . 0.0))))
    (should (smear-cursor--sprite-flip-p walk 2))
    (should-not (smear-cursor--sprite-flip-p walk 4))))

(ert-deftest smear-cursor-test-pacman-eats-nothing-in-the-buffer ()
  ;; GIVEN the effect
  ;; WHEN it is built
  ;; THEN it is layers and nothing else: no command, no text, no
  ;;      change to the buffer.  The eating is drawn over the window,
  ;;      and a decoration that edited the buffer to look good would be
  ;;      a very bad joke.
  (let ((effect (smear-cursor-effect 'pacman)))
    (should (plist-get effect :layers))
    (should (numberp (plist-get effect :duration)))
    (should (eq 'point (plist-get effect :shape)))))

(defun smear-cursor-test--figure-at (layers which)
  "Return the middle of the figure LAYERS draw, at their first or last frame.

WHICH is `first\=' or `last\='.  Averaged over every block of the drawing,
so it says where the figure is however it is facing: one block\='s
corner is on the other side of him once he turns round."
  (let ((xs nil) (ys nil))
    (dolist (layer layers)
      (when (eq 'sprite (plist-get layer :part))
        (let* ((quads (append (plist-get layer :quads) nil))
               (q (if (eq which 'last) (car (last quads)) (car quads))))
          (when (and q (cl-some (lambda (v) (/= v 0.0)) (append q nil)))
            (dotimes (i 4)
              (push (aref q (* 2 i)) xs)
              (push (aref q (1+ (* 2 i))) ys))))))
    ;; The corner of the box round the whole drawing.  An average over
    ;; the blocks moves as the mouth opens and shuts, because the
    ;; blocks it is made of come and go; the outline is a circle and
    ;; stays where it is.
    (cons (apply #'min xs) (apply #'min ys))))

(defun smear-cursor-test--sprite-quads (layers)
  "Return the quads of the first sprite layer among LAYERS.

The first is the block the scan finds first, at the top-left of the
drawing, and it is in every frame at the same cell.  So its corner
tracks where the figure is, whatever the animation is doing."
  (let ((part (cl-find-if (lambda (l) (eq 'sprite (plist-get l :part)))
                          layers)))
    (append (plist-get part :quads) nil)))

(ert-deftest smear-cursor-test-pacman-is-drawn-in-blocks ()
  ;; GIVEN Pacman
  ;; WHEN his layers are read
  ;; THEN they are hard-edged quads, with no blur and no soft rim.
  ;;
  ;;      He was a disc with a gradient, which is what made him a blob
  ;;      that covered the text rather than a shape standing on it.
  (let ((parts (seq-filter (lambda (l) (eq 'sprite (plist-get l :part)))
                           (plist-get (smear-cursor--pacman) :layers))))
    (should parts)
    (dolist (l parts)
      (should (eq 'quad (plist-get l :shape)))
      (should-not (plist-get l :blur))
      (should-not (plist-get l :radius)))))

(ert-deftest smear-cursor-test-pacman-fits-what-the-module-holds ()
  ;; GIVEN more pellets and rows than there is room for
  ;; WHEN the effect is built
  ;; THEN the layers fit.  Body, mouth, the bands he has eaten and the
  ;;      pellets all come out of one budget.
  (let ((smear-cursor-pacman-pellets 40))
    (should (<= (length (plist-get (smear-cursor--pacman) :layers))
                (smear-cursor--max-layers)))))

(ert-deftest smear-cursor-test-every-row-crossed-is-eaten-somewhere ()
  ;; GIVEN a route across three rows
  ;; WHEN what it takes is worked out
  ;; THEN every row it crossed has a stretch taken from it.
  ;;
  ;;      A row short of one is a row he crosses without eating, which
  ;;      reads as him passing over the text rather than through it.
  (smear-cursor--roam-reset)
  (let* ((walk (cl-loop for row in '(0.0 21.0 42.0)
                        append (cl-loop for i below 6
                                        collect (cons (* i 8.0) row))))
         (spans (smear-cursor--eaten-fold nil walk 21.0 9.0 8)))
    (should (equal '(0 1 2) (sort (delete-dups (mapcar #'car spans)) #'<)))))

(ert-deftest smear-cursor-test-pacman-eats-within-his-budget ()
  ;; GIVEN room for him and the rows he crosses
  ;; WHEN the effect is built
  ;; THEN he eats, and the whole thing fits what the module holds.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 200.0 200.0 10.0 21.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 600))
            ((symbol-function 'smear-cursor--max-layers) (lambda () 16)))
    (smear-cursor--roam-reset)
    (let* ((smear-cursor-pacman-eat-text t)
           (layers (plist-get (smear-cursor--pacman) :layers))
           (bands (seq-filter (lambda (l) (eq 'eaten (plist-get l :part)))
                              layers)))
      (should bands)
      (should (<= (length layers) (smear-cursor--max-layers))))))

(ert-deftest smear-cursor-test-a-ghost-follows-where-pacman-has-been ()
  ;; GIVEN a walk and a ghost trailing six places behind
  ;; WHEN the ghost's places are read
  ;; THEN each is where Pacman was six places ago.  Chasing is what
  ;;      makes them ghosts rather than decorations, and following his
  ;;      own route is what keeps them on the text he has eaten.
  (let* ((walk (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 30 7))
         (trail (smear-cursor--roam-trail walk 6)))
    (should (= (length walk) (length trail)))
    (should (equal (nth 10 walk) (nth 16 trail)))   ; six places back
    ;; before he has gone six places they wait where he started
    (should (equal (car walk) (nth 3 trail)))))

(ert-deftest smear-cursor-test-a-ghost-is-a-dome-on-a-body ()
  ;; GIVEN one ghost
  ;; WHEN its layers are read
  ;; THEN its own two parts are a round top and a body under it, both
  ;;      in its colour.  Its eyes are separate, and white.
  (let* ((layers (smear-cursor--pacman-ghost 0 "#ff4d4d"
                                             (smear-cursor--roam-walk
                                              10.0 21.0 200.0 200.0 2 2 20 3)
                                             9.0))
         (own (seq-remove (lambda (l) (eq 'ghost-eye (plist-get l :part))) layers)))
    ;; a dome, a body, and the eyes that go on top of them
    (should (= 2 (length own)))
    (should (cl-every (lambda (l) (equal "#ff4d4d" (plist-get l :color))) own))
    (should (cl-some (lambda (l) (eq 'radial (plist-get l :shape))) own))
    (should (cl-some (lambda (l) (plist-get l :quads)) own))))

(ert-deftest smear-cursor-test-ghosts-come-out-after-he-sets-off ()
  ;; GIVEN a ghost, with nothing behind him yet and then with a tail
  ;; WHEN its envelope is read
  ;; THEN it comes out of nothing the first time and is full after
  ;;      that.
  ;;
  ;;      With no tail the trail they chase along falls back to where
  ;;      he is standing, so arriving at full strength would have them
  ;;      sitting on top of him before anyone has moved.  Once there is
  ;;      a tail they are already behind him: fading them in again at
  ;;      every play, and the plays run back to back, is a chase that
  ;;      fades away every few seconds.
  (let ((walk (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 20 3)))
    (let* ((smear-cursor--roam-tail nil)
           (env (plist-get (car (smear-cursor--pacman-ghost
                                 0 "#ff4d4d" walk 9.0))
                           :envelope)))
      (should (= 0.0 (cdr (car env))))
      (should (> (car (nth 1 env)) 0.1)))
    (let* ((smear-cursor--roam-tail (last walk 4))
           (env (plist-get (car (smear-cursor--pacman-ghost
                                 0 "#ff4d4d" walk 9.0))
                           :envelope)))
      (dolist (stop env) (should (= 1.0 (cdr stop)))))))

(ert-deftest smear-cursor-test-ghosts-take-their-turn-in-the-budget ()
  ;; GIVEN more ghosts than there is room for
  ;; WHEN the effect is built
  ;; THEN the layers still fit, and the pellets give way to them: the
  ;;      ghosts are the point and the pellets are the trimming.
  (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 16)))
    (smear-cursor--roam-reset)          ; not another test's leftovers
    (let ((smear-cursor-pacman-ghosts 8)
          (smear-cursor-pacman-pellets 8))
      (let ((layers (plist-get (smear-cursor--pacman) :layers)))
        (should (<= (length layers) 16))
        (should (cl-some (lambda (l) (eq 'ghost (plist-get l :part))) layers))))))

(ert-deftest smear-cursor-test-pacman-can-run-alone ()
  ;; GIVEN no ghosts asked for
  ;; WHEN the effect is built
  ;; THEN there are none, and he has the text to himself.
  (let ((smear-cursor-pacman-ghosts 0))
    (should-not (cl-some (lambda (l) (eq 'ghost (plist-get l :part)))
                         (plist-get (smear-cursor--pacman) :layers)))))

;;; The janitor

(ert-deftest smear-cursor-test-the-janitor-shines-only-when-he-leaves-it ()
  ;; GIVEN the janitor with the cleaning off, and again with it on
  ;; WHEN the streak behind him is looked for
  ;; THEN it is there only when the text stays.
  ;;
  ;;      The shine says a stretch has been cleaned without changing
  ;;      it.  Once the stretch is actually gone it is saying nothing,
  ;;      and the layer buys a puff of dust instead.
  (smear-cursor-test--on-a-roomy-frame
   (let ((wipe-p (lambda () (cl-some (lambda (l) (eq 'wipe (plist-get l :part)))
                                     (plist-get (smear-cursor--janitor) :layers)))))
     (let ((smear-cursor-janitor-clean-text nil))
       (should (funcall wipe-p)))
     (smear-cursor--roam-reset)
     (let ((smear-cursor-janitor-clean-text t))
       (should-not (funcall wipe-p))))))

(ert-deftest smear-cursor-test-the-janitors-cloth-is-see-through ()
  ;; GIVEN the streak he leaves behind when the text stays
  ;; WHEN its opacity is read
  ;; THEN it is faint.  A wipe is a shine over the characters, so they
  ;;      have to stay legible under it.
  (let* ((smear-cursor-janitor-clean-text nil)
         (streak (cl-find-if (lambda (l) (eq 'wipe (plist-get l :part)))
                             (plist-get (smear-cursor--janitor) :layers))))
    (should streak)
    (should (< (plist-get streak :alpha) 0.5))))

(ert-deftest smear-cursor-test-the-janitor-roams ()
  ;; GIVEN the janitor
  ;; WHEN the places he takes are read
  ;; THEN he moves along rows and between them, as Pacman does.  The
  ;;      two of them share the roaming and differ in what they do
  ;;      along the way.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 200.0 200.0 10.0 21.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 600)))
    (let* ((part (cl-find-if (lambda (l) (eq 'sprite (plist-get l :part)))
                             (plist-get (smear-cursor--janitor) :layers)))
           (quads (append (plist-get part :quads) nil))
           (xs (mapcar (lambda (q) (aref q 0)) quads))
           (ys (mapcar (lambda (q) (aref q 1)) quads)))
      (should part)
      (should (> (- (apply #'max xs) (apply #'min xs)) 10.0))
      (should (> (- (apply #'max ys) (apply #'min ys)) 10.0)))))

(ert-deftest smear-cursor-test-the-janitors-dust-goes-up ()
  ;; GIVEN the dust he kicks up
  ;; WHEN its places are read
  ;; THEN each one is higher than the last.  Dust settling downwards
  ;;      would read as something dripping.
  ;; Given room for it: the figure comes first, and on a module that
  ;; cannot hold both there is no dust to test.
  (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 16)))
    (let* ((dust (cl-find-if (lambda (l) (eq 'dust (plist-get l :part)))
                             (plist-get (smear-cursor--janitor) :layers)))
           (offs (append (plist-get dust :offsets) nil)))
      (should dust)
      (should (< (cdr (car (last offs))) (cdr (car offs)))))))

(ert-deftest smear-cursor-test-the-janitors-mop-sweeps ()
  ;; GIVEN the mop
  ;; WHEN its shapes are compared across the phases
  ;; THEN the foot of it is not always in the same place: it swings as
  ;;      he goes, which is the difference between mopping and
  ;;      carrying a mop.
  (let* ((mop (cl-find-if (lambda (l) (eq 'mop (plist-get l :part)))
                          (plist-get (smear-cursor--janitor) :layers)))
         (quads (append (plist-get mop :quads) nil)))
    (should mop)
    (should (> (length (delete-dups
                        (mapcar (lambda (q) (round (aref q 4))) quads)))
               2))))

(ert-deftest smear-cursor-test-the-janitor-fits-what-the-module-holds ()
  ;; GIVEN more dust than there is room for
  ;; WHEN the effect is built
  ;; THEN the layers fit.
  (let ((smear-cursor-janitor-dust 40))
    (should (<= (length (plist-get (smear-cursor--janitor) :layers))
                (smear-cursor--max-layers)))))

(ert-deftest smear-cursor-test-pacman-can-be-told-to-leave-the-text-alone ()
  ;; GIVEN the eating turned off
  ;; WHEN Pacman is built
  ;; THEN nothing of his covers the text, and he still has a mouth to
  ;;      eat the pellets with.
  (let ((smear-cursor-pacman-eat-text nil))
    (let* ((layers (plist-get (smear-cursor--pacman) :layers))
           (quads (smear-cursor-test--sprite-quads layers)))
      (should-not (cl-some (lambda (l) (eq 'eaten (plist-get l :part))) layers))
      (should quads)
      ;; and he is still chewing: the drawing changes as it plays.
      ;; Read across the whole figure rather than one block of it: on a
      ;; coarse grid, which is what a frame with a one pixel line gives,
      ;; the top block is the same whether the mouth is open or shut.
      (let ((seen nil))
        (dolist (layer layers)
          (when (eq 'sprite (plist-get layer :part))
            (dolist (q (append (plist-get layer :quads) nil))
              (push (aref q 5) seen))))
        (should (> (length (delete-dups seen)) 1))))))

(ert-deftest smear-cursor-test-pacman-can-be-asked-to-eat-the-text ()
  ;; GIVEN the option turned on
  ;; WHEN he is built
  ;; THEN the bands are back.  It is a joke worth keeping for anyone
  ;;      who wants it, just not one to spring on people.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 200.0 200.0 10.0 21.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 600))
            ;; Given room for it: he is served first, and on a module
            ;; holding eight layers there is nothing left to eat with.
            ((symbol-function 'smear-cursor--max-layers) (lambda () 16)))
    (should (cl-some (lambda (l) (eq 'eaten (plist-get l :part)))
                     (plist-get (smear-cursor--pacman) :layers)))))

(ert-deftest smear-cursor-test-a-roaming-effect-boxes-each-frame-tightly ()
  ;; GIVEN an effect whose shapes travel a long way over its life
  ;; WHEN its flight is laid out
  ;; THEN each frame carries a box around what that frame draws rather
  ;;      than one around the whole route, and the boxes follow the
  ;;      shapes.  The module lays the window back down over the box
  ;;      and composites it for every frame, so a box around a long
  ;;      route costs the whole route thirty times a second, which
  ;;      over a forwarded display is the difference between an
  ;;      animation and a slide show.
  (let* ((effect '(:duration 1.0 :shape point
                   :layers ((:shape radial :alpha 1.0 :radius 8
                             :offsets [(0.0 . 0.0) (100.0 . 0.0)
                                       (200.0 . 0.0) (300.0 . 0.0)]))))
         (rects (list (vector 100.0 100.0 10.0 20.0)))
         (stride (smear-cursor--flight-stride 1))
         (flight (smear-cursor--effect-frames effect rects 4))
         (box (lambda (k) (mapcar (lambda (i) (aref flight (+ (* k stride) i)))
                                  '(0 1 2 3)))))
    ;; the box moves with him
    (should (< (nth 0 (funcall box 0)) (nth 0 (funcall box 3))))
    ;; and each one is a fraction of the whole route
    (should (< (nth 2 (funcall box 0)) 100))
    (should (< (nth 2 (funcall box 3)) 100))))

(ert-deftest smear-cursor-test-a-still-effect-keeps-one-box ()
  ;; GIVEN an effect that holds its shape
  ;; WHEN its flight is laid out
  ;; THEN every frame carries the same box.  Nothing moves, so a box
  ;;      per frame would be the same box worked out again and again.
  (let* ((effect (smear-cursor-effect 'region-flash))
         (rects (list (vector 0.0 0.0 200.0 20.0)))
         (stride (smear-cursor--flight-stride 1))
         (flight (smear-cursor--effect-frames effect rects 6)))
    (dotimes (i 4)
      (should (= (aref flight i) (aref flight (+ (* 3 stride) i)))))))

(ert-deftest smear-cursor-test-typing-stops-the-idle-effect ()
  ;; GIVEN an idle effect part way through playing
  ;; WHEN a command runs
  ;; THEN its track is stopped and the next play is dropped.  Pacman
  ;;      covers the text he crosses, so one still going while
  ;;      somebody types is worse than none at all.
  (let ((smear-cursor--idle-stage 'stage)
        (smear-cursor--idle-next nil)
        (stopped nil))
    (cl-letf (((symbol-function 'smear-cursor-x11--play-stop-track)
               (lambda (stage track) (setq stopped (cons stage track)))))
      (smear-cursor--idle-interrupt)
      (should (equal (cons 'stage smear-cursor--track-idle) stopped))
      (should-not smear-cursor--idle-stage))))

(ert-deftest smear-cursor-test-nothing-is-stopped-when-nothing-plays ()
  ;; GIVEN no idle effect playing
  ;; WHEN commands run, as they do all day
  ;; THEN the module is left alone.  This runs after every command, so
  ;;      it has to cost nothing when there is nothing to stop.
  (let ((smear-cursor--idle-stage nil)
        (called nil))
    (cl-letf (((symbol-function 'smear-cursor-x11--play-stop-track)
               (lambda (&rest _) (setq called t))))
      (smear-cursor--idle-interrupt)
      (should-not called))))

(ert-deftest smear-cursor-test-the-idle-effect-has-a-track-to-itself ()
  ;; GIVEN the tracks the player keeps
  ;; WHEN the idle one is compared with the others
  ;; THEN it is its own.  Stopping it at the first keystroke must not
  ;;      cut short a trail, a pulse or a typing effect.
  (should-not (memq smear-cursor--track-idle
                    (list smear-cursor--track-trail
                          smear-cursor--track-occasion
                          smear-cursor--track-typing))))

(ert-deftest smear-cursor-test-a-roaming-effect-plays-on-without-a-gap ()
  ;; GIVEN an effect that says it carries on
  ;; WHEN the wait until the next play is worked out
  ;; THEN it is the length of the one playing, so the next takes over
  ;;      as it ends.  Anything else stops him, waits, and starts him
  ;;      again from the cursor, which is a loop rather than roaming.
  (let ((smear-cursor-idle-delay 1.5)
        (smear-cursor-idle-effect 'pacman))
    (cl-letf (((symbol-function 'smear-cursor-effect)
               (lambda (_n) '(:duration 3.0 :continuous t))))
      (should (< (smear-cursor--idle-gap) 3.0))
      (should (> (smear-cursor--idle-gap) 2.5))))
  ;; and one that does not carry on still waits between plays
  (let ((smear-cursor-idle-delay 1.5)
        (smear-cursor-idle-effect 'cursor-breathe))
    (cl-letf (((symbol-function 'smear-cursor-effect)
               (lambda (_n) '(:duration 1.0))))
      (should (= 2.5 (smear-cursor--idle-gap))))))

(ert-deftest smear-cursor-test-he-picks-up-where-he-left-off ()
  ;; GIVEN a route already walked
  ;; WHEN the next one is built
  ;; THEN it starts from the end of the last rather than at the
  ;;      cursor.  Roaming means going on from where he is; starting
  ;;      over would have him teleport back every few seconds.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 200.0 200.0 10.0 21.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 600)))
    (smear-cursor--roam-reset)
    ;; Where the figure is, not where one of its blocks is: the block
    ;; is mirrored when he turns, and he may turn at the handover.
    (let* ((ended (smear-cursor-test--figure-at
                   (plist-get (smear-cursor--pacman) :layers) 'last))
           (began (smear-cursor-test--figure-at
                   (plist-get (smear-cursor--pacman) :layers) 'first)))
      ;; Within a character.  The two frames are different points of
      ;; the chomp, so the box round the drawing can differ by a cell
      ;; or two; starting over would put him back at the cursor, which
      ;; is the length of a route away.
      (should (< (abs (- (car ended) (car began))) (frame-char-width)))
      (should (< (abs (- (cdr ended) (cdr began))) (frame-char-width)))))
  (smear-cursor--roam-reset))

(ert-deftest smear-cursor-test-what-he-has-eaten-stays-eaten ()
  ;; GIVEN a stretch already eaten
  ;; WHEN the next play is built
  ;; THEN every stretch it had is still covered by one of the new ones.
  ;;
  ;;      Each play covers the text itself, so a play that knew only
  ;;      its own route would hand back everything eaten a moment
  ;;      earlier.
  ;;
  ;;      Compared stretch by stretch rather than by how wide the bands
  ;;      come out: the route is drawn from a counter that other tests
  ;;      have also stepped, so the widths are not the same twice, but
  ;;      what has been taken may never shrink.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 200.0 200.0 10.0 21.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 600))
            ((symbol-function 'smear-cursor--max-layers) (lambda () 32)))
    (smear-cursor--roam-reset)
    (smear-cursor--pacman)
    (let ((before (mapcar (lambda (s) (list (car s) (cadr s) (cddr s)))
                          smear-cursor--eaten-spans)))
      (should before)
      (smear-cursor--pacman)
      (dolist (b before)
        (should (cl-some (lambda (s) (and (= (car s) (nth 0 b))
                                          (<= (cadr s) (nth 1 b))
                                          (>= (cddr s) (nth 2 b))))
                         smear-cursor--eaten-spans))))))
(ert-deftest smear-cursor-test-stopping-him-forgets-the-route ()
  ;; GIVEN a route walked and text eaten
  ;; WHEN he is stopped, as typing stops him
  ;; THEN the next time he starts from the cursor with the text whole
  ;;      again.  Picking up an old route after an interruption would
  ;;      have him carry on from wherever the window used to be.
  (setq smear-cursor--roam-from '(80.0 . 21.0))
  (setq smear-cursor--roam-tail '((70.0 . 0.0) (80.0 . 0.0)))
  (let ((smear-cursor--idle-stage nil))
    (smear-cursor--idle-interrupt))
  (should (equal '(0.0 . 0.0) smear-cursor--roam-from))
  (should-not smear-cursor--roam-tail))

(ert-deftest smear-cursor-test-asking-how-long-to-wait-moves-nothing ()
  ;; GIVEN a roaming effect, whose builder walks the route on
  ;; WHEN the wait until the next play is worked out
  ;; THEN the route is where it was.  Working out a delay must not
  ;;      advance the walk: an effect built and never played would eat
  ;;      its way across the window with nothing drawn.
  (let ((smear-cursor-idle-effect 'pacman)
        (smear-cursor--last-effect (cons 'pacman '(:duration 3.0 :continuous t))))
    (setq smear-cursor--roam-from '(40.0 . 0.0))
    (smear-cursor--idle-gap)
    (should (equal '(40.0 . 0.0) smear-cursor--roam-from))))

(ert-deftest smear-cursor-test-what-draws-on-top-survives-the-cap ()
  ;; GIVEN an effect asking for more layers than the module holds
  ;; WHEN they are cut down to fit
  ;; THEN the last of them are kept.  Layers are drawn in order, so
  ;;      the last are the ones on top: for Pacman that is Pacman
  ;;      himself, drawn over the text he has eaten.  Dropping the
  ;;      tail instead leaves the hole in the text and takes away the
  ;;      character who is supposed to be eating it, which is exactly
  ;;      the shape of a bug that hid him for a whole evening.
  (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 4)))
    (let* ((effect '(:layers ((:part eaten) (:part pellet) (:part pellet)
                              (:part ghost) (:part body) (:part mouth))))
           (kept (mapcar (lambda (pair) (plist-get (car pair) :part))
                         (smear-cursor--effect-layers
                          effect (list (vector 0.0 0.0 10.0 20.0))))))
      (should (= 4 (length kept)))
      (should (memq 'body kept))
      (should (memq 'mouth kept))
      ;; and what it drops is the earliest, which is drawn underneath
      (should-not (memq 'eaten kept)))))

(ert-deftest smear-cursor-test-a-ghost-has-eyes ()
  ;; GIVEN one ghost
  ;; WHEN its layers are read
  ;; THEN it has a pair of eyes, drawn after its body so they sit on
  ;;      it rather than under it.  At a line high there is room for
  ;;      them, and eyes are most of what makes a coloured blob read
  ;;      as a ghost.
  (let* ((walk (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 40 3))
         (layers (smear-cursor--pacman-ghost 0 "#ff4d4d" walk 21.0))
         (eyes (seq-filter (lambda (l) (eq 'ghost-eye (plist-get l :part))) layers)))
    (should (= 4 (length layers)))
    (should (= 2 (length eyes)))
    ;; the eyes come last of the four, so they are drawn on top
    (should (eq 'ghost-eye (plist-get (nth 2 layers) :part)))
    (should (eq 'ghost-eye (plist-get (nth 3 layers) :part)))
    (should (cl-every (lambda (l) (equal "#ffffff" (plist-get l :color))) eyes))))

(ert-deftest smear-cursor-test-a-ghosts-eyes-sit-in-its-face ()
  ;; GIVEN a ghost and its eyes at the same moment
  ;; WHEN their places are compared
  ;; THEN the eyes are a pair, apart from each other and both within
  ;;      the dome.  Eyes wandering off the head would read as two
  ;;      more pellets.
  (let* ((walk (smear-cursor--roam-walk 10.0 21.0 200.0 200.0 2 2 40 3))
         (layers (smear-cursor--pacman-ghost 0 "#ff4d4d" walk 21.0))
         (dome (aref (plist-get (nth 0 layers) :offsets) 30))
         (left (aref (plist-get (nth 2 layers) :offsets) 30))
         (right (aref (plist-get (nth 3 layers) :offsets) 30)))
    (should (> (abs (- (car left) (car right))) 8.0))
    (dolist (eye (list left right))
      (should (< (abs (- (car eye) (car dome))) 21.0))
      (should (< (abs (- (cdr eye) (cdr dome))) 21.0)))))

(ert-deftest smear-cursor-test-ghosts-with-eyes-still-fit ()
  ;; GIVEN ghosts that now take four layers each
  ;; WHEN the effect is built
  ;; THEN it fits what the module holds.
  (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 16)))
    (let ((smear-cursor-pacman-ghosts 8))
      (should (<= (length (plist-get (smear-cursor--pacman) :layers)) 16)))))

(ert-deftest smear-cursor-test-an-idle-spell-keeps-the-overlay-it-has ()
  ;; GIVEN the start of an idle spell
  ;; WHEN the effect is played
  ;; THEN the overlay is not thrown away and remade first.
  ;;
  ;;      It was, on the theory that a long-lived stage and what the
  ;;      compositor presents come apart.  They do not: the animation
  ;;      that went missing was losing its layers in the renderer, and
  ;;      remaking the window costs a flash of the real text at the
  ;;      start of every spell.
  (let ((released 0))
    (cl-letf (((symbol-function 'smear-cursor-x11-release)
               (lambda (&rest _) (cl-incf released)))
              ((symbol-function 'smear-cursor--sample-window)
               (lambda () (selected-window)))
              ((symbol-function 'smear-cursor--minibuffer-p) (lambda () nil))
              ((symbol-function 'current-idle-time) (lambda () 1))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (&rest _) (vector 0.0 0.0 8.0 18.0)))
              ((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage))
              ((symbol-function 'smear-cursor--play-effect) (lambda (&rest _) t))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (let ((smear-cursor-mode t)
            (smear-cursor-idle-effect 'pacman)
            (smear-cursor--idle-next nil))
        (smear-cursor--roam-reset)
        (smear-cursor--idle-play)
        (should (= 0 released))))))

(ert-deftest smear-cursor-test-a-click-smears-though-the-drag-ate-the-sample ()
  ;; GIVEN a cursor at one place and a click that puts it at another
  ;; WHEN a sample lands mid-click, while the mouse is still tracked
  ;; THEN the click still smears once the button comes up.
  ;;
  ;;      `mouse-drag-region' binds `track-mouse' for the whole gesture
  ;;      and moves point inside it, and the sample timer runs during
  ;;      that.  Such a sample is suppressed -- a cursor defining a
  ;;      region is not travelling -- but it used to record where the
  ;;      cursor had got to anyway.  That left the sample after the
  ;;      release with the new position on both sides of the
  ;;      comparison, so a plain click animated nothing at all.
  (let ((started nil)
        (rect [8.0 0.0 8.0 18.0]))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'minibufferp) (lambda (&rest _) nil))
              ((symbol-function 'selected-window) (lambda () nil))
              ((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer)))
              ((symbol-function 'window-start) (lambda (&rest _) 1))
              ((symbol-function 'region-active-p) (lambda () nil))
              ((symbol-function 'smear-cursor--point-rect) (lambda (_w) rect))
              ((symbol-function 'smear-cursor--start)
               (lambda (&rest _) (setq started t))))
      (let ((smear-cursor-mode t)
            (smear-cursor-while-selecting nil))
        (clrhash smear-cursor--last-rects)
        ;; where the cursor was before the click
        (let ((track-mouse nil))
          (setq rect [200.0 0.0 8.0 18.0])
          (smear-cursor--sample))
        ;; the click has moved point, and the button is still down
        (setq rect [8.0 0.0 8.0 18.0])
        (let ((track-mouse 'drag-tracking))
          (setq started nil)
          (smear-cursor--sample)
          (should-not started))
        ;; the button comes up: the move is still there to be drawn
        (let ((track-mouse nil))
          (setq started nil)
          (smear-cursor--sample)
          (should started))))))

;;;; Sprites

;; A sprite is drawn from rectangles, not pixels: the module draws
;; sixteen layers for a whole effect, and a figure of a few hundred
;; pixels has to come inside that.

(ert-deftest smear-cursor-test-a-solid-block-is-one-rectangle ()
  ;; GIVEN a sprite row-set that is one filled block
  ;; WHEN it is broken into rectangles
  ;; THEN there is one, covering the block
  (should (equal (smear-cursor--sprite-rects '("oo" "oo"))
                 '((?o 0 0 2 2 0 2)))))

(ert-deftest smear-cursor-test-blank-cells-draw-nothing ()
  ;; GIVEN rows with spaces
  ;; WHEN they are broken up
  ;; THEN the spaces take no rectangle
  ;;
  ;;      A space is the transparent cell, which is what lets a figure
  ;;      have a silhouette rather than being a box.
  (should (equal (smear-cursor--sprite-rects '(" o " "   "))
                 '((?o 1 0 1 1 1 2)))))

(ert-deftest smear-cursor-test-two-colours-stay-apart ()
  ;; GIVEN a row of two different characters
  ;; WHEN it is broken up
  ;; THEN each gets its own rectangle
  ;;
  ;;      A layer carries one colour for the whole flight, so a run may
  ;;      never span two of them.
  (should (equal (smear-cursor--sprite-rects '("ab"))
                 '((?a 0 0 1 1 0 1) (?b 1 0 1 1 1 2)))))

(ert-deftest smear-cursor-test-a-shape-ends-where-its-edges-stop-agreeing ()
  ;; GIVEN an L of one character
  ;; WHEN it is broken up
  ;; THEN the upright and the foot are separate shapes: the foot is
  ;;      wider than the row above it, and a straight-sided quad
  ;;      cannot be both widths at once.  The upright keeps its own
  ;;      width all the way down rather than flaring into the foot.
  (should (equal (smear-cursor--sprite-rects '("o " "o " "oo"))
                 '((?o 0 0 1 2 0 1) (?o 0 2 2 1 0 2)))))

(ert-deftest smear-cursor-test-a-sprite-plans-for-its-busiest-frame ()
  ;; GIVEN two frames, one needing more rectangles than the other
  ;; WHEN the layers are planned
  ;; THEN the busier frame decides how many that character gets
  ;;
  ;;      A layer keeps its colour and its slot for the whole flight,
  ;;      so the allotment is made once and every frame draws into it.
  (should (equal (smear-cursor--sprite-plan '(("oo") ("o o")))
                 '((?o . 2)))))

(ert-deftest smear-cursor-test-a-sprite-stands-on-the-line ()
  ;; GIVEN a one-cell sprite
  ;; WHEN its quad is worked out for a place
  ;; THEN its foot is at that place, not its middle
  ;;
  ;;      He walks along the text, so the line is the floor.  Centred
  ;;      instead, he would be buried in it to the waist.
  (let ((q (smear-cursor--sprite-quad '(?o 0 0 1 1) '(0.0 . 0.0) 4 1 1 nil))
        (bleed smear-cursor--sprite-bleed))
    (should (= (+ (aref q 1) bleed) -4.0))  ; top, one cell up
    (should (= (- (aref q 5) bleed) 0.0)))) ; foot, on the line

(ert-deftest smear-cursor-test-a-sprite-faces-the-way-it-goes ()
  ;; GIVEN a sprite with something on its left
  ;; WHEN it is drawn flipped
  ;; THEN that thing is on its right
  ;;
  ;;      One drawing faces both ways, so a figure walking back the way
  ;;      it came is not moonwalking.
  (let ((plain (smear-cursor--sprite-quad '(?o 0 0 1 1) '(0.0 . 0.0) 4 4 1 nil))
        (flipped (smear-cursor--sprite-quad '(?o 0 0 1 1) '(0.0 . 0.0) 4 4 1 t))
        (bleed smear-cursor--sprite-bleed))
    (should (= (+ (aref plain 0) bleed) -8.0))
    (should (= (+ (aref flipped 0) bleed) 4.0))))

(ert-deftest smear-cursor-test-a-sprite-layer-carries-a-quad-for-every-phase ()
  ;; GIVEN a sprite and a walk
  ;; WHEN its layers are built
  ;; THEN each holds one quad per place walked, in the palette's colour
  (let* ((sprite '(:palette ((?o . "#010203")) :hold 1 :frames (("o"))))
         (walk '((0.0 . 0.0) (4.0 . 0.0) (8.0 . 0.0)))
         (layers (smear-cursor--sprite-layers sprite walk 4 8.0)))
    (should (= (length layers) 1))
    (should (equal (plist-get (car layers) :color) "#010203"))
    (should (= (length (plist-get (car layers) :quads)) 3))))

(ert-deftest smear-cursor-test-an-unused-slot-repeats-rather-than-showing ()
  ;; GIVEN frames needing different numbers of rectangles
  ;; WHEN the leaner frame is drawn
  ;; THEN its spare slot repeats a rectangle it already has
  ;;
  ;;      A slot with nothing to draw cannot simply be left: a quad of
  ;;      no area still covers its own pixel faintly.  Drawing a
  ;;      rectangle twice in one colour is what shows nothing.
  (let* ((sprite '(:palette ((?o . "#fff")) :hold 1
                   :frames (("o o") ("ooo"))))
         (walk '((0.0 . 0.0) (0.0 . 0.0)))
         (layers (smear-cursor--sprite-layers sprite walk 4 8.0))
         (second (nth 1 layers)))
    (should (= (length layers) 2))
    ;; the second frame needs one rectangle, so both slots draw it
    (should (equal (aref (plist-get (car layers) :quads) 1)
                   (aref (plist-get second :quads) 1)))))

(ert-deftest smear-cursor-test-a-sprite-gives-up-detail-before-shape ()
  ;; GIVEN a plan too big for the layers there are
  ;; WHEN it is fitted
  ;; THEN the character with the most rectangles loses one first
  ;;
  ;;      A character's later rectangles are its smaller pieces, so
  ;;      they are what a figure can afford to lose.  Dropping the
  ;;      character instead takes away a whole colour of him.
  (should (equal (smear-cursor--sprite-fit '((?o . 3) (?c . 1)) 3)
                 '((?o . 2) (?c . 1)))))

(ert-deftest smear-cursor-test-a-sprite-drops-its-topmost-detail-last ()
  ;; GIVEN a plan still too big with every character down to one block
  ;; WHEN it is fitted
  ;; THEN the end of the palette goes, where the fine detail is drawn
  (should (equal (smear-cursor--sprite-fit '((?o . 1) (?c . 1) (?m . 1)) 2)
                 '((?o . 1) (?c . 1)))))

(ert-deftest smear-cursor-test-a-sprite-never-exceeds-its-budget ()
  ;; GIVEN the janitor's own sprite and a module holding very little
  ;; WHEN his layers are built
  ;; THEN they fit, because the trim would take his overalls and leave
  ;;      his moustache hanging in the air.
  (let ((sprite (smear-cursor-sprite 'janitor))
        (walk '((0.0 . 0.0) (4.0 . 0.0))))
    (dolist (budget '(1 3 5 8 12))
      (should (<= (length (smear-cursor--sprite-layers sprite walk 3 20.0 budget))
                  budget)))))

;;;; The saucer and the Grinch

(defmacro smear-cursor-test--on-a-roomy-frame (&rest body)
  "Run BODY with a frame big enough to roam and a module to draw it."
  `(cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21))
             ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
             ((symbol-function 'smear-cursor--point-rect)
              (lambda (_w) (vector 200.0 200.0 10.0 21.0)))
             ((symbol-function 'window-body-width) (lambda (&rest _) 800))
             ((symbol-function 'window-body-height) (lambda (&rest _) 600))
             ((symbol-function 'smear-cursor--max-layers) (lambda () 16)))
     (smear-cursor--roam-reset)
     ,@body))

(ert-deftest smear-cursor-test-the-saucer-flies-above-the-line-it-works ()
  ;; GIVEN the saucer
  ;; WHEN the ship and its beam are compared
  ;; THEN the ship is well clear of the line and the beam reaches down
  ;;      to it.  A saucer standing on the text is a hat.
  (smear-cursor-test--on-a-roomy-frame
   (let* ((layers (plist-get (smear-cursor--ufo) :layers))
          (ship (car (smear-cursor-test--sprite-quads layers)))
          (beam (aref (plist-get
                       (cl-find-if (lambda (l) (eq 'beam (plist-get l :part)))
                                   layers)
                       :quads)
                      0)))
     ;; the beam's foot is lower than the ship's lowest corner
     (should (> (aref beam 5) (aref ship 7)))
     ;; and it is wider at the foot than where it leaves the hull
     (should (> (- (aref beam 4) (aref beam 6))
                (- (aref beam 2) (aref beam 0)))))))

(ert-deftest smear-cursor-test-the-saucer-lifts-what-it-catches ()
  ;; GIVEN the character going up the beam
  ;; WHEN its places are read
  ;; THEN it starts at the line and climbs, over and over: one is
  ;;      taken, then the next.
  (smear-cursor-test--on-a-roomy-frame
   (let* ((layers (plist-get (smear-cursor--ufo) :layers))
          (caught (cl-find-if (lambda (l) (eq 'caught (plist-get l :part)))
                              layers))
          (ys (mapcar (lambda (q) (aref q 1))
                      (append (plist-get caught :quads) nil))))
     (should caught)
     ;; it rises: somewhere in the first few phases it is higher up
     (should (< (nth 8 ys) (nth 0 ys)))
     ;; and it goes back down to start again, rather than leaving once
     (should (> (apply #'max (seq-drop ys 1)) (apply #'min ys))))))

(ert-deftest smear-cursor-test-the-saucer-can-be-told-to-leave-the-text ()
  ;; GIVEN the taking turned off
  ;; WHEN the saucer is built
  ;; THEN nothing covers the text, and it still shines a beam on it.
  (smear-cursor-test--on-a-roomy-frame
   (let* ((smear-cursor-ufo-take-text nil)
          (layers (plist-get (smear-cursor--ufo) :layers)))
     (should-not (cl-some (lambda (l) (eq 'eaten (plist-get l :part))) layers))
     (should (cl-some (lambda (l) (eq 'beam (plist-get l :part))) layers)))))

(ert-deftest smear-cursor-test-the-grinch-takes-the-text ()
  ;; GIVEN the Grinch
  ;; WHEN his layers are read
  ;; THEN the rows he has walked are covered, and he is standing there
  ;;      drawn in blocks over them.
  (smear-cursor-test--on-a-roomy-frame
   (let ((layers (plist-get (smear-cursor--grinch) :layers)))
     (should (cl-some (lambda (l) (eq 'eaten (plist-get l :part))) layers))
     (should (cl-some (lambda (l) (eq 'sprite (plist-get l :part))) layers))
     (should (<= (length layers) (smear-cursor--max-layers))))))

(ert-deftest smear-cursor-test-the-grinch-can-be-told-to-leave-the-text ()
  ;; GIVEN the taking turned off
  ;; WHEN he is built
  ;; THEN he only walks past it.
  (smear-cursor-test--on-a-roomy-frame
   (let* ((smear-cursor-grinch-take-text nil)
          (layers (plist-get (smear-cursor--grinch) :layers)))
     (should-not (cl-some (lambda (l) (eq 'eaten (plist-get l :part))) layers))
     (should (cl-some (lambda (l) (eq 'sprite (plist-get l :part))) layers)))))

(ert-deftest smear-cursor-test-the-new-roamers-fit-what-the-module-holds ()
  ;; GIVEN a module holding as little as eight layers
  ;; WHEN each roamer is built
  ;; THEN it fits, plainer rather than butchered.
  (dolist (build (list #'smear-cursor--ufo #'smear-cursor--grinch
                       #'smear-cursor--pacman #'smear-cursor--janitor))
    (smear-cursor--roam-reset)
    (should (<= (length (plist-get (funcall build) :layers))
                (smear-cursor--max-layers)))))

(ert-deftest smear-cursor-test-a-roamer-carries-its-route-on ()
  ;; GIVEN a route already walked
  ;; WHEN it is recorded
  ;; THEN the next play starts where it ended, and the places behind
  ;;      it are kept so the gap is not filled in at the handover.
  (let ((walk '((0.0 . 0.0) (4.0 . 0.0) (8.0 . 0.0) (12.0 . 0.0))))
    (smear-cursor--roam-advance walk 2)
    (should (equal smear-cursor--roam-from '(12.0 . 0.0)))
    (should (equal smear-cursor--roam-tail '((8.0 . 0.0) (12.0 . 0.0))))))

(ert-deftest smear-cursor-test-sprite-blocks-overlap-at-their-seam ()
  ;; GIVEN two blocks of a sprite stacked one above the other
  ;; WHEN their quads are worked out
  ;; THEN they overlap rather than meeting exactly.
  ;;
  ;;      Every quad is drawn with a soft half-pixel edge, so two that
  ;;      meet exactly each cover their shared line about halfway.
  ;;      Composited one over the other that comes to about three
  ;;      quarters, not one, and the join shows as a darker line
  ;;      across the figure.  A sprite of horizontal bands came out
  ;;      stripey.
  (let ((upper (smear-cursor--sprite-quad '(?o 0 0 1 1) '(0.0 . 0.0) 4 1 2 nil))
        (lower (smear-cursor--sprite-quad '(?o 0 1 1 1) '(0.0 . 0.0) 4 1 2 nil)))
    ;; the upper block's foot reaches past the lower block's head
    (should (> (aref upper 5) (aref lower 1)))))

(ert-deftest smear-cursor-test-a-sprite-stands-on-whole-pixels ()
  ;; GIVEN a place that is not on a pixel boundary
  ;; WHEN the sprite is placed there
  ;; THEN its blocks still land on whole pixels.
  ;;
  ;;      A cell landing on a fraction is resolved differently at each
  ;;      of its edges, which rounds off the corners of a drawing whose
  ;;      whole point is its corners.
  (let* ((q (smear-cursor--sprite-quad '(?o 0 0 1 1) '(0.4 . 0.6) 4 2 1 nil))
         (bleed smear-cursor--sprite-bleed))
    (should (= (+ (aref q 0) bleed) -4.0))
    (should (= (- (aref q 2) bleed) 0.0))))

(ert-deftest smear-cursor-test-what-is-taken-stays-taken ()
  ;; GIVEN a row already emptied by an earlier play
  ;; WHEN the next play lays out its bands
  ;; THEN it opens with that stretch still covered, and covers more.
  ;;
  ;;      The text comes back when the cursor is touched, not a few
  ;;      seconds after it went.  Handing it back while the animation
  ;;      is still running reads as the display failing to keep up.
  (smear-cursor--roam-reset)
  (let* ((first-walk (cl-loop for i below 20 collect (cons (* i 6.0) 0.0)))
         (next-walk (cl-loop for i below 20 collect (cons (+ 120.0 (* i 6.0)) 0.0))))
    (smear-cursor--eaten-bands first-walk 21.0 4 9.0)
    (smear-cursor--eaten-remember first-walk 21.0 9.0 4)
    (let* ((band (car (smear-cursor--eaten-bands next-walk 21.0 4 9.0)))
           (quads (append (plist-get band :quads) nil))
           (opening (car quads))
           (closing (car (last quads))))
      ;; the first frame of the second play already covers the first
      (should (< (aref opening 0) 0.0))
      ;; and by the end it reaches further than it did
      (should (> (aref closing 2) (aref opening 2))))))

(ert-deftest smear-cursor-test-the-text-comes-back-when-the-spell-ends ()
  ;; GIVEN a row emptied during a spell
  ;; WHEN the spell ends
  ;; THEN nothing is remembered, so the next one starts on whole text.
  (smear-cursor--roam-reset)
  (let ((walk (cl-loop for i below 20 collect (cons (* i 6.0) 0.0))))
    (smear-cursor--eaten-remember walk 21.0 9.0 4)
    (should smear-cursor--eaten-spans)
    (smear-cursor--roam-reset)
    (should-not smear-cursor--eaten-spans)))

(ert-deftest smear-cursor-test-a-stretch-with-no-layer-is-never-taken ()
  ;; GIVEN more stretches walked than there are layers to cover them
  ;; WHEN what has been taken is recorded
  ;; THEN the ones past the limit are not, so nothing is taken that
  ;;      cannot go on being covered.
  ;;
  ;;      Remembering a stretch there is no layer for would hand it
  ;;      back the moment another one was emptied, which is the
  ;;      flicker this whole thing exists to stop.
  (smear-cursor--roam-reset)
  (let ((walk (cl-loop for row below 5
                       append (cl-loop for i below 4
                                       collect (cons (* i 6.0) (* row 21.0))))))
    (smear-cursor--eaten-remember walk 21.0 9.0 2)
    (should (= 2 (length smear-cursor--eaten-spans)))))

(ert-deftest smear-cursor-test-a-band-grows-as-he-covers-the-row ()
  ;; GIVEN one play along a row
  ;; WHEN its band is read frame by frame
  ;; THEN it is wider at the end than at the start: a character goes
  ;;      as he reaches it, rather than the row emptying at once.
  (smear-cursor--roam-reset)
  (let* ((walk (cl-loop for i below 30 collect (cons (* i 6.0) 0.0)))
         (band (car (smear-cursor--eaten-bands walk 21.0 4 9.0)))
         (quads (append (plist-get band :quads) nil))
         (width (lambda (q) (abs (- (aref q 2) (aref q 0))))))
    (should (< (funcall width (car quads))
               (funcall width (car (last quads)))))))

(ert-deftest smear-cursor-test-a-sprite-cell-is-a-whole-number-of-pixels ()
  ;; GIVEN a sprite and a height in lines
  ;; WHEN its cell size is worked out
  ;; THEN it is a whole number of pixels, and never below one.
  ;;
  ;;      A cell landing on a fraction is resolved differently at each
  ;;      of its edges, which rounds the corners off a drawing whose
  ;;      whole point is that it has corners.  One pixel is where a
  ;;      figure of many cells drawn small ends up: Pacman is
  ;;      thirty-two cells and about that many pixels.
  (let ((sprite '(:palette ((?o . "#ffffff")) :frames (("o" "o" "o" "o" "o")))))
    (should (integerp (smear-cursor--sprite-pixel sprite 1.5 21.0)))
    (should (>= (smear-cursor--sprite-pixel sprite 0.1 21.0) 1))))

(ert-deftest smear-cursor-test-a-shorter-figure-has-smaller-cells ()
  ;; GIVEN one sprite asked for at two heights
  ;; WHEN the cell sizes are compared
  ;; THEN the shorter figure has the smaller cell, which is the whole
  ;;      of how a roamer is sized against the text.
  (let ((sprite '(:palette ((?o . "#ffffff"))
                  :frames (("o" "o" "o" "o" "o" "o" "o" "o" "o" "o")))))
    (should (< (smear-cursor--sprite-pixel sprite 1.4 21.0)
               (smear-cursor--sprite-pixel sprite 2.8 21.0)))))

(ert-deftest smear-cursor-test-the-janitor-cleans-the-text-away ()
  ;; GIVEN the janitor
  ;; WHEN his layers are read
  ;; THEN the rows he has gone over are covered.  A cleaner who leaves
  ;;      the text exactly as he found it is not cleaning, he is
  ;;      pacing.
  (smear-cursor-test--on-a-roomy-frame
   (let ((layers (plist-get (smear-cursor--janitor) :layers)))
     (should (cl-some (lambda (l) (eq 'eaten (plist-get l :part))) layers))
     (should (cl-some (lambda (l) (eq 'mop (plist-get l :part))) layers))
     (should (<= (length layers) (smear-cursor--max-layers))))))

(ert-deftest smear-cursor-test-the-janitor-can-be-told-to-leave-the-text ()
  ;; GIVEN the cleaning turned off
  ;; WHEN he is built
  ;; THEN he goes over it and leaves it there.
  (smear-cursor-test--on-a-roomy-frame
   (let* ((smear-cursor-janitor-clean-text nil)
          (layers (plist-get (smear-cursor--janitor) :layers)))
     (should-not (cl-some (lambda (l) (eq 'eaten (plist-get l :part))) layers))
     (should (cl-some (lambda (l) (eq 'sprite (plist-get l :part))) layers)))))

(ert-deftest smear-cursor-test-the-beam-decides-what-the-saucer-takes ()
  ;; GIVEN the saucer
  ;; WHEN the beam's footprint is compared with what is taken
  ;; THEN the band is as wide as the beam lands, not as wide as the
  ;;      ship.  It is the beam that does the taking, so a band
  ;;      narrower than it leaves text standing inside the light.
  (smear-cursor-test--on-a-roomy-frame
   (let* ((layers (plist-get (smear-cursor--ufo) :layers))
          (beam (aref (plist-get
                       (cl-find-if (lambda (l) (eq 'beam (plist-get l :part)))
                                   layers)
                       :quads)
                      0))
          (foot (- (aref beam 4) (aref beam 6)))
          ;; the opening frame, before the band has grown along the row
          (band (let ((q (aref (plist-get
                                (cl-find-if
                                 (lambda (l) (eq 'eaten (plist-get l :part)))
                                 layers)
                                :quads)
                               0)))
                  (abs (- (aref q 2) (aref q 0))))))
     (should (>= band foot)))))

(ert-deftest smear-cursor-test-what-takes-the-text-says-how-wide ()
  ;; GIVEN two reaches, one twice the other
  ;; WHEN the stretch emptied is worked out
  ;; THEN the wider one empties more.  A mouth, the head of a mop and
  ;;      the foot of a beam are different widths, and each effect
  ;;      says its own.
  (let ((narrow (smear-cursor--eaten-reach '(100.0 . 0.0) 5.0))
        (wide (smear-cursor--eaten-reach '(100.0 . 0.0) 10.0)))
    (should (= (- (cdr narrow) (car narrow)) 10.0))
    (should (= (- (cdr wide) (car wide)) 20.0))))

(ert-deftest smear-cursor-test-a-staircase-is-one-trapezoid ()
  ;; GIVEN a run of rows whose edge steps evenly sideways
  ;; WHEN it is broken up
  ;; THEN it is one shape, not one per step.
  ;;
  ;;      A layer is any four-cornered shape, not just an upright box,
  ;;      and a diagonal in a drawing is a staircase.  Taken a step at
  ;;      a time a round figure costs a layer a row, which is most of
  ;;      what the module holds for a whole effect.
  (should (equal (smear-cursor--sprite-rects
                  '("oooooo" "  oooo" "    oo"))
                 '((?o 0 0 6 3 4 6)))))

(ert-deftest smear-cursor-test-a-slope-has-to-keep-its-step ()
  ;; GIVEN an edge that steps sideways and then stops
  ;; WHEN it is broken up
  ;; THEN the slope ends where the stepping does.
  ;;
  ;;      A shape is a straight-sided quad, so a run of rows may only
  ;;      join one while the edge keeps moving by the same amount.
  (let ((shapes (smear-cursor--sprite-rects '("oooo" "  oo" "  oo"))))
    (should (= 2 (length shapes)))))

(ert-deftest smear-cursor-test-a-sloped-shape-leans-the-way-it-steps ()
  ;; GIVEN a shape whose left edge steps right as it goes down
  ;; WHEN its quad is worked out
  ;; THEN the bottom-left corner is right of the top-left one.
  ;;      Drawn as an upright box it would cover the steps it should
  ;;      be cutting away.
  (let ((q (smear-cursor--sprite-quad '(?o 0 0 6 3 4 6) '(0.0 . 0.0)
                                      4 6 3 nil)))
    ;; corner 0 is the top left, corner 3 the bottom left
    (should (> (aref q 6) (aref q 0)))))

(ert-deftest smear-cursor-test-the-eating-comes-before-the-chase ()
  ;; GIVEN a module with barely room for Pacman himself
  ;; WHEN he is built with ghosts and then without
  ;; THEN he is eating either way, and the chase is what goes without.
  ;;
  ;;      It used to be the other way about: a ghost's worth was held
  ;;      back first, so a full chase took every layer that was left
  ;;      and he crossed the text without a mark on it.  Eating is the
  ;;      one thing the effect is for, so it is served first and the
  ;;      ghosts have the rest.  Twelve layers is where that shows; at
  ;;      thirty-two there is room for both.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 200.0 200.0 10.0 21.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 600))
            ((symbol-function 'smear-cursor--max-layers) (lambda () 12)))
    (let* ((parts (lambda (ghosts part)
                    (let ((smear-cursor-pacman-ghosts ghosts))
                      (smear-cursor--roam-reset)
                      (seq-count (lambda (l) (eq part (plist-get l :part)))
                                 (plist-get (smear-cursor--pacman) :layers))))))
      (should (> (funcall parts 2 'eaten) 0))
      (should (= (funcall parts 2 'ghost) 0))
      (should (= (funcall parts 0 'ghost) 0))
      (should (> (funcall parts 0 'eaten) 0)))))

(ert-deftest smear-cursor-test-a-sprite-heads-the-way-it-last-moved ()
  ;; GIVEN a walk that goes right, then up, then stands still
  ;; WHEN the heading is read at each point
  ;; THEN it is the last direction it actually travelled in, so a
  ;;      figure that has stopped keeps facing the way it was going.
  (let ((walk '((0.0 . 0.0) (10.0 . 0.0) (10.0 . -20.0) (10.0 . -20.0))))
    (should (eq 'right (smear-cursor--sprite-heading walk 1)))
    (should (eq 'up (smear-cursor--sprite-heading walk 2)))
    (should (eq 'up (smear-cursor--sprite-heading walk 3)))))

(ert-deftest smear-cursor-test-a-sprite-without-a-way-up-uses-its-side ()
  ;; GIVEN a sprite drawn only from the side
  ;; WHEN it travels upwards
  ;; THEN it is still drawn, from the frames it has.
  ;;
  ;;      Facing up is worth a set of frames for Pacman, whose mouth
  ;;      has to point where he is going, and worth nothing for a man
  ;;      with a mop.
  (let* ((sprite '(:palette ((?o . "#ffffff")) :hold 1 :frames (("o"))))
         (walk '((0.0 . 0.0) (0.0 . -20.0)))
         (layers (smear-cursor--sprite-layers sprite walk 4 20.0)))
    (should (= 1 (length layers)))
    (should (= 2 (length (plist-get (car layers) :quads))))))

(ert-deftest smear-cursor-test-a-sprite-turns-to-face-up ()
  ;; GIVEN a sprite with its own frames for going up
  ;; WHEN it travels upwards
  ;; THEN those are the ones drawn.
  (let* ((sprite '(:palette ((?o . "#ffffff")) :hold 1
                   :frames (("oo" "  "))
                   :frames-up (("  " "oo"))))
         (walk '((0.0 . 0.0) (0.0 . -20.0)))
         (quads (plist-get (car (smear-cursor--sprite-layers sprite walk 4 20.0))
                           :quads))
         ;; Measured from where it stands, not in the window: it has
         ;; travelled between the two, and that would swamp the
         ;; difference being looked for.
         (rel (lambda (k foot) (- (aref (aref quads k) 1) foot))))
    ;; the side frame draws on the top row, the up frame on the bottom
    (should (< (funcall rel 0 10.0) (funcall rel 1 -10.0)))))

(ert-deftest smear-cursor-test-a-sloped-side-keeps-its-slope ()
  ;; GIVEN a shape whose edge steps one cell a row
  ;; WHEN its quad is worked out
  ;; THEN the drawn side has exactly that slope.
  ;;
  ;;      Shapes are grown half a pixel so their seams do not show,
  ;;      and growing a sloped side straight up and down instead of
  ;;      along itself leaves each end of it off the line.  Two shapes
  ;;      stacked on one straight edge -- which is what the two sides
  ;;      of Pacman's mouth are made of -- then meet with a notch at
  ;;      every join, and the mouth comes out jagged however exactly
  ;;      the cells were cut.
  (let* ((q (smear-cursor--sprite-quad '(?o 0 0 4 4 4 4) '(0.0 . 0.0)
                                       2 8 4 nil))
         (run (- (aref q 6) (aref q 0)))    ; bottom-left x, less top-left x
         (rise (- (aref q 7) (aref q 1))))
    ;; one cell across per cell down, the step the shape was built
    ;; from, so the shape below it carries the same line on
    (should (< (abs (- (/ run rise) 1.0)) 0.001))))

(ert-deftest smear-cursor-test-a-shape-does-not-lean-into-the-one-below ()
  ;; GIVEN a wide block sitting straight on top of a narrow one
  ;; WHEN it is broken up
  ;; THEN the wide block keeps its width all the way down.
  ;;
  ;;      A side ends where the side below it starts, so the join does
  ;;      not show, but only as far as one more step.  A torso that
  ;;      took the width of the legs under it came out tapered to a
  ;;      point, and the janitor lost his overalls to it.
  (let ((shapes (smear-cursor--sprite-rects '("oooooo" "oooooo" "  oo  "))))
    (should (= 2 (length shapes)))
    ;; the block's foot is as wide as its head: 0 to 6, not 2 to 4
    (should (equal (nthcdr 5 (car shapes)) '(0 6)))))

(ert-deftest smear-cursor-test-a-side-does-not-reach-past-its-own-shape ()
  ;; GIVEN a shape whose edge steps, above a row that juts out further
  ;;       than the step would reach
  ;; WHEN it is broken up
  ;; THEN the shape stops at its own last row.
  ;;
  ;;      Reaching past it puts colour where the figure is not, which
  ;;      on a curve is a spike at every turn.  Pacman was covered in
  ;;      them.
  (let ((shapes (smear-cursor--sprite-rects '("  oooo  " " oooooo " "oooooooo"))))
    ;; the first shape steps out by one a row; the row below it steps
    ;; out by one as well, so that side is carried on
    (should (equal (nthcdr 5 (car shapes)) '(0 8)))))

(ert-deftest smear-cursor-test-a-side-stops-where-the-outline-turns ()
  ;; GIVEN a shape whose edge steps out by one, over a row that steps
  ;;       out by two
  ;; WHEN it is broken up
  ;; THEN the side stops at its own last row rather than leaning out
  ;;      to meet a line that is not its own.
  (let ((shapes (smear-cursor--sprite-rects '("   oo   " "  oooo  " "oooooooo"))))
    (should (equal (nthcdr 5 (car shapes)) '(2 6)))))

(ert-deftest smear-cursor-test-pacman-is-drawn-as-a-circle ()
  ;; GIVEN his art with the mouth shut
  ;; WHEN it is read
  ;; THEN the middle is filled and the corners are not.
  (let ((art (smear-cursor--pacman-art 16 0.0 'side)))
    (should (= 16 (length art)))
    (should (eq ?o (aref (nth 8 art) 8)))
    (should (eq ?\s (aref (nth 0 art) 0)))
    (should (eq ?\s (aref (nth 15 art) 15)))))

(ert-deftest smear-cursor-test-pacmans-mouth-opens-the-way-he-faces ()
  ;; GIVEN his art open, facing each way
  ;; WHEN the cell just inside the rim is read on each side
  ;; THEN it is missing on the side he faces and there on the others.
  ;;
  ;;      His mouth points where he is going: the arcade turns him,
  ;;      and a mirror can only do left and right.
  (let ((side (smear-cursor--pacman-art 16 1.0 'side))
        (up (smear-cursor--pacman-art 16 1.0 'up))
        (down (smear-cursor--pacman-art 16 1.0 'down)))
    ;; middle row, left edge: gone facing left, there facing up
    (should (eq ?\s (aref (nth 8 side) 1)))
    (should (eq ?o (aref (nth 8 up) 1)))
    ;; middle column, top edge: gone facing up, there facing down
    (should (eq ?\s (aref (nth 1 up) 8)))
    (should (eq ?o (aref (nth 1 down) 8)))
    (should (eq ?\s (aref (nth 14 down) 8)))))

(ert-deftest smear-cursor-test-a-shut-mouth-is-a-whole-circle ()
  ;; GIVEN the mouth shut
  ;; WHEN the art is compared between headings
  ;; THEN they are the same, because a circle has no front.
  (should (equal (smear-cursor--pacman-art 16 0.0 'side)
                 (smear-cursor--pacman-art 16 0.0 'up))))

(ert-deftest smear-cursor-test-a-side-runs-on-inward-to-its-point ()
  ;; GIVEN a side stepping inward, over a row that stops advancing
  ;; WHEN it is broken up
  ;; THEN the side carries its step anyway.
  ;;
  ;;      Inward it can only eat into its own last row, so it can
  ;;      never put colour outside the figure, and carrying on is what
  ;;      keeps a long edge straight all the way to its end.  Each
  ;;      side of Pacman's mouth runs to a point where the cells stop
  ;;      advancing, and held back there the mouth bent.
  (let ((shapes (smear-cursor--sprite-rects '("oooo" " ooo" "  oo" "  oo"))))
    ;; three rows stepping in by one, then a row that does not: the
    ;; left side still ends a step on, at three rather than two
    (should (equal (nth 5 (car shapes)) 3))))

(ert-deftest smear-cursor-test-a-row-can-be-eaten-in-two-places ()
  ;; GIVEN a roamer that eats one end of a row, leaves it, and comes
  ;;       back to the row further along
  ;; WHEN what has been taken is recorded
  ;; THEN the row holds two stretches, and the text between them is
  ;;      not covered.
  ;;
  ;;      A row kept one stretch, from the leftmost thing taken to the
  ;;      rightmost, so coming back to a row swallowed everything he
  ;;      had walked past on the way.
  (smear-cursor--roam-reset)
  (let ((walk (append
               ;; along the top row
               (cl-loop for i below 10 collect (cons (* i 10.0) 0.0))
               ;; down a row and away to the right
               (cl-loop for i below 20 collect (cons (+ 100.0 (* i 10.0)) 21.0))
               ;; and back up, well past where he left off
               (cl-loop for i below 5 collect (cons (+ 300.0 (* i 10.0)) 0.0)))))
    (smear-cursor--eaten-remember walk 21.0 5.0 8)
    (let ((top (seq-filter (lambda (s) (= 0 (car s)))
                           smear-cursor--eaten-spans)))
      (should (= 2 (length top)))
      ;; nothing covers the middle of the row, which he only flew over
      (should-not (cl-some (lambda (s) (and (< (cadr s) 200.0)
                                            (> (cddr s) 200.0)))
                           top)))))

(ert-deftest smear-cursor-test-two-stretches-of-a-row-join-when-they-meet ()
  ;; GIVEN two stretches of one row with a gap between them
  ;; WHEN the gap is eaten
  ;; THEN they become one stretch rather than three.
  (smear-cursor--roam-reset)
  (smear-cursor--eaten-remember '((0.0 . 0.0)) 21.0 5.0 8)
  (smear-cursor--eaten-remember '((100.0 . 0.0)) 21.0 5.0 8)
  (should (= 2 (length smear-cursor--eaten-spans)))
  (smear-cursor--eaten-remember
   (cl-loop for i below 12 collect (cons (* i 10.0) 0.0)) 21.0 5.0 8)
  (should (= 1 (length smear-cursor--eaten-spans))))

(ert-deftest smear-cursor-test-places-join-across-the-stride ()
  ;; GIVEN a route whose places are further apart than the reach
  ;; WHEN it is folded in
  ;; THEN it is one stretch, not one per place.
  ;;
  ;;      A roamer takes the text a place at a time, however far apart
  ;;      the route put them.  Judged on the reach alone, a reach
  ;;      shorter than the stride starts a stretch at every place and
  ;;      spends every layer in a moment.
  (let ((walk (cl-loop for i below 12 collect (cons (* i 9.0) 0.0))))
    (should (= 1 (length (smear-cursor--eaten-fold nil walk 21.0 0.0 8))))))

(ert-deftest smear-cursor-test-the-text-in-his-mouth-is-still-there ()
  ;; GIVEN Pacman setting off across whole text
  ;; WHEN the stretch he has taken is read at its first frame
  ;; THEN it is a mouthful wide and no wider: he has closed on what is
  ;;      under the jaw and not on what the open wedge still shows.
  ;;
  ;;      Taken to his leading edge, the background-coloured band fills
  ;;      the wedge and his mouth reads as a black hole rather than as
  ;;      a mouth.  Taken at his middle, nothing under the wedge is
  ;;      eaten at all and he crosses the text hiding it.
  (smear-cursor-test--on-a-roomy-frame
   (let* ((layers (plist-get (smear-cursor--pacman) :layers))
          (width (lambda (q) (abs (- (aref q 2) (aref q 0)))))
          ;; the stretch he is eating on this play, which is the one
          ;; that ends up widest: the others are stretches from before,
          ;; opening where they were left
          (band (car (sort (seq-filter (lambda (l) (eq 'eaten (plist-get l :part)))
                                       layers)
                           (lambda (a b)
                             (> (funcall width (car (last (append (plist-get a :quads) nil))))
                                (funcall width (car (last (append (plist-get b :quads) nil)))))))))
          (quads (append (plist-get band :quads) nil))
          (across (* smear-cursor-pacman-size (frame-char-height))))
     (should band)
     (should (> (funcall width (car quads)) 0.0))
     (should (< (funcall width (car quads)) across))
     (should (> (funcall width (car (last quads))) 20.0)))))

(ert-deftest smear-cursor-test-eating-carries-on-when-the-layers-are-full ()
  ;; GIVEN every layer already spent on a stretch
  ;; WHEN another stretch is eaten somewhere new
  ;; THEN it is taken, and the one eaten from longest ago is let go.
  ;;
  ;;      Refusing it stopped the eating dead: the stretches filled
  ;;      the layers within two plays and nothing was taken for the
  ;;      rest of the spell, however far he walked.
  (let* ((spans '((0 0.0 . 10.0) (1 0.0 . 10.0) (2 0.0 . 10.0)))
         (walk '((500.0 . 63.0) (508.0 . 63.0)))
         (out (smear-cursor--eaten-fold spans walk 21.0 0.0 3)))
    (should (= 3 (length out)))
    (should (cl-some (lambda (s) (= 3 (car s))) out))
    ;; the oldest went, the others stayed
    (should-not (cl-some (lambda (s) (= 0 (car s))) out))
    (should (cl-some (lambda (s) (= 1 (car s))) out))))

(ert-deftest smear-cursor-test-a-gap-is-never-eaten-to-make-room ()
  ;; GIVEN a row eaten in two places, the layers full, and a new row
  ;;       eaten somewhere else
  ;; WHEN what has been taken is worked out
  ;; THEN the two stretches of that row are still two, with the text
  ;;      between them still there.
  ;;
  ;;      Joining them would have made room at the cost of a gap he
  ;;      never ate, and the pair to join is always the pair on the
  ;;      row he has just come back to -- so the gap collapsed exactly
  ;;      where it was meant to be kept.
  (let* ((spans '((5 0.0 . 10.0) (0 0.0 . 10.0) (0 100.0 . 110.0)))
         (walk '((500.0 . 42.0) (508.0 . 42.0)))
         (out (smear-cursor--eaten-fold spans walk 21.0 0.0 3))
         (row0 (seq-filter (lambda (s) (= 0 (car s))) out)))
    (should (= 2 (length row0)))
    (should-not (cl-some (lambda (s) (and (< (cadr s) 50.0) (> (cddr s) 50.0)))
                         row0))))

(ert-deftest smear-cursor-test-a-roamer-keeps-within-its-rows-of-the-cursor ()
  ;; GIVEN a spell of many plays, each carrying on from the last
  ;; WHEN the rows walked are collected
  ;; THEN none is further from the cursor than it was allowed.
  ;;
  ;;      The allowance bounded each play rather than the whole spell,
  ;;      so a roamer drifted: twelve rows out after half a minute,
  ;;      pinned against the side of the frame.  It is also what keeps
  ;;      the eating: a row visited once and left is a stretch of its
  ;;      own, and wandering across more rows than there are layers
  ;;      churns through them, handing text back after a few seconds.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 40.0 400.0 10.0 21.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 1900))
            ((symbol-function 'window-body-height) (lambda (&rest _) 1000)))
    (smear-cursor--roam-reset)
    (let ((worst 0))
      (dotimes (play 20)
        (let ((walk (smear-cursor--roam-route 2 40 (+ 7 play))))
          (dolist (place walk)
            (setq worst (max worst (abs (round (/ (cdr place) 21.0))))))
          (smear-cursor--roam-advance walk 1)))
      (should (<= worst 2)))))

(ert-deftest smear-cursor-test-a-roamer-keeps-within-its-columns-too ()
  ;; GIVEN a spell of many plays
  ;; WHEN the places walked are collected
  ;; THEN none is further along the line than it was allowed.
  ;;
  ;;      Bounded up and down but not side to side, a roamer walks off
  ;;      across the frame.  On every row it eats a short piece, leaves,
  ;;      and comes back somewhere else, so the stretches are short and
  ;;      many and the layers churn through them.  Held near the cursor
  ;;      it goes back over its own ground, and stretches join instead
  ;;      of piling up.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 21))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 400.0 400.0 10.0 21.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 1900))
            ((symbol-function 'window-body-height) (lambda (&rest _) 1000)))
    (smear-cursor--roam-reset)
    (let ((worst 0.0))
      (dotimes (play 20)
        (let ((walk (smear-cursor--roam-route 2 40 (+ 7 play))))
          (dolist (place walk)
            (setq worst (max worst (abs (car place)))))
          (smear-cursor--roam-advance walk 1)))
      (should (<= worst (* 10.0 smear-cursor-roam-columns))))))

(ert-deftest smear-cursor-test-a-pulse-flashes-as-often-as-asked ()
  ;; GIVEN a pulse of one flash and a pulse of several
  ;; WHEN the envelopes are read
  ;; THEN each has that many peaks, and both start and end dark.
  (let ((peaks (lambda (env)
                 (cl-loop for (a b c) on env
                          count (and b c (> (cdr b) (cdr a))
                                     (>= (cdr b) (cdr c)))))))
    (should (= 1 (funcall peaks (smear-cursor--pulse-envelope 1))))
    (should (= 4 (funcall peaks (smear-cursor--pulse-envelope 4))))
    (dolist (n '(1 2 5))
      (let ((env (smear-cursor--pulse-envelope n)))
        (should (= 0.0 (cdr (car env))))
        (should (= 1.0 (car (car (last env)))))
        (should (= 0.0 (cdr (car (last env)))))
        ;; stops climb, or the module reads them as one flat run
        (should (apply #'< (mapcar #'car env)))))))

(ert-deftest smear-cursor-test-a-flashing-pulse-fades-as-it-goes ()
  ;; GIVEN a pulse of several flashes
  ;; WHEN their heights are compared
  ;; THEN each is a little dimmer than the one before, so it reads as
  ;;      one thing settling rather than a light left switched on.
  (let* ((env (smear-cursor--pulse-envelope 4))
         (tops (cl-loop for (a b c) on env
                        when (and b c (> (cdr b) (cdr a)) (>= (cdr b) (cdr c)))
                        collect (cdr b))))
    (should (= 4 (length tops)))
    (should (apply #'> tops))))

(ert-deftest smear-cursor-test-a-pulse-flashes-no-faster-than-it-is-drawn ()
  ;; GIVEN more flashes asked for than there are frames to draw them
  ;; WHEN the pulse is built
  ;; THEN it flashes as often as the frames allow and no oftener.
  ;;
  ;;      A flash needs a frame lit and a frame dark.  Asked for more
  ;;      than half the frames, most of them fall between one frame
  ;;      and the next and are never drawn at all: what reaches the
  ;;      screen is not a fast blink but a shimmer of whichever ones
  ;;      happened to land on a frame.
  (let* ((smear-cursor-fps 30)
         (smear-cursor-effect-fps nil)
         (smear-cursor-pulse-duration 0.5)
         (smear-cursor-pulse-flashes 42)
         (env (plist-get (car (plist-get (smear-cursor--line-pulse) :layers))
                         :envelope))
         (peaks (cl-loop for (a b c) on env
                         count (and b c (> (cdr b) (cdr a)) (>= (cdr b) (cdr c))))))
    (should (> peaks 1))
    (should (<= peaks 8))))


(ert-deftest smear-cursor-test-a-scan-throws-the-trail-end-to-end ()
  ;; GIVEN the scan command
  ;; WHEN it runs
  ;; THEN it aims the trail at one end of the line and then the other,
  ;;      as many times as asked.
  ;;
  ;;      Not a shape of its own: what it is meant to look like is
  ;;      what cycling end and start of line looks like, and that is
  ;;      the trail being retargeted.  Drawn as a point with a tail
  ;;      instead it was not close.
  (let ((aimed nil))
    (cl-letf (((symbol-function 'smear-cursor--start)
               (lambda (_win _old new) (push (aref new 0) aimed)))
              ((symbol-function 'smear-cursor--point-rect)
               (lambda (_w) (vector 300.0 200.0 10.0 21.0)))
              ((symbol-function 'window-text-width) (lambda (&rest _) 800))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'run-at-time)
               (lambda (_t _r fn &rest args) (apply fn args) 'timer)))
      (let ((smear-cursor-mode t)
            (smear-cursor-scan-passes 4))
        (smear-cursor-scan-line)
        (setq aimed (nreverse aimed))
        (should (= 4 (length aimed)))
        ;; the far end, then the near one, and so on, and home last
        (should (> (nth 0 aimed) 700))
        (should (< (nth 1 aimed) 100))
        (should (> (nth 2 aimed) 700))
        (should (= 300.0 (nth 3 aimed)))))))

(ert-deftest smear-cursor-test-a-scan-ends-where-the-cursor-is ()
  ;; GIVEN a scan of an odd number of passes
  ;; WHEN it finishes
  ;; THEN the last throw is back to the cursor, so the trail settles
  ;;      where the cursor actually is rather than at the end of the
  ;;      line it happened to stop at.
  (let ((aimed nil))
    (cl-letf (((symbol-function 'smear-cursor--start)
               (lambda (_win _old new) (push (aref new 0) aimed)))
              ((symbol-function 'smear-cursor--point-rect)
               (lambda (_w) (vector 300.0 200.0 10.0 21.0)))
              ((symbol-function 'window-text-width) (lambda (&rest _) 800))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'run-at-time)
               (lambda (_t _r fn &rest args) (apply fn args) 'timer)))
      (let ((smear-cursor-mode t)
            (smear-cursor-scan-passes 3))
        (smear-cursor-scan-line)
        (should (= 300.0 (car aimed)))))))

(ert-deftest smear-cursor-test-what-is-eaten-takes-the-background-it-covers ()
  ;; GIVEN a stretch over text with a background of its own
  ;; WHEN the colour to cover it with is worked out
  ;; THEN it is that background, not the buffer's.
  ;;
  ;;      Covered in the `default' face's background, a stretch over a
  ;;      highlighted line or any face with its own reads as a block
  ;;      dropped on the text rather than as text gone.
  (with-temp-buffer
    (insert "one two three")
    (put-text-property 4 8 'face '(:background "#123456"))
    (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
      ;; Compared with what the colour resolves to here rather than to
      ;; a written-out triple: a batch Emacs has no display, and snaps
      ;; a colour to the few a terminal has.
      (should (equal (smear-cursor--eaten-bg-at nil 5)
                     (smear-cursor--color-rgb "#123456")))
      ;; and outside it, the buffer's own
      (should-not (equal (smear-cursor--eaten-bg-at nil 1)
                         (smear-cursor--eaten-bg-at nil 5))))))

(ert-deftest smear-cursor-test-an-active-region-is-a-background-too ()
  ;; GIVEN a stretch over an active region
  ;; WHEN the colour is worked out
  ;; THEN it is the region's background.
  ;;
  ;;      The region is painted by redisplay rather than carried on the
  ;;      text, so nothing at the position says it is there and the
  ;;      face lookup alone comes back with the buffer's own.
  (with-temp-buffer
    (insert "one two three")
    (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer)))
              ((symbol-function 'region-active-p) (lambda () t))
              ((symbol-function 'region-beginning) (lambda () 4))
              ((symbol-function 'region-end) (lambda () 8))
              ((symbol-function 'smear-cursor--face-attr)
               (lambda (face attr)
                 (if (and (eq face 'region) (eq attr :background)) "#654321" nil))))
      (should (equal (smear-cursor--eaten-bg-at nil 5)
                     (smear-cursor--color-rgb "#654321"))))))

(ert-deftest smear-cursor-test-each-stretch-takes-its-own-background ()
  ;; GIVEN two stretches over differently coloured text
  ;; WHEN the bands are built
  ;; THEN each carries the colour sampled where it lies, rather than
  ;;      one colour for all of them.
  (smear-cursor--roam-reset)
  (cl-letf (((symbol-function 'smear-cursor--eaten-color)
             (lambda (span _lh) (if (= 0 (car span)) [1 1 1] [2 2 2]))))
    (let* ((walk (append (cl-loop for i below 6 collect (cons (* i 8.0) 0.0))
                         (cl-loop for i below 6 collect (cons (* i 8.0) 21.0))))
           (bands (smear-cursor--eaten-bands walk 21.0 8 5.0))
           (colors (mapcar (lambda (b) (plist-get b :color)) bands)))
      (should (member [1 1 1] colors))
      (should (member [2 2 2] colors)))))

(ert-deftest smear-cursor-test-a-roamer-plays-in-the-prompt ()
  ;; GIVEN the minibuffer active and the cursor left alone
  ;; WHEN the idle effect comes round
  ;; THEN it plays in the minibuffer window itself, over the prompt.
  ;;
  ;;      Reading a prompt is time spent not typing, which is the
  ;;      occasion an idle effect exists for, and the prompt is what
  ;;      there is to roam over.  Any key restores it: the interrupt
  ;;      runs from `post-command-hook', so nothing stays eaten under
  ;;      what is being typed.
  (let ((where nil))
    (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t))
              ((symbol-function 'current-idle-time) (lambda () 1))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (&rest _) (vector 0.0 0.0 8.0 18.0)))
              ((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage))
              ((symbol-function 'smear-cursor--play-effect)
               (lambda (win &rest _) (setq where win)))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (let ((smear-cursor-mode t)
            (smear-cursor-idle-effect 'pacman)
            (smear-cursor--idle-next nil))
        (let ((smear-cursor-idle-while-prompting t))
          (setq where nil)
          (smear-cursor--idle-play)
          (should (eq where (selected-window))))
        (let ((smear-cursor-idle-while-prompting nil))
          (setq where nil)
          (smear-cursor--idle-play)
          (should-not where))))))

(ert-deftest smear-cursor-test-the-prompt-is-one-line-to-roam ()
  ;; GIVEN a window one line tall, as the minibuffer is
  ;; WHEN a route is worked out for it
  ;; THEN it keeps to that line.
  ;;
  ;;      The row allowance is bounded by the room the window has, so
  ;;      nothing has to know that this one is the minibuffer.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 18))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 40.0 0.0 8.0 18.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 1900))
            ((symbol-function 'window-body-height) (lambda (&rest _) 18)))
    (smear-cursor--roam-reset)
    (let ((walk (smear-cursor--roam-route 6 40 3)))
      (should walk)
      (dolist (place walk)
        (should (= 0.0 (cdr place)))))))
(ert-deftest smear-cursor-test-standing-down-at-a-prompt-looks-at-the-window ()
  ;; GIVEN the minibuffer active but some other buffer current
  ;; WHEN the idle timer comes round with prompting turned off
  ;; THEN nothing plays.
  ;;
  ;;      The timer runs in whatever buffer was last current, which
  ;;      during a read need not be the minibuffer.  Asked about the
  ;;      current buffer the answer is no and the effect plays into the
  ;;      prompt the setting just said to leave alone; asked about the
  ;;      window it is drawing in, the answer is right.
  (let ((where nil))
    (cl-letf (((symbol-function 'minibufferp)
               (lambda (&optional buffer &rest _) (and buffer t)))
              ((symbol-function 'current-idle-time) (lambda () 1))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (&rest _) (vector 0.0 0.0 8.0 18.0)))
              ((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage))
              ((symbol-function 'smear-cursor--play-effect)
               (lambda (win &rest _) (setq where win)))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (let ((smear-cursor-mode t)
            (smear-cursor-idle-effect 'pacman)
            (smear-cursor--idle-next nil)
            (smear-cursor-idle-while-prompting nil))
        (smear-cursor--idle-play)
        (should-not where)))))
(ert-deftest smear-cursor-test-a-new-idle-delay-arms-the-timer-again ()
  ;; GIVEN the mode on with the idle timer armed
  ;; WHEN the delay is changed
  ;; THEN the timer is armed again at the new delay.
  ;;
  ;;      The delay is baked into the timer when it is made, so a
  ;;      setting changed on its own does nothing until something
  ;;      restarts the timer.  Nobody should have to know that.
  (let ((armed nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs &rest _) (setq armed secs) 'timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (let ((smear-cursor-mode t)
            (smear-cursor-idle-effect 'pacman)
            (smear-cursor--idle-timer nil)
            (smear-cursor-idle-delay 1.5))
        (customize-set-variable 'smear-cursor-idle-delay 4.0)
        (should (= 4.0 smear-cursor-idle-delay))
        (should (= 4.0 armed)))
      ;; and not while the mode is off: there is no timer to arm
      (let ((smear-cursor-mode nil)
            (smear-cursor-idle-effect 'pacman)
            (smear-cursor--idle-timer nil)
            (smear-cursor-idle-delay 1.5))
        (setq armed nil)
        (customize-set-variable 'smear-cursor-idle-delay 4.0)
        (should-not armed)))))
(ert-deftest smear-cursor-test-an-effect-may-throw-the-trail-instead ()
  ;; GIVEN an effect that names a function rather than layers
  ;; WHEN it is played
  ;; THEN the function runs, and no layers are asked for.
  ;;
  ;;      What a line scan looks like is the trail aimed at one end of
  ;;      the line and then the other before it has settled.  A shape
  ;;      drawn to imitate that is not close, so an effect has to be
  ;;      able to say "throw the trail" as well as "draw this".
  (let ((played nil) (layers 0))
    (smear-cursor-define-effect 'test-thrower
      :doc "Throw something." :shape 'region
      :play (lambda (win rects) (setq played (list win (length rects)))))
    (unwind-protect
        (cl-letf (((symbol-function 'smear-cursor--effect-layers)
                   (lambda (&rest _) (setq layers (1+ layers)) nil))
                  ((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage)))
          (smear-cursor--play-effect (selected-window) 'test-thrower
                                     (list (vector 0.0 0.0 8.0 18.0)) 1)
          (should (equal played (list (selected-window) 1)))
          (should (zerop layers)))
      (remhash 'test-thrower smear-cursor--effects))))

(ert-deftest smear-cursor-test-the-line-scan-is-an-effect-like-any-other ()
  ;; GIVEN the line scan, which was a command and nothing else
  ;; WHEN the effects are listed
  ;; THEN it is among them, and choosing it for an occasion scans.
  ;;
  ;;      It is the answer to "where is the cursor on this line", which
  ;;      is what the pulse occasion is for, so it belongs on the same
  ;;      list as the washes -- offered by name, played by the trail.
  (should (smear-cursor-effect 'line-scan))
  (should (memq 'line-scan (smear-cursor--effect-names)))
  (let ((scanned 0))
    (cl-letf (((symbol-function 'smear-cursor--scan)
               (lambda (&rest _) (setq scanned (1+ scanned))))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor--line-rect)
               (lambda (_w) (vector 0.0 0.0 80.0 18.0)))
              ((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage)))
      (let ((smear-cursor-effects '((pulse . line-scan))))
        (smear-cursor-pulse-line)
        (should (= 1 scanned)))
      ;; and the command is the same scan, not a second copy of it
      (let ((smear-cursor-mode t))
        (smear-cursor-scan-line)
        (should (= 2 scanned))))))
(ert-deftest smear-cursor-test-a-roamer-wanders-both-ways ()
  ;; GIVEN room on either side of the cursor
  ;; WHEN a spell of plays runs, each starting where the last ended
  ;; THEN he covers ground to the left of the cursor as well as the
  ;;      right, rather than running to one edge and staying there.
  ;;
  ;;      He used to step right whenever there was room and left only
  ;;      when there was none.  That is not a wander: he reached the
  ;;      right-hand end of his range in a few plays and spent the rest
  ;;      of the time turning back and forth over the same few
  ;;      characters, which is what it looked like.
  (cl-letf (((symbol-function 'frame-char-height) (lambda (&rest _) 18))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
            ((symbol-function 'smear-cursor--point-rect)
             (lambda (_w) (vector 400.0 200.0 8.0 18.0)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 1600))
            ((symbol-function 'window-body-height) (lambda (&rest _) 720)))
    (smear-cursor--roam-reset)
    (let ((xs nil) (span (* 8.0 smear-cursor-roam-columns)))
      (dotimes (i 24)
        (let ((walk (smear-cursor--roam-route 6 40 i)))
          (setq xs (append xs (mapcar #'car walk)))
          (smear-cursor--roam-advance walk 3)))
      ;; well into the half of his range left of the cursor, and the
      ;; half right of it
      (should (< (apply #'min xs) (* -0.5 span)))
      (should (> (apply #'max xs) (* 0.5 span))))))
(ert-deftest smear-cursor-test-pacman-eats-with-his-mouth-not-his-middle ()
  ;; GIVEN Pacman crossing a row
  ;; WHEN what he has taken is worked out
  ;; THEN the stretch reaches past his middle, by the mouth that closed
  ;;      over it.
  ;;
  ;;      Measured at his middle exactly, a character went only once he
  ;;      was sitting on top of it, so the whole of his open wedge was
  ;;      over text it was not eating and almost none of the mouth did
  ;;      any work.  The other roamers take what their tool covers --
  ;;      the janitor his mop, the Grinch his reach -- and his is the
  ;;      part of the mouth that has closed.
  (should (> smear-cursor--pacman-reach 0.0))
  ;; short of his leading edge, or the band fills the open wedge and
  ;; the mouth reads as a hole rather than as a mouth
  (should (< smear-cursor--pacman-reach 0.5))
  (let* ((lh 18.0)
         (walk '((0.0 . 0.0) (8.0 . 0.0) (16.0 . 0.0)))
         (cells (smear-cursor--pacman-cells lh))
         (px (/ (* 1.5 lh) cells))
         (reach (* px cells smear-cursor--pacman-reach)))
    (smear-cursor--roam-reset)
    (smear-cursor--eaten-remember walk lh reach 8)
    (let ((span (car smear-cursor--eaten-spans)))
      (should span)
      ;; past the furthest place his middle reached
      (should (> (cddr span) 16.0))
      ;; and behind the first, by the same mouth
      (should (< (cadr span) 0.0)))
    (smear-cursor--roam-reset)))
(ert-deftest smear-cursor-test-a-sprite-is-never-drawn-half-there ()
  ;; GIVEN a figure built from blocks that overlap by design
  ;; WHEN the envelope of its layers is read
  ;; THEN it is full for the whole play, fading neither in nor out.
  ;;
  ;;      The blocks are grown half a pixel so their seams are covered
  ;;      twice in the same colour -- see `smear-cursor--sprite-bleed'.
  ;;      At anything short of full opacity, twice is not the same
  ;;      colour: every overlap composites denser than the rest and the
  ;;      figure comes out striped.  So he arrives and leaves whole,
  ;;      and the play that follows takes over from where this one had
  ;;      got to, which is what made a fade look continuous anyway.
  (smear-cursor-test--on-a-roomy-frame
   (dolist (name '(pacman janitor ufo grinch))
     (let ((layers (plist-get (smear-cursor-effect name) :layers)))
       (should layers)
       (dolist (layer layers)
         (when (eq 'sprite (plist-get layer :part))
           (let ((envelope (plist-get layer :envelope)))
             (should envelope)
             (dolist (stop envelope)
               (should (= 1.0 (cdr stop)))))))))))
(defun smear-cursor-test--art-runs (line)
  "Return the runs of drawn cells in LINE as (START . END)."
  (let ((i 0) (n (length line)) out)
    (while (< i n)
      (if (eq ?\s (aref line i))
          (setq i (1+ i))
        (let ((s i))
          (while (and (< i n) (not (eq ?\s (aref line i)))) (setq i (1+ i)))
          (push (cons s i) out))))
    (nreverse out)))

(defun smear-cursor-test--drawn-at (rects row)
  "Return how far the shapes RECTS reach on ROW, as (LEFT . RIGHT) or nil."
  (let (out)
    (dolist (r rects)
      (let* ((col (float (nth 1 r))) (top (nth 2 r)) (w (float (nth 3 r)))
             (h (nth 4 r)) (bl (float (or (nth 5 r) col)))
             (br (float (or (nth 6 r) (+ col w)))))
        (when (and (>= row top) (< row (+ top h)))
          (let* ((f (/ (float (- row top)) h))
                 (l (+ col (* f (- bl col))))
                 (rr (+ (+ col w) (* f (- br (+ col w))))))
            (setq out (cons (min (or (car-safe out) l) l)
                            (max (or (cdr-safe out) rr) rr)))))))
    out))

(ert-deftest smear-cursor-test-the-shapes-keep-to-the-drawing ()
  ;; GIVEN each frame Pacman is drawn from, every heading and mouth
  ;; WHEN the shapes are laid back over the art they came from
  ;; THEN they reach where it does, give or take the step a diagonal
  ;;      is taken in.
  ;;
  ;;      A shape runs down for as long as its sides keep stepping by
  ;;      the same amount, and the first row below set that step
  ;;      whatever it was.  Where the drawing closes up -- the point of
  ;;      the mouth, with two runs meeting one below them -- that first
  ;;      step was half the figure, and the shape splayed across the
  ;;      gap it should have left while the rows under the other side
  ;;      went uncovered.  Facing up, that is a bar sticking out to his
  ;;      right and a strip missing beside it.
  (let ((slack 2.0))
    (dolist (way '(side up))
      (dolist (open '(0.0 0.5 1.0))
        (let* ((art (smear-cursor--pacman-art (smear-cursor--pacman-cells 18.0)
                                              open way))
               (lines (if (stringp art) (split-string art "\n") art))
               (rects (smear-cursor--sprite-rects art)))
          (dotimes (row (length lines))
            (let ((runs (smear-cursor-test--art-runs (nth row lines)))
                  (drawn (smear-cursor-test--drawn-at rects row)))
              (when runs
                (should drawn)
                (should (< (abs (- (car drawn) (float (car (car runs))))) slack))
                (should (< (abs (- (cdr drawn)
                                   (float (cdr (car (last runs))))))
                           slack))))))))))
(ert-deftest smear-cursor-test-a-burst-of-deletions-is-one-fade ()
  ;; GIVEN a deletion marked while its own effect is still playing
  ;; WHEN more text is deleted in the same moment
  ;; THEN nothing further is started until that one has finished.
  ;;
  ;;      Held down, `C-k' fires as fast as the keyboard repeats, and
  ;;      each deletion started a fresh fade over the one still
  ;;      running.  Thirty fades on top of each other are one fade to
  ;;      look at and thirty lots of work to do -- a photograph of the
  ;;      text, a set of layers, a flight to play -- and what the
  ;;      typist notices is the editor, not the effect.
  (let ((played 0) (now 100.0))
    (cl-letf (((symbol-function 'smear-cursor--fire-region-effect)
               (lambda (&rest _) (setq played (1+ played))))
              ((symbol-function 'smear-cursor--own-edit-p) (lambda (&rest _) t))
              ((symbol-function 'float-time) (lambda (&rest _) now)))
      (let ((smear-cursor-mode t)
            (smear-cursor-effects '((delete . region-fade)))
            (smear-cursor-delete-min-chars 1))
        (smear-cursor--delete-forget)
        ;; a burst of thirty inside a sixth of a second, as a held key
        ;; delivers them, and the fade lasts longer than that
        (dotimes (_ 30)
          (smear-cursor--delete-fire 1 40)
          (setq now (+ now 0.005)))
        (should (= 1 played))
        ;; and once it has finished, the next one is marked
        (setq now (+ now 10.0))
        (smear-cursor--delete-fire 1 40)
        (should (= 2 played))))))
(ert-deftest smear-cursor-test-the-photograph-is-let-go-of-early ()
  ;; GIVEN a deletion, played over a photograph of the text
  ;; WHEN the flight is handed to the module
  ;; THEN it is told to hold the photograph for the first frames only.
  ;;
  ;;      The photograph is what lets the flash mark text that has
  ;;      already gone.  Held for the whole flight it also keeps the
  ;;      deleted line on the screen for as long as the effect runs,
  ;;      and what the typist sees is the flash, a wait, and only then
  ;;      the line going.  The edit has to look like it happened when
  ;;      it happened, so the photograph goes early and the rest of the
  ;;      flash plays over the line as it now is.
  (should (> smear-cursor--frozen-share 0.0))
  (should (< smear-cursor--frozen-share 1.0))
  (should (= 1 (smear-cursor--frozen-frames 1)))
  (should (< (smear-cursor--frozen-frames 20) 20))
  (should (>= (smear-cursor--frozen-frames 20) 1))
  ;; and what reaches the module is that count, not a flag
  (let ((told nil))
    (cl-letf (((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage))
              ((symbol-function 'smear-cursor--rect-in-frame) (lambda (_w r) r))
              ((symbol-function 'smear-cursor-x11--freeze) (lambda (&rest _) t))
              ((symbol-function 'smear-cursor-x11--play)
               (lambda (_s _t _l frames _fps &optional frozen _still)
                 (setq told (list (length frames) frozen)) t)))
      (smear-cursor--play-effect (selected-window) 'region-fade
                                 (list (vector 0.0 0.0 80.0 18.0))
                                 smear-cursor--track-occasion t)
      (should (integerp (nth 1 told)))
      (should (< (nth 1 told) (nth 0 told))))))
(ert-deftest smear-cursor-test-pacman-is-drawn-the-size-he-is-asked-for ()
  ;; GIVEN a size in lines and a line height
  ;; WHEN Pacman is drawn
  ;; THEN he comes out that tall, within a pixel.
  ;;
  ;;      A cell is a whole number of pixels, so the size used to be
  ;;      the number of cells rounded, which for a thirty-two cell
  ;;      drawing on an eighteen pixel line was one pixel a cell at
  ;;      every size from 0.8 to 2.5: he was thirty-two pixels tall
  ;;      whatever the setting said.  The grid follows the size
  ;;      instead, so the cell stays a whole pixel and the figure is
  ;;      the height that was asked for.
  (let ((lh 18.0))
    ;; up to the cap; past it he stays at the cap, which is checked below
    (dolist (size '(0.8 1.0 1.2 1.5 1.7))
      (let ((smear-cursor-pacman-size size))
        (let* ((cells (smear-cursor--pacman-cells lh))
               (sprite (progn (smear-cursor--pacman-redraw cells)
                              (smear-cursor-sprite 'pacman)))
               (px (smear-cursor--sprite-pixel sprite size lh))
               (tall (* px (cdr (smear-cursor--sprite-size sprite)))))
          (should (= 1 px))
          (should (< (abs (- tall (* size lh))) 2.0))))))
  ;; and a bigger ask is capped, so he never costs more layers than the
  ;; drawing was budgeted for
  (let ((smear-cursor-pacman-size 8.0))
    (should (= smear-cursor--pacman-most-cells (smear-cursor--pacman-cells 18.0))))
  ;; nor so small that there is no mouth left to see
  (let ((smear-cursor-pacman-size 0.1))
    (should (= smear-cursor--pacman-least-cells (smear-cursor--pacman-cells 18.0)))))

(ert-deftest smear-cursor-test-what-pacman-eats-follows-his-size ()
  ;; GIVEN Pacman drawn at two sizes
  ;; WHEN his reach is worked out
  ;; THEN it is the same share of him either way.
  ;;
  ;;      The reach is the part of the mouth that has shut.  Counted in
  ;;      cells it was a quarter of a thirty-two cell drawing, and on a
  ;;      smaller grid the same count would have been his whole head.
  (should (> smear-cursor--pacman-reach 0.0))
  (should (< smear-cursor--pacman-reach 0.5))
  (let ((lh 18.0))
    (dolist (size '(0.8 1.5))
      (let* ((smear-cursor-pacman-size size)
             (cells (smear-cursor--pacman-cells lh))
             (reach (* 1 cells smear-cursor--pacman-reach)))
        ;; short of his leading edge, or the band fills the open wedge
        (should (< reach (/ cells 2.0)))
        (should (> reach 0.0))))))
(ert-deftest smear-cursor-test-the-ghosts-are-the-size-he-is ()
  ;; GIVEN Pacman drawn smaller
  ;; WHEN the ghosts chasing him are built
  ;; THEN they are drawn smaller with him.
  ;;
  ;;      Their radius was a line height, which is fixed, so shrinking
  ;;      him left them the size they were and the chase became four
  ;;      ghosts running down a crumb.  They are chasing him, so he is
  ;;      what they are measured against.
  (smear-cursor-test--on-a-roomy-frame
   (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 32)))
    (let ((smear-cursor-pacman-ghosts 2))
     (cl-flet ((radius (size)
                 (let ((smear-cursor-pacman-size size))
                   (smear-cursor--roam-reset)
                   (let* ((layers (plist-get (smear-cursor--pacman) :layers))
                          (ghost (cl-find-if
                                  (lambda (l) (and (eq 'ghost (plist-get l :part))
                                                   (plist-get l :radius)))
                                  layers)))
                     (should ghost)
                     (plist-get ghost :radius)))))
       (let ((small (radius 0.8)) (big (radius 1.6)))
         (should (< small big))
         ;; and by about as much as he shrank
         (should (> (/ big small) 1.5))))))))
(ert-deftest smear-cursor-test-he-keeps-eating-with-a-chase-behind-him ()
  ;; GIVEN as many ghosts as anyone would ask for
  ;; WHEN the effect is built
  ;; THEN he is still eating: some stretches are kept back for him.
  ;;
  ;;      Every ghost costs four layers and the stretches he has eaten
  ;;      had whatever was left, so a full chase took the lot and he
  ;;      crossed the text without a mark on it.  Eating is what the
  ;;      effect is; a ghost that will not fit beside it is the one to
  ;;      go without.
  (smear-cursor-test--on-a-roomy-frame
   (let ((smear-cursor-pacman-eat-text t))
     (cl-flet ((bands (ghosts)
                 (let ((smear-cursor-pacman-ghosts ghosts))
                   (smear-cursor--roam-reset)
                   (length (seq-filter
                            (lambda (l) (eq 'eaten (plist-get l :part)))
                            (plist-get (smear-cursor--pacman) :layers))))))
       ;; Not compared with each other: each build takes the next
       ;; seed, so the two routes cross different rows and the count of
       ;; stretches is the route's, not the budget's.
       (let ((alone (bands 0)) (chased (bands 4)))
         (should (> alone 0))
         (should (> chased 0)))
       (smear-cursor--roam-reset)))))
(ert-deftest smear-cursor-test-there-are-only-so-many-ghosts ()
  ;; GIVEN more ghosts asked for than there are
  ;; WHEN Pacman is built
  ;; THEN he is chased by the cap, not by the number asked for.
  ;;
  ;;      Four is the arcade's whole set.  Past that they are drawn on
  ;;      top of each other and each one still costs four of the
  ;;      layers the eating wants, so the setting is bounded where the
  ;;      chase stops being a chase.
  (should (= 3 smear-cursor--pacman-most-ghosts))
  (smear-cursor-test--on-a-roomy-frame
   (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 64)))
     (cl-flet ((eyes (asked)
                 (let ((smear-cursor-pacman-ghosts asked))
                   (smear-cursor--roam-reset)
                   (seq-count (lambda (l) (eq 'ghost (plist-get l :part)))
                              (plist-get (smear-cursor--pacman) :layers)))))
       (should (= (eyes smear-cursor--pacman-most-ghosts) (eyes 99)))
       (should (< (eyes 1) (eyes smear-cursor--pacman-most-ghosts)))))
   (smear-cursor--roam-reset)))
(ert-deftest smear-cursor-test-the-chase-does-not-blink ()
  ;; GIVEN ghosts chasing him play after play
  ;; WHEN the envelope of each part of the chase is read
  ;; THEN it is full throughout: they arrive and leave whole.
  ;;
  ;;      A ghost is a dome, a body and two eyes drawn over each other,
  ;;      so at anything short of full opacity the overlaps composite
  ;;      denser than the rest, as a sprite's blocks do.  Worse, the
  ;;      plays run back to back: an envelope that fades in and out is
  ;;      a chase that fades away and comes back every few seconds.
  ;;      The pellets are left alone, because blinking is what a pellet
  ;;      does.
  (smear-cursor-test--on-a-roomy-frame
   ;; a plain play: on a blue one a ghost he catches is eaten, and its
   ;; envelope ends early on purpose
   (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 64))
             ((symbol-function 'smear-cursor--pacman-powered-p) (lambda () nil)))
     (let ((smear-cursor-pacman-ghosts smear-cursor--pacman-most-ghosts))
       (smear-cursor--roam-reset)
       ;; the second play: by then there is a tail behind him, which is
       ;; the state a spell of them spends nearly all its time in
       (smear-cursor--pacman)
       (let ((seen 0))
         (dolist (layer (plist-get (smear-cursor--pacman) :layers))
           (when (memq (plist-get layer :part) '(ghost ghost-eye))
             (setq seen (1+ seen))
             (dolist (stop (plist-get layer :envelope))
               (should (= 1.0 (cdr stop))))))
         (should (> seen 0))))
     (smear-cursor--roam-reset))))
(ert-deftest smear-cursor-test-a-power-pellet-turns-the-chase-blue ()
  ;; GIVEN a power pellet every so many plays
  ;; WHEN the plays come round
  ;; THEN the chase is blue on those and its own colours on the rest.
  ;;
  ;;      He runs into them often, because they follow the route he
  ;;      has just walked and he doubles back along it.  Ending the
  ;;      play at every meeting would end most plays, so the arcade's
  ;;      other rule is the one worth having: eat the pellet and the
  ;;      ghosts are yours for a while.
  (should (> smear-cursor-pacman-power 0))
  (let ((smear-cursor--pacman-run 0))
    (should (smear-cursor--pacman-powered-p))
    (setq smear-cursor--pacman-run 1)
    (should-not (smear-cursor--pacman-powered-p)))
  ;; and off is off
  (let ((smear-cursor-pacman-power 0)
        (smear-cursor--pacman-run 0))
    (should-not (smear-cursor--pacman-powered-p))))

(ert-deftest smear-cursor-test-a-blue-ghost-is-eaten-when-he-catches-it ()
  ;; GIVEN a ghost on a route that doubles back through it
  ;; WHEN he meets it while it is blue
  ;; THEN it stops being drawn from that moment, and while it is not
  ;;      blue it carries on regardless.
  (let* ((out (cl-loop for i below 30 collect (cons (* i 4.0) 0.0)))
         (back (cl-loop for i below 30 collect (cons (- 116.0 (* i 4.0)) 0.0)))
         (walk (append out back))
         (smear-cursor--roam-tail nil))
    (let* ((blue (smear-cursor--pacman-ghost 0 "#ff4d4d" walk 9.0 t))
           (env (plist-get (car blue) :envelope))
           (last (car (last env))))
      ;; it goes, and stays gone
      (should (= 0.0 (cdr last)))
      (should (< (car (nth (- (length env) 2) env)) 1.0)))
    (let* ((plain (smear-cursor--pacman-ghost 0 "#ff4d4d" walk 9.0 nil))
           (env (plist-get (car plain) :envelope)))
      (should (= 1.0 (cdr (car (last env))))))))
(ert-deftest smear-cursor-test-he-shrugs-a-ghost-off ()
  ;; GIVEN a route that doubles back through a ghost, and no pellet
  ;; WHEN they meet
  ;; THEN there is a flash where they touched, and he carries on.
  ;;
  ;;      They follow the route he has just walked, so meeting one is
  ;;      the queue behind him rather than a chase he is losing, and
  ;;      the arcade's answer of dying would end most plays.  He shrugs
  ;;      it off instead, and the flash is what says so.
  (let* ((out (cl-loop for i below 30 collect (cons (* i 4.0) 0.0)))
         (back (cl-loop for i below 30 collect (cons (- 116.0 (* i 4.0)) 0.0)))
         (walk (append out back))
         (trail (last walk 40))
         (r 9.0)
         (at (smear-cursor--pacman-caught walk trail r)))
    (should at)
    ;; not the first frame: they start together, because the trail
    ;; falls back to where he is standing until there is a tail behind
    ;; him, and a flash on frame one is a flash nobody sees
    (should (> at 0))
    (let ((flash (smear-cursor--pacman-clash walk at r)))
      (should flash)
      (should (eq 'clash (plist-get flash :part)))
      ;; nothing before the meeting, everything at it, nothing after
      (let* ((env (plist-get flash :envelope))
             (peak (car (cl-find-if (lambda (s) (= 1.0 (cdr s))) env)))
             (want (/ (float at) (1- (length walk)))))
        (should (< (abs (- peak want)) 0.05))
        (should (= 0.0 (cdr (car env))))
        (should (= 0.0 (cdr (car (last env)))))))
    ;; and no meeting is no flash
    (should-not (smear-cursor--pacman-clash walk nil r))))

(ert-deftest smear-cursor-test-a-blue-ghost-is-eaten-rather-than-shrugged-off ()
  ;; GIVEN a play he has the run of them in
  ;; WHEN he catches one
  ;; THEN it is eaten and there is no flash: the flash is what happens
  ;;      instead of eating one.
  (smear-cursor-test--on-a-roomy-frame
   (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 64)))
     (let ((smear-cursor-pacman-ghosts 3))
       (cl-flet ((clashes (powered)
                   (cl-letf (((symbol-function 'smear-cursor--pacman-powered-p)
                              (lambda () powered)))
                     (smear-cursor--roam-reset)
                     (smear-cursor--pacman)
                     (seq-count (lambda (l) (eq 'clash (plist-get l :part)))
                                (plist-get (smear-cursor--pacman) :layers)))))
         (should (= 0 (clashes t)))
         (should (<= (clashes nil) 1))))
     (smear-cursor--roam-reset))))
(ert-deftest smear-cursor-test-a-meeting-is-a-crossing-not-a-standing-together ()
  ;; GIVEN a ghost already touching him and never leaving
  ;; WHEN a meeting is looked for
  ;; THEN there is none: they have to come together to have met.
  ;;
  ;;      Until there is a tail behind him the trail they chase falls
  ;;      back to where he is standing, so the pair start on top of
  ;;      each other.  Read as a state rather than as a crossing, that
  ;;      is a meeting on the first frame of every play, at the place
  ;;      he set off from, gone before anything moved.
  (let ((walk (cl-loop for i below 20 collect (cons (* i 4.0) 0.0))))
    (should-not (smear-cursor--pacman-caught walk walk 9.0))
    ;; and one that comes in from outside is met where it arrives
    (let ((comes (cl-loop for i below 20
                          collect (cons (+ 200.0 (* i -10.0)) 0.0))))
      (let ((at (smear-cursor--pacman-caught walk comes 9.0)))
        (should at)
        (should (> at 0))))))
(ert-deftest smear-cursor-test-a-frightened-ghost-drops-back ()
  ;; GIVEN a play he has the run of them in
  ;; WHEN a ghost is followed through it
  ;; THEN it falls further behind as the play goes on, where a chasing
  ;;      one keeps its distance.
  ;;
  ;;      The arcade slows them while they are blue.  They cannot flee
  ;;      here, because what they follow is the route he has already
  ;;      walked, but they can lose ground on it, which is the same
  ;;      thing to look at.
  (let* ((walk (cl-loop for i below 40 collect (cons (* i 10.0) 0.0)))
         (gap (lambda (trail at)
                (abs (- (car (nth at walk)) (car (nth at trail))))))
         (chasing (smear-cursor--roam-trail walk 5))
         (fleeing (smear-cursor--roam-trail walk 5 nil 10)))
    ;; the chase holds its distance once it is under way
    (should (= (funcall gap chasing 20) (funcall gap chasing 39)))
    ;; the blue one keeps losing ground
    (should (> (funcall gap fleeing 39) (funcall gap fleeing 20)))
    (should (> (funcall gap fleeing 39) (funcall gap chasing 39)))))
(ert-deftest smear-cursor-test-the-flash-is-on-top-of-the-pair-of-them ()
  ;; GIVEN a meeting between him and a ghost
  ;; WHEN the layers are ordered
  ;; THEN the flash is the last of them.
  ;;
  ;;      Layers draw in order and the last is on top.  Put with the
  ;;      bands, the flash went under the ghost and under him: it
  ;;      reached the screen -- a probe measured it changing pixels --
  ;;      and it was covered by the very two things whose meeting it
  ;;      was there to mark.
  (smear-cursor-test--on-a-roomy-frame
   (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 64))
             ((symbol-function 'smear-cursor--pacman-caught)
              (lambda (walk &rest _) (/ (length walk) 2)))
             ((symbol-function 'smear-cursor--pacman-powered-p) (lambda () nil)))
     (let* ((smear-cursor-pacman-ghosts 3)
            (layers (progn (smear-cursor--roam-reset) (smear-cursor--pacman)))
            (parts (mapcar (lambda (l) (plist-get l :part))
                           (plist-get layers :layers))))
       (should (memq 'clash parts))
       (should (eq 'clash (car (last parts))))))
   (smear-cursor--roam-reset)))
(ert-deftest smear-cursor-test-a-ghost-can-be-the-end-of-him ()
  ;; GIVEN a meeting on a play he has no power in
  ;; WHEN he is not immune
  ;; THEN he goes out where they touched, and the chase carries on
  ;;      without him.
  ;;
  ;;      The arcade's rule.  It costs no layers, because going out is
  ;;      an envelope that ends early on the layers he is already
  ;;      drawn from.
  (smear-cursor-test--on-a-roomy-frame
   (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 64))
             ((symbol-function 'smear-cursor--pacman-caught)
              (lambda (walk &rest _) (round (* 0.4 (length walk)))))
             ((symbol-function 'smear-cursor--pacman-powered-p) (lambda () nil)))
     (let ((smear-cursor-pacman-ghosts 3))
       (cl-flet ((ends-early (immune)
                   (let ((smear-cursor-pacman-immune immune))
                     (smear-cursor--roam-reset)
                     (let ((his (seq-filter
                                 (lambda (l) (eq 'sprite (plist-get l :part)))
                                 (plist-get (smear-cursor--pacman) :layers))))
                       (should his)
                       (cl-every (lambda (l)
                                   (= 0.0 (cdr (car (last (plist-get l :envelope))))))
                                 his)))))
         (should (ends-early nil))
         (should-not (ends-early t)))))
   (smear-cursor--roam-reset)))
(ert-deftest smear-cursor-test-he-can-read-what-he-is-eating ()
  ;; GIVEN a word he cannot let pass on the screen near him
  ;; WHEN the play is built
  ;; THEN he goes for it: red, and taking bigger bites.
  ;;
  ;;      Found by searching the text on screen rather than by reading
  ;;      the route a pixel at a time.  A route covers a few dozen
  ;;      columns of a few rows and a word is three characters of one
  ;;      of them, so samples along the way went over the top of it
  ;;      every time; and a search is one pass of the text against one
  ;;      regexp, where a sample is a question for the display.
  (smear-cursor-test--on-a-roomy-frame
   (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 64))
             ((symbol-function 'smear-cursor--pacman-powered-p) (lambda () nil)))
     (cl-flet ((his-colour (text)
                 (let ((buf (get-buffer-create " *smear-word*")))
                   (with-current-buffer buf (erase-buffer) (insert text))
                   (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) buf))
                             ((symbol-function 'window-start) (lambda (&rest _) 1))
                             ((symbol-function 'window-end)
                              (lambda (&rest _) (1+ (length text))))
                             ((symbol-function 'smear-cursor--pos-rect)
                              (lambda (&rest _) (vector 210.0 200.0 10.0 21.0))))
                     (smear-cursor--roam-reset)
                     (plist-get (cl-find-if (lambda (l) (eq 'sprite (plist-get l :part)))
                                            (plist-get (smear-cursor--pacman) :layers))
                                :color)))))
       (let ((smear-cursor-pacman-berserk '("vim")))
         (should (equal smear-cursor--pacman-berserk-color (his-colour "a vim here")))
         ;; whatever case it is written in
         (should (equal smear-cursor--pacman-berserk-color (his-colour "a VIM here")))
         ;; whole words only: this is not one
         (should-not (equal smear-cursor--pacman-berserk-color
                            (his-colour "a vimrc here")))
         (should-not (equal smear-cursor--pacman-berserk-color
                            (his-colour "an emacs here"))))
       ;; and nothing set means nothing to look for
       (let ((smear-cursor-pacman-berserk nil))
         (should-not (equal smear-cursor--pacman-berserk-color
                            (his-colour "a vim here"))))))
   (smear-cursor--roam-reset)))

(ert-deftest smear-cursor-test-berserk-carries-him-through-a-ghost ()
  ;; GIVEN a meeting with a ghost on a play he has gone berserk in
  ;; WHEN he is not otherwise immune
  ;; THEN he goes through it: the word is what he is here for.
  (smear-cursor-test--on-a-roomy-frame
   (cl-letf (((symbol-function 'smear-cursor--max-layers) (lambda () 64))
             ((symbol-function 'smear-cursor--pacman-powered-p) (lambda () nil))
             ((symbol-function 'smear-cursor--pacman-caught)
              (lambda (walk &rest _) (round (* 0.5 (length walk))))))
     (let ((smear-cursor-pacman-immune nil)
           (smear-cursor-pacman-ghosts 3))
       (cl-flet ((survives (seen)
                   (cl-letf (((symbol-function 'smear-cursor--pacman-berserk-at)
                              (lambda (&rest _) seen)))
                     (smear-cursor--roam-reset)
                     (let ((his (cl-find-if (lambda (l) (eq 'sprite (plist-get l :part)))
                                            (plist-get (smear-cursor--pacman) :layers))))
                       (= 1.0 (cdr (car (last (plist-get his :envelope)))))))))
         (should (survives '(:at 4 :place (40.0 . 0.0) :wide 30.0)))
         (should-not (survives nil)))))
   (smear-cursor--roam-reset)))
(ert-deftest smear-cursor-test-he-turns-round-and-stares ()
  ;; GIVEN a word he has gone for
  ;; WHEN he lands on it
  ;; THEN a pair of eyes open on him, and not before.
  ;;
  ;;      He is a drawing of a mouth in profile, so facing out is a
  ;;      pair of eyes rather than another set of frames: two layers
  ;;      against nineteen, and they say the same thing.
  (let* ((walk (cl-loop for i below 40 collect (cons (* i 5.0) 0.0)))
         (eyes (smear-cursor--pacman-stare walk 30 (cons 200.0 0.0) 9.0)))
    (should (= 2 (length eyes)))
    (dolist (eye eyes)
      (should (eq 'pacman-eye (plist-get eye :part)))
      ;; shut until he arrives, open after
      (let ((env (plist-get eye :envelope)))
        (should (= 0.0 (cdr (car env))))
        (should (= 1.0 (cdr (car (last env)))))
        (should (> (car (cl-find-if (lambda (s) (= 1.0 (cdr s))) env)) 0.5)))
      ;; on his face, either side of the middle
      (should (= 0.0 (cdr (plist-get eye :offset)))))
    (should (< (car (plist-get (car eyes) :offset))
               (car (plist-get (cadr eyes) :offset))))))
(ert-deftest smear-cursor-test-the-word-has-to-be-near-him ()
  ;; GIVEN a word worth minding far up the screen
  ;; WHEN he looks for one
  ;; THEN it does not count: what he cannot reach he cannot go for.
  ;;
  ;;      Nearness is measured against where he is, read once before
  ;;      the search: the search moves point, and the point of the
  ;;      selected window is the buffer's, so read inside the loop it
  ;;      followed each match and every one came out near.
  (with-temp-buffer
    (dotimes (_ 40) (insert "nothing to see here\n"))
    (let ((far (point))
          ;; standing where the cursor is, rather than wherever the
          ;; last test left him: nearness is measured from him
          (smear-cursor--roam-from '(0.0 . 0.0)))
      (insert "vim\n")
      (dotimes (_ 40) (insert "nothing to see here\n"))
      (goto-char (point-min))
      (should-not (smear-cursor--pacman-near-p (point) far 6 40))
      ;; and one on the next line over does
      (goto-char far)
      (forward-line -2)
      (should (smear-cursor--pacman-near-p (point) far 6 40)))))
(ert-deftest smear-cursor-test-the-chase-scatters-when-he-charges ()
  ;; GIVEN a full chase and a word he has gone for
  ;; WHEN the layers are handed out
  ;; THEN he has his eyes and the ghosts are nowhere to be seen.
  ;;
  ;;      Their dozen layers go to the eating and to the pair of eyes,
  ;;      which have to come from somewhere: the eating has a floor and
  ;;      the figure is the effect.  A chase and a charge at once is
  ;;      two things happening where there is room for one.
  (smear-cursor-test--on-a-roomy-frame
   ;; the budget a real module gives, not the sixteen the harness
   ;; stubs: with sixteen there is no room for a chase at all
   (cl-letf (((symbol-function 'smear-cursor--pacman-powered-p) (lambda () nil))
             ((symbol-function 'smear-cursor--max-layers) (lambda () 32)))
     (let ((smear-cursor-pacman-ghosts smear-cursor--pacman-most-ghosts))
       (cl-flet ((parts (seen part)
                   (cl-letf (((symbol-function 'smear-cursor--pacman-berserk-at)
                              (lambda (&rest _) seen)))
                     (smear-cursor--roam-reset)
                     (seq-count (lambda (l) (eq part (plist-get l :part)))
                                (plist-get (smear-cursor--pacman) :layers)))))
         (let ((word '(:at 4 :place (40.0 . -21.0) :wide 30.0)))
           (should (= 2 (parts word 'pacman-eye)))
           (should (= 0 (parts nil 'pacman-eye)))
           (should (= 0 (parts word 'ghost)))
           (should (> (parts nil 'ghost) 0))
           ;; and the eating has the rest of what they were using
           (should (> (parts word 'eaten) 0))))))
   (smear-cursor--roam-reset)))
(ert-deftest smear-cursor-test-a-word-is-worth-going-for-once ()
  ;; GIVEN a word that stays on the screen, as words do
  ;; WHEN play follows play
  ;; THEN he goes for it once and then leaves it alone for a while.
  ;;
  ;;      He covers the text he eats rather than changing it, so the
  ;;      word is still there next time he looks.  Nothing to stop him
  ;;      and he found it again every play: red for ever, which is not
  ;;      going berserk, it is a colour scheme.
  (cl-letf (((symbol-function 'smear-cursor--pacman-word-place)
             (lambda (&rest _) (list (cons 40.0 -21.0) 30.0))))
    (let ((walk (cl-loop for i below 30 collect (cons (* i 5.0) 0.0))))
      (smear-cursor--pacman-berserk-forget)
      (should (smear-cursor--pacman-berserk-at nil nil walk))
      ;; and not again for a few plays
      (dotimes (_ smear-cursor--pacman-berserk-rest)
        (should-not (smear-cursor--pacman-berserk-at nil nil walk)))
      ;; then he is ready to be set off again
      (should (smear-cursor--pacman-berserk-at nil nil walk))))
  (smear-cursor--pacman-berserk-forget))
(ert-deftest smear-cursor-test-he-minds-what-is-near-him-not-near-point ()
  ;; GIVEN a word out of reach of the cursor but beside where he has
  ;;       wandered to
  ;; WHEN he looks for one
  ;; THEN it counts: he is the one doing the eating.
  ;;
  ;;      A spell of plays carries him a long way from the cursor --
  ;;      that is the whole of what roaming is -- so a word measured
  ;;      against point was out of reach exactly when he was standing
  ;;      next to it.
  (with-temp-buffer
    (insert (make-string 200 ?x) "\n")
    (goto-char (point-min))
    (let ((here (point))
          (far (+ (point-min) 60)))
      (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 10))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 20)))
        ;; standing at the cursor, sixty columns is too far
        (let ((smear-cursor--roam-from '(0.0 . 0.0)))
          (should-not (smear-cursor--pacman-near-p here far 6 40)))
        ;; having wandered fifty columns that way, it is not
        (let ((smear-cursor--roam-from '(500.0 . 0.0)))
          (should (smear-cursor--pacman-near-p here far 6 40)))))))
(ert-deftest smear-cursor-test-he-eats-the-whole-word-then-jumps-on-it ()
  ;; GIVEN a word he has gone for, and its width
  ;; WHEN the route is rebuilt around it
  ;; THEN he runs at it, crosses the whole of it, and jumps on the spot
  ;;      where it was until the play runs out.
  ;;
  ;;      Landing on the first letter and sitting there ate two
  ;;      characters of it: his mouth is about that wide, so eating a
  ;;      word means crossing it.  The jumps are what says he meant it.
  (let* ((walk (cl-loop for i below 60 collect (cons (* i 5.0) 0.0)))
         (word (cons 400.0 -20.0))
         (wide 30.0)
         (charged (smear-cursor--pacman-charge walk 20 word wide)))

    (should (= (length walk) (length charged)))
    (should (equal (seq-take walk 20) (seq-take charged 20)))
    (let* ((after (nthcdr 20 charged))
           (xs (mapcar #'car after))
           (ys (mapcar #'cdr after)))
      ;; he arrives at the word -- the places before that are the run
      ;; across the ground between -- and goes on to the far end of it
      (should (cl-some (lambda (x) (< (abs (- x (car word))) 1.0)) xs))
      (should (>= (apply #'max xs) (+ (car word) wide -1.0)))
      ;; the run at it is faster than the wander it left off
      (should (> (smear-cursor--pacman-pace (seq-take after 6))
                 (smear-cursor--pacman-pace walk)))
      ;; and he finishes bobbing over it rather than wandering off
      (should (> (length (delete-dups (copy-sequence ys))) 2))
      (let ((last-xs (mapcar #'car (last after 8))))
        (should (< (- (apply #'max last-xs) (apply #'min last-xs)) (1+ wide)))))))

(ert-deftest smear-cursor-test-the-word-he-minds-is-marked ()
  ;; GIVEN a word he has gone for
  ;; WHEN the layers are built
  ;; THEN it is marked from the moment he notices until he has had it.
  (let* ((walk (cl-loop for i below 60 collect (cons (* i 5.0) 0.0)))
         (mark (smear-cursor--pacman-mark walk 20 (cons 400.0 -20.0) 30.0 21.0)))
    (should (eq 'word (plist-get mark :part)))
    (let ((q (aref (plist-get mark :quads) 0)))
      ;; a box the width of the word, a line high
      (should (< (abs (- (- (aref q 2) (aref q 0)) 30.0)) 1.0))
      (should (< (abs (- (- (aref q 5) (aref q 1)) 21.0)) 1.0)))
    (let ((env (plist-get mark :envelope)))
      (should (= 0.0 (cdr (car env))))
      (should (= 0.0 (cdr (car (last env)))))
      (should (cl-some (lambda (s) (> (cdr s) 0.0)) env)))))
(ert-deftest smear-cursor-test-he-gets-there-however-far-it-is ()
  ;; GIVEN a word further off than his usual pace covers in what is
  ;;       left of the play
  ;; WHEN he goes for it
  ;; THEN he still arrives, crosses it and jumps: he runs at whatever
  ;;      pace the distance and the time between them ask for.
  ;;
  ;;      Built at a set pace and then cut to the places left, the run
  ;;      simply stopped partway and he stood there: no crossing, no
  ;;      jumping, and a word left uneaten a few characters away.
  (let* ((walk (cl-loop for i below 30 collect (cons (* i 2.0) 0.0)))
         (word (cons 900.0 -40.0))
         (wide 30.0)
         (charged (smear-cursor--pacman-charge walk 20 word wide 20.0))
         (after (nthcdr 20 charged))
         (xs (mapcar #'car after)))
    (should (= (length walk) (length charged)))
    ;; he reaches it and crosses it
    (should (>= (apply #'max xs) (+ (car word) wide -1.0)))
    ;; and there is something left over to jump with
    (let ((ys (mapcar #'cdr (last after 3))))
      (should (> (length (delete-dups (copy-sequence ys))) 1)))))
(ert-deftest smear-cursor-test-an-iconified-popup-is-not-on-screen ()
  ;; GIVEN a completion popup's child frame, put away by iconifying it
  ;; WHEN the effects ask whether one is showing
  ;; THEN it is not: an icon is not a popup over the text.
  ;;
  ;;      `frame-visible-p' answers t, nil, or the symbol `icon', and
  ;;      the last of those is truthy.  Corfu keeps one child frame and
  ;;      iconifies it rather than making it invisible, so from the
  ;;      first completion of a session onwards every effect that marks
  ;;      an edit stood down: no flash on a kill, none on a yank, none
  ;;      on a keystroke.
  (cl-letf (((symbol-function 'frame-list) (lambda () '(popup)))
            ((symbol-function 'frame-parent) (lambda (_f) 'parent)))
    (cl-letf (((symbol-function 'frame-visible-p) (lambda (_f) 'icon)))
      (should-not (smear-cursor--child-frame-showing-p)))
    (cl-letf (((symbol-function 'frame-visible-p) (lambda (_f) nil)))
      (should-not (smear-cursor--child-frame-showing-p)))
    ;; and one that really is up still counts
    (cl-letf (((symbol-function 'frame-visible-p) (lambda (_f) t)))
      (should (smear-cursor--child-frame-showing-p)))))
(ert-deftest smear-cursor-test-a-whole-line-stops-at-the-end-of-it ()
  ;; GIVEN a region that runs from a line's start to the next line's
  ;; WHEN the text it covers is worked out
  ;; THEN it ends with the newline, which is drawn at the end of the
  ;;      line above, rather than at the first character of the line
  ;;      below.
  ;;
  ;;      `kill-whole-line' and the slick-cut idiom both hand over a
  ;;      region like that, and taken at face value the flash covered
  ;;      the line and one character of the next, which reads as a
  ;;      mistake because it is one.
  (with-temp-buffer
    (insert "first line\nsecond line\nthird line\n")
    (goto-char (point-min))
    (let ((beg (line-beginning-position))
          (end (line-beginning-position 2)))
      (should (= (1- end) (smear-cursor--region-last beg end)))
      ;; a region ending mid-line is left alone
      (should (= (+ beg 5) (smear-cursor--region-last beg (+ beg 5))))
      ;; and so is an empty one
      (should (= beg (smear-cursor--region-last beg beg)))))
  ;; the rectangles that come of it keep to the one line
  (let ((rows nil))
    (cl-letf (((symbol-function 'smear-cursor--pos-rect)
               (lambda (_w pos) (vector 0.0 (* 20.0 (/ pos 12)) 8.0 20.0)))
              ((symbol-function 'window-text-width) (lambda (&rest _) 800)))
      (with-temp-buffer
        (insert "first line\nsecond line\n")
        (goto-char (point-min))
        (setq rows (smear-cursor--region-rects
                    (selected-window) (point-min) (line-beginning-position 2))))
      (should rows)
      (should (= 1 (length (delete-dups (mapcar (lambda (r) (aref r 1)) rows))))))))
(ert-deftest smear-cursor-test-a-selection-is-not-a-reason-to-stand-down ()
  ;; GIVEN a region active because a motion made one
  ;; WHEN the cursor moves
  ;; THEN the trail is drawn.
  ;;
  ;;      Modal editing makes a selection out of every motion -- that
  ;;      is what meow's `meow-next-word' is -- so a rule that stands
  ;;      down whenever a region is active turns the package off for
  ;;      anyone editing that way, and says nothing about it.  A mouse
  ;;      drag is still a drag: the pointer is doing the moving and the
  ;;      trail would follow it about.
  (should smear-cursor-while-selecting)
  (cl-letf (((symbol-function 'region-active-p) (lambda () t))
            ((symbol-function 'smear-cursor--tracking-p) (lambda () nil)))
    (should-not (smear-cursor--selecting-p))
    ;; and turning it off is still turning it off
    (let ((smear-cursor-while-selecting nil))
      (should (smear-cursor--selecting-p))))
  ;; the mouse is another matter either way
  (cl-letf (((symbol-function 'region-active-p) (lambda () nil))
            ((symbol-function 'smear-cursor--tracking-p) (lambda () t)))
    (should (smear-cursor--selecting-p))))
(ert-deftest smear-cursor-test-the-cursor-can-glow-at-rest ()
  ;; GIVEN a resting effect asked for
  ;; WHEN the cursor is sitting still
  ;; THEN it plays at the cursor, on the trail's own track, over and
  ;;      over.
  ;;
  ;;      On a track of its own, because what the cursor wears never
  ;;      stops and the trail comes and goes over the top of it.
  (let ((played nil))
    (cl-letf (((symbol-function 'smear-cursor--play-effect)
               (lambda (_win name _rects track &rest _)
                 (setq played (cons name track))))
              ((symbol-function 'smear-cursor--point-rect)
               (lambda (_w) (vector 10.0 20.0 8.0 18.0)))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (let ((smear-cursor-mode t)
            (smear-cursor-rest-effect 'cursor-rest))
        (smear-cursor--rest-play)
        (should (eq 'cursor-rest (car played)))
        (should (= smear-cursor--track-rest (cdr played))))
      ;; and nothing at all when it is not asked for
      (setq played nil)
      (let ((smear-cursor-mode t)
            (smear-cursor-rest-effect nil))
        (smear-cursor--rest-play)
        (should-not played)))))

(ert-deftest smear-cursor-test-the-resting-glow-is-one-still-picture ()
  ;; GIVEN the resting effect
  ;; WHEN it is built
  ;; THEN it holds still, so the module can draw it once and stamp it
  ;;      again at each opacity.
  ;;
  ;;      It plays for as long as Emacs is open.  Rendering it afresh
  ;;      every frame is the one thing that cannot be afforded on a
  ;;      display reached over a network.
  (let ((effect (smear-cursor-effect 'cursor-rest)))
    (should effect)
    (should (smear-cursor--effect-still-p effect))
    (should (eq 'point (plist-get effect :shape)))))
(ert-deftest smear-cursor-test-the-resting-glow-has-a-size ()
  ;; GIVEN the resting glow
  ;; WHEN its layer is read
  ;; THEN it has a radius, which is how a radial is measured.
  ;;
  ;;      Given `:grow', which is how far a quad spreads out from the
  ;;      rectangle it is handed, the glow was a radial with no size at
  ;;      all: a probe of the real effect measured it changing four
  ;;      pixels.
  (let* ((effect (smear-cursor-effect 'cursor-rest))
         (layer (car (plist-get effect :layers))))
    (should (eq 'radial (plist-get layer :shape)))
    (should (> (or (plist-get layer :radius) 0) (/ (frame-char-height) 2.0)))
    (should-not (plist-get layer :grow))))
(ert-deftest smear-cursor-test-the-resting-glow-wears-the-trail-style ()
  ;; GIVEN a trail style with a head of its own
  ;; WHEN the resting glow is built
  ;; THEN it is that head: the same rings, in the same colours.
  ;;
  ;;      The laser style is a hot near-white dot inside a red bloom,
  ;;      and a glow invented separately is a different thing that
  ;;      happens to sit in the same place.  Whatever style is in use
  ;;      is what the cursor should be wearing at rest.
  (let ((smear-cursor-trail-style 'laser))
    (let* ((effect (smear-cursor-effect 'cursor-rest))
           (layers (plist-get effect :layers))
           (style (gethash 'laser smear-cursor--trails)))
      (should (= (length layers)
                 (seq-count (lambda (l) (eq 'radial (plist-get l :shape)))
                            (plist-get style :layers))))
      ;; the bright centre keeps its own colour
      (should (cl-some (lambda (l) (plist-get l :color)) layers))
      ;; every ring is a radial with a radius, and none has grown
      (dolist (l layers)
        (should (eq 'radial (plist-get l :shape)))
        (should (> (plist-get l :radius) 0)))
      ;; and it still holds still, which is what makes it affordable
      (should (smear-cursor--effect-still-p effect))))
  ;; a style with no head of its own still gets a glow
  (let ((smear-cursor-trail-style 'plain))
    (should (plist-get (smear-cursor-effect 'cursor-rest) :layers))))
(ert-deftest smear-cursor-test-the-resting-glow-keeps-up-with-the-cursor ()
  ;; GIVEN the glow up at one place
  ;; WHEN the cursor moves
  ;; THEN it is played again at the new one, on a track of its own.
  ;;
  ;;      On the trail's track it could only be aimed again between
  ;;      flights, so a movement left it behind for the length of a
  ;;      turn -- over a second of the cursor and its glow in different
  ;;      places, which is worse than no glow.
  (let ((played nil) (rect (vector 10.0 20.0 8.0 18.0)))
    (cl-letf (((symbol-function 'smear-cursor--play-effect)
               (lambda (_win _name rects track &rest _)
                 (setq played (cons (car rects) track))))
              ((symbol-function 'smear-cursor--point-rect) (lambda (_w) rect))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (let ((smear-cursor-mode t)
            (smear-cursor-rest-effect 'cursor-rest))
        (smear-cursor--rest-forget)
        (smear-cursor--rest-follow)
        (should (equal rect (car played)))
        (should (= smear-cursor--track-rest (cdr played)))
        (should-not (= smear-cursor--track-rest smear-cursor--track-trail))
        ;; standing still asks for nothing further
        (setq played nil)
        (smear-cursor--rest-follow)
        (should-not played)
        ;; and moving asks again
        (setq rect (vector 90.0 20.0 8.0 18.0))
        (cl-letf (((symbol-function 'window-point) (lambda (&rest _) 42)))
          (smear-cursor--rest-follow))
        (should (equal rect (car played)))))))
(ert-deftest smear-cursor-test-the-resting-glow-is-as-strong-as-the-trail ()
  ;; GIVEN the resting glow left at its shipped strength
  ;; WHEN its rings are compared with the trail style's own
  ;; THEN they are the same: the cursor wears what the trail is made
  ;;      of, at the strength the trail is made at.
  ;;
  ;;      Scaled down it read as a dimmer relative of the trail rather
  ;;      than as the trail standing still.
  (should (= 1.0 smear-cursor-rest-strength))
  (let ((smear-cursor-trail-style 'laser))
    (let ((worn (plist-get (smear-cursor-effect 'cursor-rest) :layers))
          (style (seq-filter (lambda (l) (and (eq 'radial (plist-get l :shape))
                                              (plist-get l :radius)))
                             (plist-get (gethash 'laser smear-cursor--trails)
                                        :layers))))
      (should (= (length worn) (length style)))
      (cl-loop for a in worn for b in style do
               (should (= (plist-get a :alpha) (plist-get b :alpha)))
               (should (= (plist-get a :radius) (plist-get b :radius)))))))
(ert-deftest smear-cursor-test-a-trail-gets-the-overlay-to-itself ()
  ;; GIVEN the resting glow up
  ;; WHEN a trail starts
  ;; THEN the glow's flight is stopped first.
  ;;
  ;;      The overlay is shaped, mapped, and taken down between
  ;;      flights, and the taking down is what lets the next flight map
  ;;      it with a shape a compositor will latch on to.  A flight that
  ;;      never ends holds it up, and after that the trails were drawn
  ;;      and never presented.  So the glow gets out of the way when
  ;;      something else has somewhere to be, and its timer brings it
  ;;      back a turn later.
  (let ((stopped nil))
    (cl-letf (((symbol-function 'smear-cursor-x11--play-stop-track)
               (lambda (_stage track) (push track stopped)))
              ((symbol-function 'smear-cursor--effect-stage) (lambda (_w) 'stage)))
      (let ((smear-cursor-mode t)
            (smear-cursor-rest-effect 'cursor-rest)
            (smear-cursor--rest-at (vector 0.0 0.0 8.0 18.0)))
        (smear-cursor--rest-yield (selected-window))
        (should (memq smear-cursor--track-rest stopped))
        (should-not smear-cursor--rest-at))
      ;; nothing to do when no glow was asked for
      (setq stopped nil)
      (let ((smear-cursor-mode t)
            (smear-cursor-rest-effect nil))
        (smear-cursor--rest-yield (selected-window))
        (should-not stopped)))))
(ert-deftest smear-cursor-test-a-new-line-is-marked-however-it-arrives ()
  ;; GIVEN a newline inserted by a command of the mode's own
  ;; WHEN the buffer changes
  ;; THEN it is marked, because what is watched is the newline rather
  ;;      than the keystroke.
  ;;
  ;;      `markdown-enter-key' and its like insert a newline and any
  ;;      list prefix that goes with it, without going through
  ;;      `self-insert-command', so `post-self-insert-hook' never runs
  ;;      and the return key was marked in some buffers and not others.
  (let ((played nil))
    (cl-letf (((symbol-function 'smear-cursor--play-effect)
               (lambda (_w name &rest _) (push name played)))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (&rest _) (vector 0.0 0.0 8.0 18.0)))
              ((symbol-function 'smear-cursor--own-edit-p) (lambda (&rest _) t)))
      (let ((smear-cursor-mode t)
            (smear-cursor-effects '((insert . type-blink)
                                    (newline . cursor-glow))))
        (with-temp-buffer
          ;; a newline with a list prefix, as a markdown mode inserts
          (insert "- one\n- ")
          (smear-cursor--newline-fire 6 9 0)
          (should (eq 'cursor-glow (car played)))
          ;; an ordinary character is not a newline
          (setq played nil)
          (smear-cursor--newline-fire 1 2 0)
          (should-not played)
          ;; nor is a paste, which arrives with text of its own
          (setq played nil)
          (let ((from (point)))
            (insert "pasted\nlines\nhere\n")
            (smear-cursor--newline-fire from (point) 0))
          (should-not played))))))

(ert-deftest smear-cursor-test-a-newline-is-marked-once ()
  ;; GIVEN a newline typed with the return key
  ;; WHEN both the keystroke and the change are seen
  ;; THEN it is marked once: the typing effect leaves newlines alone.
  (let ((played nil))
    (cl-letf (((symbol-function 'smear-cursor--play-effect)
               (lambda (_w name &rest _) (push name played)))
              ((symbol-function 'smear-cursor--pos-rect)
               (lambda (&rest _) (vector 0.0 0.0 8.0 18.0)))
              ((symbol-function 'smear-cursor--child-frame-showing-p)
               (lambda () nil))
              ((symbol-function 'smear-cursor--own-edit-p) (lambda (&rest _) t)))
      (let ((smear-cursor-mode t)
            (smear-cursor-effects '((insert . type-blink)
                                    (newline . cursor-glow))))
        (with-temp-buffer
          (cl-letf (((symbol-function 'window-buffer)
                     (lambda (&rest _) (current-buffer))))
            (insert "a\n")
            (smear-cursor--insert-effect)
            (should-not played)
            (smear-cursor--newline-fire 2 3 0)
            (should (equal '(cursor-glow) played))))))))
(ert-deftest smear-cursor-test-a-newline-and-its-indentation-is-a-return ()
  ;; GIVEN a return in deeply indented code, or in a list
  ;; WHEN the change is weighed up
  ;; THEN it is a return, whatever came with the newline, while text
  ;;      arriving with newlines of its own is not.
  ;;
  ;;      No length in the rule.  A return in a nested block inserts a
  ;;      newline and forty spaces, which is longer than plenty of
  ;;      pastes, so any limit on the size of the change turns the
  ;;      effect off in exactly the code that is most indented.
  (should (smear-cursor--newline-alone-p "\n"))
  (should (smear-cursor--newline-alone-p
           (concat "\n" (make-string 40 ?\s))))          ; nested code
  (should (smear-cursor--newline-alone-p "\n- "))         ; a list carries on
  (should (smear-cursor--newline-alone-p "\n   ;; "))     ; and a comment
  (should (smear-cursor--newline-alone-p
           (concat "\n" (make-string 60 ?x))))           ; a long prefix, still one line
  (should-not (smear-cursor--newline-alone-p "no newline here"))
  (should-not (smear-cursor--newline-alone-p "\nfirst\nsecond\n")))
(ert-deftest smear-cursor-test-the-glow-keeps-out-of-a-prompt ()
  ;; GIVEN the minibuffer active
  ;; WHEN a command runs, as one does for every keystroke of a
  ;;      completion
  ;; THEN the glow is not aimed again.
  ;;
  ;;      Where the cursor is while a prompt is up is the prompt, and
  ;;      that is not what the glow is for.  It costs a millisecond a
  ;;      command against a tenth of one without it, and a prompt is
  ;;      where commands come fastest: `C-x b' pays it on every letter
  ;;      of the buffer name.
  (let ((played nil))
    (cl-letf (((symbol-function 'smear-cursor--play-effect)
               (lambda (&rest _) (setq played t)))
              ((symbol-function 'smear-cursor--point-rect)
               (lambda (_w) (vector 0.0 0.0 8.0 18.0)))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (let ((smear-cursor-mode t)
            (smear-cursor-rest-effect 'cursor-rest))
        (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t)))
          (smear-cursor--rest-forget)
          (smear-cursor--rest-follow)
          (should-not played))
        ;; and elsewhere it still follows
        (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) nil)))
          (smear-cursor--rest-forget)
          (smear-cursor--rest-follow)
          (should played))))))
(ert-deftest smear-cursor-test-the-glow-asks-the-display-only-when-it-must ()
  ;; GIVEN a command that moved nothing
  ;; WHEN the glow is asked to follow
  ;; THEN it does not measure anything: where point is and where the
  ;;      window starts are both free to read, and if neither has
  ;;      changed the cursor is where it was.
  ;;
  ;;      Measuring costs half a millisecond, and this runs after every
  ;;      command.  The window start is in the check as well as point,
  ;;      because scrolling moves the cursor on the screen without
  ;;      moving it in the buffer.
  (let ((asked 0))
    (cl-letf (((symbol-function 'smear-cursor--point-rect)
               (lambda (_w) (setq asked (1+ asked)) (vector 0.0 0.0 8.0 18.0)))
              ((symbol-function 'smear-cursor--play-effect) #'ignore)
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'minibufferp) (lambda (&rest _) nil)))
      (let ((smear-cursor-mode t)
            (smear-cursor-rest-effect 'cursor-rest))
        (smear-cursor--rest-forget)
        (smear-cursor--rest-follow)
        (should (= 1 asked))
        ;; nothing moved
        (smear-cursor--rest-follow)
        (smear-cursor--rest-follow)
        (should (= 1 asked))
        ;; point moved
        (cl-letf (((symbol-function 'window-point) (lambda (&rest _) 4)))
          (smear-cursor--rest-follow))
        (should (= 2 asked))))))
(ert-deftest smear-cursor-test-a-prompt-can-hold-the-trail-back ()
  ;; GIVEN a prompt up, with a completion previewing buffers behind it
  ;; WHEN the cursor lands somewhere new
  ;; THEN no trail is drawn, and the reason is on the record.
  ;;
  ;;      `consult' shows each candidate in the window behind the
  ;;      prompt, so browsing a buffer list is a jump across the window
  ;;      for every key pressed, and every one of them was a flight
  ;;      down the wire.  The work in Emacs is a third of a
  ;;      millisecond; what falls behind is the display.
  (let ((old (vector 0.0 0.0 8.0 18.0))
        (new (vector 400.0 300.0 8.0 18.0)))
    (cl-letf (((symbol-function 'smear-cursor--tracking-p) (lambda () nil))
              ((symbol-function 'smear-cursor--selecting-p) (lambda () nil))
              ((symbol-function 'minibuffer-depth) (lambda () 1)))
      (let ((smear-cursor-while-prompting nil)
            (smear-cursor--prompt-settled nil))
        (should (equal "a prompt is up"
                       (smear-cursor--why-not (selected-window) old new))))
      ;; on, it waits for the cursor to settle rather than drawing each jump
      (let ((smear-cursor-while-prompting t)
            (smear-cursor-prompt-settle 0.25)
            (smear-cursor--prompt-settled nil))
        (should (equal "a prompt is up"
                       (smear-cursor--why-not (selected-window) old new))))
      ;; and told not to wait, it draws them as they come
      (let ((smear-cursor-while-prompting t)
            (smear-cursor-prompt-settle nil)
            (smear-cursor--prompt-settled nil))
        (should-not (equal "a prompt is up"
                           (smear-cursor--why-not (selected-window) old new)))))
    ;; with no prompt up it never comes into it
    (cl-letf (((symbol-function 'smear-cursor--tracking-p) (lambda () nil))
              ((symbol-function 'smear-cursor--selecting-p) (lambda () nil))
              ((symbol-function 'minibuffer-depth) (lambda () 0)))
      (let ((smear-cursor-while-prompting nil))
        (should-not (equal "a prompt is up"
                           (smear-cursor--why-not (selected-window) old new)))))))
(ert-deftest smear-cursor-test-a-prompt-waits-for-the-cursor-to-settle ()
  ;; GIVEN a prompt up and the cursor jumping from candidate to
  ;;       candidate
  ;; WHEN the jumps come faster than the settling time
  ;; THEN nothing is drawn until they stop, and then one trail is,
  ;;      from where the burst began.
  ;;
  ;;      A completion that previews its candidates moves the cursor
  ;;      across the window for every key pressed, and a flight for
  ;;      each of them is what falls behind on a display reached over a
  ;;      network.  Waiting for the cursor to settle draws the one that
  ;;      says where it ended up, which is what the trail is for.
  (let ((waits 0) (cancelled 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (setq waits (1+ waits)) 'timer))
              ((symbol-function 'cancel-timer)
               (lambda (&rest _) (setq cancelled (1+ cancelled)))))
      (let ((smear-cursor-prompt-settle 0.25)
            (smear-cursor--prompt-timer nil)
            (smear-cursor--prompt-settled nil))
        ;; each jump puts the wait off again
        (smear-cursor--prompt-wait)
        (should (= 1 waits))
        (smear-cursor--prompt-wait)
        (should (= 2 waits))
        (should (= 1 cancelled))
        ;; and the wait lets exactly one sample through
        (setq smear-cursor--prompt-settled t)
        (should-not (smear-cursor--why-not
                     (selected-window) (vector 0.0 0.0 8.0 18.0)
                     (vector 400.0 300.0 8.0 18.0)))))))

(ert-deftest smear-cursor-test-a-settled-prompt-draws-from-where-it-began ()
  ;; GIVEN a burst of jumps behind a prompt
  ;; WHEN the cursor settles
  ;; THEN the origin is the one from before the burst, so the trail
  ;;      spans the whole of it rather than the last hop.
  ;;
  ;;      The recorded position is left alone while the burst runs, for
  ;;      that reason.
  (let ((recorded 0))
    (cl-letf (((symbol-function 'smear-cursor--record-p)
               (symbol-function 'smear-cursor--record-p)))
      (let ((smear-cursor-prompt-settle 0.25)
            (smear-cursor--prompt-settled nil))
        (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
          (should-not (smear-cursor--record-p "a prompt is up")))
        ;; once it has settled, and anywhere else, it is recorded
        (setq smear-cursor--prompt-settled t)
        (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
          (should (smear-cursor--record-p nil))))
      (ignore recorded))))
(ert-deftest smear-cursor-test-choosing-a-candidate-draws-the-whole-jump ()
  ;; GIVEN a prompt that has been held back while candidates were
  ;;       skimmed
  ;; WHEN the prompt closes on the one chosen
  ;; THEN the trail draws, and from where the cursor was before the
  ;;      prompt rather than from the candidate before last.
  (let ((old (vector 0.0 0.0 8.0 18.0))
        (new (vector 400.0 300.0 8.0 18.0)))
    (cl-letf (((symbol-function 'smear-cursor--tracking-p) (lambda () nil))
              ((symbol-function 'smear-cursor--selecting-p) (lambda () nil)))
      ;; while it is up, held back and nothing recorded
      (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
        (let ((smear-cursor--prompt-settled nil)
              (smear-cursor-prompt-settle 0.25))
          (should (equal "a prompt is up"
                         (smear-cursor--why-not (selected-window) old new)))
          (should-not (smear-cursor--record-p "a prompt is up"))))
      ;; the moment it closes, it is an ordinary movement again
      (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 0)))
        (let ((smear-cursor--prompt-settled nil))
          (should-not (smear-cursor--why-not (selected-window) old new))
          (should (smear-cursor--record-p nil)))))))
