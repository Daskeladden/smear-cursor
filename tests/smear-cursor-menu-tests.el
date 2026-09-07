;;; smear-cursor-menu-tests.el --- the menu's own logic -*- lexical-binding: t -*-

;; The layout is declarative, so these tests cover what the menu says:
;; which settings apply to the backend in use, which do not, and what
;; changing one from the menu changes.

(require 'ert)
(require 'cl-lib)
(require 'smear-cursor)
(require 'smear-cursor-menu)

(defmacro smear-cursor-menu-tests--with-preview (win &rest body)
  "Run BODY with WIN bound to a live preview window, then tear it down."
  (declare (indent 1))
  `(let ((,win (smear-cursor-menu--preview-window)))
     (unwind-protect (progn ,@body)
       (smear-cursor-menu--preview-stop))))

(ert-deftest smear-cursor-menu-test-a-setting-for-another-backend-says-so ()
  ;; GIVEN `smear-cursor-x11-alpha', which only the x11 backend draws,
  ;;       while the cpu backend is the one running
  ;; WHEN the menu describes it
  ;; THEN it says the setting is doing nothing.  An x11-only setting
  ;;      changed under cpu otherwise looks like a broken feature.
  (let ((smear-cursor-backend 'cpu))
    (should (smear-cursor-menu--inert-p 'smear-cursor-x11-alpha))
    (should (string-match-p "needs backend `x11'"
                            (smear-cursor-menu--describe
                             'smear-cursor-x11-alpha "alpha"))))
  (let ((smear-cursor-backend 'x11))
    (should-not (smear-cursor-menu--inert-p 'smear-cursor-x11-alpha))))

(ert-deftest smear-cursor-menu-test-a-setting-that-applies-anywhere-is-plain ()
  ;; GIVEN a setting every backend honours
  ;; WHEN the menu describes it
  ;; THEN no warning is attached.  A warning on every line would carry
  ;;      no information.
  (let ((smear-cursor-backend 'cpu))
    (should-not (smear-cursor-menu--inert-p 'smear-cursor-fps))
    (should-not (string-match-p "needs backend"
                                (smear-cursor-menu--describe
                                 'smear-cursor-fps "fps")))))

(ert-deftest smear-cursor-menu-test-a-description-carries-the-live-value ()
  ;; GIVEN a setting with a value
  ;; WHEN the menu describes it
  ;; THEN the value is in the line, so the menu shows what each setting
  ;;      holds and not only that it exists.
  (let ((smear-cursor-fps 90))
    (should (string-match-p "90" (smear-cursor-menu--describe
                                  'smear-cursor-fps "fps")))))

(ert-deftest smear-cursor-menu-test-every-defined-effect-is-offered ()
  ;; GIVEN the effects that ship
  ;; WHEN the menu asks what an occasion could be set to
  ;; THEN each of them is on the list.  Effects are registered by macro
  ;;      into a hash table, so nothing else lists them.
  (let ((names (smear-cursor-menu--effect-names)))
    (dolist (want '(line-pulse region-flash region-fade spark-dot type-blink
                    cursor-glow cursor-breathe lightning))
      (should (memq want names)))))

(ert-deftest smear-cursor-menu-test-setting-an-occasion-replaces-not-appends ()
  ;; GIVEN an occasion that already has an effect
  ;; WHEN it is set to another from the menu
  ;; THEN the entry is replaced.  `smear-cursor-effects' is read with
  ;;      `assq', so a second entry for the same occasion would be
  ;;      ignored and the menu would appear to do nothing.
  (let ((smear-cursor-effects '((pulse . line-pulse) (insert . nil))))
    (smear-cursor-menu--set-occasion 'insert 'type-blink)
    (should (eq 'type-blink (cdr (assq 'insert smear-cursor-effects))))
    (should (= 1 (cl-count 'insert smear-cursor-effects :key #'car)))
    (should (eq 'line-pulse (cdr (assq 'pulse smear-cursor-effects))))))

(ert-deftest smear-cursor-menu-test-an-occasion-can-be-turned-off ()
  ;; GIVEN an occasion with an effect
  ;; WHEN it is set to nil
  ;; THEN the entry stays, holding nil.  The menu lists occasions from
  ;;      this alist, so an entry dropped when switched off could not be
  ;;      switched back on.
  (let ((smear-cursor-effects '((insert . type-blink))))
    (smear-cursor-menu--set-occasion 'insert nil)
    (should (assq 'insert smear-cursor-effects))
    (should-not (cdr (assq 'insert smear-cursor-effects)))))

(ert-deftest smear-cursor-menu-test-the-state-line-says-what-is-drawing ()
  ;; GIVEN a configured session
  ;; WHEN the menu draws its heading
  ;; THEN backend, renderer and style are all in it.  Every other line
  ;;      is read against those three.
  (let ((smear-cursor-backend 'x11)
        (smear-cursor-x11-renderer 'gl)
        (smear-cursor-trail-style 'laser))
    (let ((line (smear-cursor-menu--state)))
      (should (string-match-p "x11" line))
      (should (string-match-p "gl" line))
      (should (string-match-p "laser" line)))))

(ert-deftest smear-cursor-menu-test-it-names-the-settings-being-ignored ()
  ;; GIVEN an x11-only setting changed while the cpu backend is running
  ;; WHEN the menu asks what is being ignored
  ;; THEN it names it, and does not name one left at its default.
  (let ((smear-cursor-backend 'cpu)
        (smear-cursor-x11-alpha 0.5)
        (smear-cursor-x11-tail-alpha
         (smear-cursor-menu--default 'smear-cursor-x11-tail-alpha)))
    (let ((ignored (smear-cursor-menu--ignored)))
      (should (memq 'smear-cursor-x11-alpha ignored))
      (should-not (memq 'smear-cursor-x11-tail-alpha ignored)))))

(ert-deftest smear-cursor-menu-test-nothing-is-ignored-on-its-own-backend ()
  ;; GIVEN the same setting with the backend that draws it
  ;; WHEN the menu asks
  ;; THEN nothing is named.
  (let ((smear-cursor-backend 'x11)
        (smear-cursor-x11-alpha 0.5))
    (should-not (memq 'smear-cursor-x11-alpha
                      (smear-cursor-menu--ignored)))))

(ert-deftest smear-cursor-menu-test-the-idle-effect-goes-through-its-setter ()
  ;; GIVEN an effect chosen for when the cursor is left alone
  ;; WHEN the menu sets it
  ;; THEN it goes through the custom setter, which is what starts and
  ;;      stops the timer.  A plain `setq' would leave the timer as it
  ;;      was, so the menu line would name an effect that never played.
  (let ((smear-cursor-idle-effect nil)
        (set-through nil))
    (cl-letf (((symbol-function 'smear-cursor-menu--pick-effect)
               (lambda (&rest _) 'pacman))
              ((symbol-function 'customize-set-variable)
               (lambda (sym val) (setq set-through (cons sym val)))))
      (smear-cursor-menu--read-idle-effect)
      (should (equal '(smear-cursor-idle-effect . pacman) set-through)))))

(ert-deftest smear-cursor-menu-test-the-idle-effect-applies-to-every-backend ()
  ;; GIVEN the x11 backend
  ;; WHEN the menu describes the idle effect
  ;; THEN no backend warning is attached: effects need x11, and x11 is
  ;;      what is running.
  (let ((smear-cursor-backend 'x11))
    (should-not (smear-cursor-menu--inert-p 'smear-cursor-idle-effect))))

(ert-deftest smear-cursor-menu-test-a-candidate-is-previewed-once ()
  ;; GIVEN the same candidate under point for two commands running
  ;; WHEN the preview runs after each
  ;; THEN it plays once.  `post-command-hook' runs after every key,
  ;;      including the ones that only narrow the list, and playing on
  ;;      each of them would stack effects on one track.
  (let ((smear-cursor-menu--previewed nil)
        (plays 0))
    (cl-letf (((symbol-function 'smear-cursor-menu--candidate)
               (lambda () "line-pulse"))
              ((symbol-function 'smear-cursor-menu--preview-start)
               (lambda (_name) (setq plays (1+ plays)))))
      (smear-cursor-menu--preview-candidate)
      (smear-cursor-menu--preview-candidate)
      (should (= 1 plays)))))

(ert-deftest smear-cursor-menu-test-nothing-is-previewed-for-off ()
  ;; GIVEN `off' under point, which names no effect
  ;; WHEN the preview runs
  ;; THEN nothing is played, and the same goes for a half-typed name.
  (let ((smear-cursor-menu--previewed nil)
        (played nil))
    (cl-letf (((symbol-function 'smear-cursor-menu--preview-start)
               (lambda (_name) (setq played t))))
      (cl-letf (((symbol-function 'smear-cursor-menu--candidate)
                 (lambda () "off")))
        (smear-cursor-menu--preview-candidate))
      (cl-letf (((symbol-function 'smear-cursor-menu--candidate)
                 (lambda () "line-pul")))
        (smear-cursor-menu--preview-candidate))
      (should-not played))))

(ert-deftest smear-cursor-menu-test-the-live-preview-can-be-turned-off ()
  ;; GIVEN the live preview switched off
  ;; WHEN a candidate comes under point
  ;; THEN nothing is played.  Choosing an effect should not draw one
  ;;      for anybody who would rather it did not.
  (let ((smear-cursor-menu-live-preview nil)
        (smear-cursor-menu--previewed nil)
        (played nil))
    (cl-letf (((symbol-function 'smear-cursor-menu--candidate)
               (lambda () "line-pulse"))
              ((symbol-function 'smear-cursor-menu--preview-start)
               (lambda (_name) (setq played t))))
      (smear-cursor-menu--preview-candidate)
      (should-not played))))

(ert-deftest smear-cursor-menu-test-the-candidate-falls-back-to-what-is-typed ()
  ;; GIVEN no completion UI that highlights one candidate
  ;; WHEN the current candidate is asked for
  ;; THEN it is what stands in the minibuffer.  Which candidate is
  ;;      current is the UI's own idea, and plain completion has only
  ;;      the text that has been typed.
  (cl-letf (((symbol-function 'minibuffer-contents-no-properties)
             (lambda () "spark-dot")))
    (should (equal "spark-dot" (smear-cursor-menu--candidate)))))

(ert-deftest smear-cursor-menu-test-a-typing-effect-is-shown-by-typing ()
  ;; GIVEN an effect that marks the character just typed
  ;; WHEN the preview takes a step
  ;; THEN one more character of the sample appears and the effect is
  ;;      played on it.  An insert effect only happens while typing, so
  ;;      a preview that showed it once at a resting cursor would not
  ;;      show what it does.
  (let ((smear-cursor-menu--preview-column 0)
        (played nil))
    (smear-cursor-menu-tests--with-preview win
      (cl-letf (((symbol-function 'smear-cursor-menu--preview-rects)
                 (lambda (_w _e) '(rect)))     ; batch has no display to measure
                ((symbol-function 'smear-cursor--play-effect)
                 (lambda (_w name _r _t &rest _) (setq played name))))
        (smear-cursor-menu--preview-step win 'type-blink '(:shape point))
        (should (= 1 smear-cursor-menu--preview-column))
        (should (equal (substring smear-cursor-menu--preview-text 0 1)
                       (with-current-buffer (window-buffer win)
                         (buffer-substring-no-properties
                          (- (point-max) 1) (point-max)))))
        (should (eq 'type-blink played))))))

(ert-deftest smear-cursor-menu-test-the-typed-sample-starts-over ()
  ;; GIVEN a sample that has been typed out to its end
  ;; WHEN the preview takes another step
  ;; THEN it clears and begins again, so the demonstration repeats for
  ;;      as long as the effect is under point.
  (let ((smear-cursor-menu--preview-column
         (length smear-cursor-menu--preview-text)))
    (smear-cursor-menu-tests--with-preview win
      (cl-letf (((symbol-function 'smear-cursor--play-effect) #'ignore))
        (smear-cursor-menu--preview-step win 'type-blink '(:shape point))
        (should (= 0 smear-cursor-menu--preview-column))))))

(ert-deftest smear-cursor-menu-test-a-region-effect-is-shown-over-the-sample ()
  ;; GIVEN an effect that covers a region
  ;; WHEN the preview takes a step
  ;; THEN the whole sample is there at once and the effect plays over
  ;;      it.  Copying and deleting mark text that is already written,
  ;;      so there is nothing to type out for them.
  (let ((smear-cursor-menu--preview-column 0)
        (played nil))
    (smear-cursor-menu-tests--with-preview win
      (cl-letf (((symbol-function 'smear-cursor-menu--preview-rects)
                 (lambda (_w _e) '(rect)))     ; batch has no display to measure
                ((symbol-function 'smear-cursor--play-effect)
                 (lambda (_w name _r _t &rest _) (setq played name))))
        (smear-cursor-menu--preview-step win 'region-flash '(:shape region))
        (should (eq 'region-flash played))
        (should (string-match-p
                 (regexp-quote smear-cursor-menu--preview-text)
                 (with-current-buffer (window-buffer win) (buffer-string))))))))

(ert-deftest smear-cursor-menu-test-the-preview-panel-is-taken-down ()
  ;; GIVEN a preview running in its own window
  ;; WHEN it is stopped, as it is when the prompt ends
  ;; THEN the timer is cancelled and the buffer is gone.  A panel left
  ;;      behind would sit in the layout after the menu had closed.
  (smear-cursor-menu-tests--with-preview _win
    (smear-cursor-menu--preview-stop)
    (should-not smear-cursor-menu--preview-timer)
    (should-not (get-buffer smear-cursor-menu--preview-buffer-name))))

(ert-deftest smear-cursor-menu-test-the-seed-keys-step-both-ways ()
  ;; GIVEN the keys the roll accepts
  ;; WHEN each is looked up
  ;; THEN n and space go forward, p goes back, and anything else does
  ;;      not move the seed.
  (should (= 1 (smear-cursor-menu--seed-step ?n)))
  (should (= 1 (smear-cursor-menu--seed-step ?\s)))
  (should (= -1 (smear-cursor-menu--seed-step ?p)))
  (should-not (smear-cursor-menu--seed-step ?z)))

(ert-deftest smear-cursor-menu-test-rolling-the-seed-shows-each-one ()
  ;; GIVEN a roll of two steps forward and one back, then RET
  ;; WHEN the seed is rolled
  ;; THEN the seed lands where the keys left it and the panel is
  ;;      restarted for each one, which is the point: the effect is
  ;;      redrawn so the new arrangement can be seen before it is kept.
  (let ((smear-cursor-noise-seed 5)
        (keys (list ?n ?n ?p ?\r))
        (starts 0))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) (pop keys)))
              ((symbol-function 'smear-cursor-menu--preview-start)
               (lambda (_name) (setq starts (1+ starts))))
              ((symbol-function 'smear-cursor-menu--preview-stop) #'ignore))
      (smear-cursor-menu--roll-noise)
      (should (= 6 smear-cursor-noise-seed))
      ;; one to open the panel, then one for each of the three steps
      (should (= 4 starts)))))

(ert-deftest smear-cursor-menu-test-cancelling-a-roll-puts-the-seed-back ()
  ;; GIVEN a roll that is quit part way through
  ;; WHEN it ends
  ;; THEN the seed is the one it started from.  Rolling is for looking
  ;;      at arrangements, so quitting has to leave the setting alone.
  (let ((smear-cursor-noise-seed 5)
        (keys (list ?n ?n ?\C-g)))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) (pop keys)))
              ((symbol-function 'smear-cursor-menu--preview-start) #'ignore)
              ((symbol-function 'smear-cursor-menu--preview-stop) #'ignore))
      (smear-cursor-menu--roll-noise)
      (should (= 5 smear-cursor-noise-seed)))))

(ert-deftest smear-cursor-menu-test-a-roll-shows-the-effect-typing-uses ()
  ;; GIVEN an effect set for typing
  ;; WHEN the seed is rolled
  ;; THEN that effect is the one demonstrated, since it is the one the
  ;;      seed will change.  With typing set to none there is still
  ;;      something to look at.
  (let ((smear-cursor-effects '((insert . spark-dot)))
        (shown nil)
        (keys (list ?\r)))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) (pop keys)))
              ((symbol-function 'smear-cursor-menu--preview-start)
               (lambda (name) (setq shown name)))
              ((symbol-function 'smear-cursor-menu--preview-stop) #'ignore))
      (smear-cursor-menu--roll-noise)
      (should (eq 'spark-dot shown))))
  (let ((smear-cursor-effects '((insert . nil)))
        (shown nil)
        (keys (list ?\r)))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) (pop keys)))
              ((symbol-function 'smear-cursor-menu--preview-start)
               (lambda (name) (setq shown name)))
              ((symbol-function 'smear-cursor-menu--preview-stop) #'ignore))
      (smear-cursor-menu--roll-noise)
      (should shown))))

(ert-deftest smear-cursor-menu-test-a-bolt-height-may-be-lines-or-the-window ()
  ;; GIVEN what can be typed at the height prompt
  ;; WHEN each is read
  ;; THEN `window' comes back as the symbol and a number as a number,
  ;;      while half-typed text comes back as nothing.  The height is
  ;;      the one setting here that takes either, and the symbol is
  ;;      the part nobody would guess from a prompt for a number.
  (should (eq 'window (smear-cursor-menu--lines-value "window")))
  (should (= 5 (smear-cursor-menu--lines-value "5")))
  (should (= 2.5 (smear-cursor-menu--lines-value "2.5")))
  (should-not (smear-cursor-menu--lines-value "wind"))
  (should-not (smear-cursor-menu--lines-value "")))

(ert-deftest smear-cursor-menu-test-the-bolt-is-redrawn-at-the-height-typed ()
  ;; GIVEN a height under point at the prompt
  ;; WHEN the preview runs
  ;; THEN the setting takes that value and the panel starts again, so
  ;;      the bolt on screen is the length being chosen rather than
  ;;      the one already set.
  (let ((smear-cursor-lightning-lines 3)
        (smear-cursor-menu--previewed nil)
        (shown nil))
    (cl-letf (((symbol-function 'smear-cursor-menu--candidate)
               (lambda () "window"))
              ((symbol-function 'smear-cursor-menu--preview-start)
               (lambda (name) (setq shown name))))
      (smear-cursor-menu--preview-lines)
      (should (eq 'window smear-cursor-lightning-lines))
      (should (eq 'lightning shown)))))

(ert-deftest smear-cursor-menu-test-half-typed-heights-are-left-alone ()
  ;; GIVEN text that is not yet a height
  ;; WHEN the preview runs
  ;; THEN the setting is untouched.  Every key runs this, so `wind' on
  ;;      the way to `window' must not be taken as a value.
  (let ((smear-cursor-lightning-lines 3)
        (smear-cursor-menu--previewed nil)
        (shown nil))
    (cl-letf (((symbol-function 'smear-cursor-menu--candidate)
               (lambda () "wind"))
              ((symbol-function 'smear-cursor-menu--preview-start)
               (lambda (name) (setq shown name))))
      (smear-cursor-menu--preview-lines)
      (should (= 3 smear-cursor-lightning-lines))
      (should-not shown))))

(ert-deftest smear-cursor-menu-test-quitting-the-height-puts-it-back ()
  ;; GIVEN a height being chosen, with the preview having already
  ;;       moved the setting as it was browsed
  ;; WHEN the prompt is quit
  ;; THEN the height is the one it started at.  The preview changes
  ;;      the setting to draw it, so quitting has to undo that.
  (let ((smear-cursor-lightning-lines 3))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _)
                 (setq smear-cursor-lightning-lines 'window)   ; the preview
                 (signal 'quit nil)))
              ((symbol-function 'smear-cursor-menu--preview-stop) #'ignore))
      (smear-cursor-menu--read-lightning-lines)
      (should (= 3 smear-cursor-lightning-lines)))))

(ert-deftest smear-cursor-menu-test-a-roll-holds-the-shape-still ()
  ;; GIVEN an effect that takes a new shape on every play
  ;; WHEN the seed is rolled
  ;; THEN the variation is held off while rolling.  Otherwise each
  ;;      strike reshuffles anyway and the seed looks like it does
  ;;      nothing, which is exactly how it looked.
  (let ((smear-cursor-effects '((insert . lightning)))
        (smear-cursor-lightning-vary t)
        (keys (list ?n ?\r))
        (varied 'unset))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) (pop keys)))
              ((symbol-function 'smear-cursor-menu--preview-start)
               (lambda (_name) (setq varied smear-cursor-lightning-vary)))
              ((symbol-function 'smear-cursor-menu--preview-stop) #'ignore))
      (smear-cursor-menu--roll-noise)
      (should-not varied)
      ;; and the setting is as it was once the roll is over
      (should smear-cursor-lightning-vary))))

(ert-deftest smear-cursor-menu-test-a-roll-says-when-the-seed-will-not-show ()
  ;; GIVEN an effect left to take a new shape on every play
  ;; WHEN a seed is kept
  ;; THEN it says so, since the seed just chosen will not be what is
  ;;      drawn until the variation is turned off.
  (let ((smear-cursor-effects '((insert . lightning)))
        (smear-cursor-lightning-vary t)
        (keys (list ?\r))
        (said nil))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) (pop keys)))
              ((symbol-function 'smear-cursor-menu--preview-start) #'ignore)
              ((symbol-function 'smear-cursor-menu--preview-stop) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (smear-cursor-menu--roll-noise)
      (should said)
      (should (string-match-p "new shape" said)))))

(ert-deftest smear-cursor-menu-test-a-roll-is-quiet-when-the-seed-holds ()
  ;; GIVEN an effect that keeps one shape
  ;; WHEN a seed is kept
  ;; THEN nothing is said, because what was seen is what will be drawn.
  (let ((smear-cursor-effects '((insert . lightning)))
        (smear-cursor-lightning-vary nil)
        (keys (list ?\r))
        (said nil))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) (pop keys)))
              ((symbol-function 'smear-cursor-menu--preview-start) #'ignore)
              ((symbol-function 'smear-cursor-menu--preview-stop) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (smear-cursor-menu--roll-noise)
      (should-not said))))

(ert-deftest smear-cursor-menu-test-a-setting-emacs-has-not-got-says-so ()
  ;; GIVEN a setting this Emacs has never defined, as happens when the
  ;;       package is updated under a session that loaded the old one
  ;; WHEN the menu tries to read it
  ;; THEN it says the package has moved on rather than reporting a
  ;;      void variable, which says nothing about what to do.
  (let ((err (should-error (smear-cursor-menu--read-number
                            'smear-cursor-no-such-setting "How much"))))
    (should (string-match-p "restart" (error-message-string err)))))

(ert-deftest smear-cursor-menu-test-gl-over-a-forward-is-called-out ()
  ;; GIVEN the GL renderer chosen by hand on a display reached over a
  ;;       network
  ;; WHEN the menu describes the renderer
  ;; THEN it says what that costs, in the heading.  GL uploads a
  ;;      picture of every frame, so an effect that moves arrives as a
  ;;      few stills, and the setting that causes it gives no hint of
  ;;      it.  It goes in the heading because a note long enough to say
  ;;      that is longer than the column it would otherwise sit in.
  (cl-letf (((symbol-function 'smear-cursor-x11--local-display-p)
             (lambda (&rest _) nil)))
    (let ((smear-cursor-x11-renderer 'gl))
      (should (smear-cursor-menu--renderer-note))
      (should (string-match-p "picture" (smear-cursor-menu--state)))
      ;; and the renderer's own line stays the width of every other
      (should-not (string-match-p
                   "picture" (smear-cursor-menu--describe
                              'smear-cursor-x11-renderer "x11 renderer"))))
    ;; and nothing to say for the ones that are fine
    (let ((smear-cursor-x11-renderer 'render))
      (should-not (smear-cursor-menu--renderer-note)))
    (let ((smear-cursor-x11-renderer 'auto))
      (should-not (smear-cursor-menu--renderer-note)))))

(ert-deftest smear-cursor-menu-test-gl-on-this-machine-is-fine ()
  ;; GIVEN the GL renderer on a local display
  ;; WHEN the menu describes it
  ;; THEN there is nothing to warn about: nothing crosses a wire.
  (cl-letf (((symbol-function 'smear-cursor-x11--local-display-p)
             (lambda (&rest _) t)))
    (let ((smear-cursor-x11-renderer 'gl))
      (should-not (smear-cursor-menu--renderer-note)))))

(provide 'smear-cursor-menu-tests)
;;; smear-cursor-menu-tests.el ends here

(ert-deftest smear-cursor-menu-test-the-new-settings-are-on-the-menu ()
  ;; GIVEN the settings added since the menu was last touched
  ;; WHEN the menu is searched for them
  ;; THEN each has a suffix, a key of its own, and a line that names
  ;;      its value.
  ;;
  ;;      A setting only reachable through `customize' is one nobody
  ;;      finds: the menu is where these are meant to be tried.
  (let ((wired nil))
    (letrec ((walk (lambda (node)
                     (cond
                      ((vectorp node) (mapc walk (append node nil)))
                      ((and (consp node) (symbolp (car node))
                            (plist-member (cdr node) :key))
                       (push (cons (plist-get (cdr node) :command)
                                   (plist-get (cdr node) :key))
                             wired))
                      ((consp node) (mapc walk node))))))
      (funcall walk (get 'smear-cursor-menu 'transient--layout)))
    (dolist (spec '((smear-cursor-menu-pulse-flashes
                     smear-cursor-pulse-flashes 3 "pulse flashes")
                    (smear-cursor-menu-roam-columns
                     smear-cursor-roam-columns 40 "roams")
                    (smear-cursor-menu-pacman-ghosts
                     smear-cursor-pacman-ghosts 2 "ghosts")))
      (let ((suffix (nth 0 spec)) (var (nth 1 spec))
            (value (nth 2 spec)) (label (nth 3 spec)))
        (should (fboundp suffix))
        (should (assq suffix wired))
        ;; Bound, not set: these are the real settings, and a test that
        ;; leaves one changed is a test that breaks another.
        (cl-progv (list var) (list value)
          (let ((line (smear-cursor-menu--describe var label)))
            (should (string-match-p label line))
            (should (string-match-p (number-to-string value) line))))))))

(defun smear-cursor-menu-tests--plist (group)
  "Return GROUP\='s plist, or nil when it has none.

A group is [CLASS PLIST CHILDREN], and the plist is where group-wide
settings such as `:level\=' and `:pad-keys\=' land."
  (and (> (length group) 1)
       (let ((x (aref group 1)))
         (and (consp x) (keywordp (car x)) x))))

(defun smear-cursor-menu-tests--group-level (node level)
  "Return the level NODE\='s children sit at, given LEVEL from above.

A suffix inside a group held back is held back with it, so the deeper
of the two wins."
  (max level (or (plist-get (smear-cursor-menu-tests--plist node) :level)
                 level)))

(defun smear-cursor-menu-tests--suffix (node level)
  "Return NODE as (:command C :key K :description D :level N).

The level is LEVEL or deeper."
  (list :command (plist-get (cdr node) :command)
        :key (plist-get (cdr node) :key)
        :description (plist-get (cdr node) :description)
        :level (max level (or (plist-get (cdr node) :level)
                              transient--default-child-level))))

(defun smear-cursor-menu-tests--description (command)
  "Return the text of COMMAND\='s line in the menu.

The layout\='s own description when it gives one, or else the one the
command was defined with."
  (let* ((suffix (seq-find (lambda (s) (eq (plist-get s :command) command))
                           (smear-cursor-menu-tests--suffixes)))
         (desc (or (plist-get suffix :description)
                   (oref (get command 'transient--suffix) description))))
    (should desc)
    (if (functionp desc) (funcall desc) desc)))

(defun smear-cursor-menu-tests--suffix-p (node)
  "Return non-nil when NODE is a suffix rather than a group or a plist."
  (and (consp node) (symbolp (car node)) (not (keywordp (car node)))
       (plist-member (cdr node) :key)))

(defun smear-cursor-menu-tests--walk (node level)
  "Return a plist for every suffix under NODE, LEVEL inherited from above."
  (cond
   ((vectorp node)
    (let ((own (smear-cursor-menu-tests--group-level node level)))
      (mapcan (lambda (n) (smear-cursor-menu-tests--walk n own))
              (append node nil))))
   ((smear-cursor-menu-tests--suffix-p node)
    (list (smear-cursor-menu-tests--suffix node level)))
   ((consp node)
    (mapcan (lambda (n) (smear-cursor-menu-tests--walk n level)) node))))

(defun smear-cursor-menu-tests--suffixes ()
  "Return a plist for every suffix in the menu, with the level it shows at."
  (smear-cursor-menu-tests--walk (get 'smear-cursor-menu 'transient--layout)
                                 transient--default-child-level))

(defun smear-cursor-menu-tests--shown ()
  "Return the commands the menu shows at the level it opens with."
  (delq nil (mapcar (lambda (s) (and (<= (plist-get s :level)
                                         transient-default-level)
                                     (plist-get s :command)))
                    (smear-cursor-menu-tests--suffixes))))

(ert-deftest smear-cursor-menu-test-every-key-is-its-own ()
  ;; GIVEN the whole layout
  ;; WHEN the keys are collected
  ;; THEN none repeats, and none is the start of another.
  ;;
  ;;      Transient reads keys a character at a time, so a key that is
  ;;      the start of another can never be pressed on its own.
  (let ((keys (delq nil (mapcar (lambda (s) (plist-get s :key))
                                (smear-cursor-menu-tests--suffixes)))))
    (should keys)
    (should (= (length keys) (length (delete-dups (copy-sequence keys)))))
    (dolist (a keys)
      (dolist (b keys)
        (unless (equal a b)
          (should-not (string-prefix-p a b)))))))

(ert-deftest smear-cursor-menu-test-every-setting-is-reachable ()
  ;; GIVEN a hundred and thirty-eight settings and a menu of forty keys
  ;; WHEN a setting is not one of the forty
  ;; THEN there is still one key that reaches it.
  ;;
  ;;      Every setting on a key of its own is a menu nobody can read.
  ;;      The everyday ones have keys; the rest are reached through
  ;;      `customize', which is one key away rather than none.
  (should (fboundp 'smear-cursor-menu-customize))
  (let ((wired (mapcar (lambda (s) (plist-get s :command))
                       (smear-cursor-menu-tests--suffixes))))
    (dolist (suffix '(smear-cursor-menu-customize
                      smear-cursor-menu-typing-highlight
                      smear-cursor-menu-typing-highlight-strength
                      smear-cursor-menu-pad-line-ends
                      smear-cursor-menu-while-selecting
                      smear-cursor-menu-idle-while-prompting))
      (should (fboundp suffix))
      (should (memq suffix wired)))))

(ert-deftest smear-cursor-menu-test-a-toggle-turns-a-setting-over ()
  ;; GIVEN a boolean setting
  ;; WHEN the menu toggles it
  ;; THEN it changes, and changes back.
  (let ((smear-cursor-pad-line-ends t))
    (smear-cursor-menu--toggle 'smear-cursor-pad-line-ends)
    (should-not smear-cursor-pad-line-ends)
    (smear-cursor-menu--toggle 'smear-cursor-pad-line-ends)
    (should smear-cursor-pad-line-ends))
  ;; and says so when the setting is not in this Emacs at all
  (should-error (smear-cursor-menu--toggle 'smear-cursor-no-such-setting)))

(ert-deftest smear-cursor-menu-test-the-menu-runs-a-settings-own-setter ()
  ;; GIVEN the idle delay, whose setter re-arms the idle timer
  ;; WHEN the menu reads a new delay for it
  ;; THEN the timer is armed again at the new delay.
  ;;
  ;;      Stored with plain `set' the value changes and nothing else
  ;;      happens: the timer keeps the delay it was made with, so the
  ;;      menu looks like it did nothing until the package is loaded
  ;;      again.  Every setting the menu changes goes through
  ;;      `customize-set-variable' for that reason, not this one alone.
  (let ((armed nil))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "4.0"))
              ((symbol-function 'run-with-idle-timer)
               (lambda (secs &rest _) (setq armed secs) 'timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (let ((smear-cursor-mode t)
            (smear-cursor-idle-effect 'pacman)
            (smear-cursor--idle-timer nil)
            (smear-cursor-idle-delay 1.5))
        (smear-cursor-menu--read-number 'smear-cursor-idle-delay "Seconds")
        (should (= 4.0 smear-cursor-idle-delay))
        (should (= 4.0 armed))))))

(defconst smear-cursor-menu-tests--everyday
  '(smear-cursor-menu-style smear-cursor-menu-color smear-cursor-menu-fps
    smear-cursor-menu-pulse smear-cursor-menu-copy smear-cursor-menu-delete
    smear-cursor-menu-yank smear-cursor-menu-insert
    smear-cursor-menu-newline
    smear-cursor-menu-idle-effect smear-cursor-menu-idle-delay
    smear-cursor-menu-effect-strength smear-cursor-menu-effect-color
    smear-cursor-menu-preview smear-cursor-menu-typing-highlight
    smear-cursor-menu-backend smear-cursor-menu-renderer
    smear-cursor-menu-report smear-cursor-menu-customize
    smear-cursor-menu-save smear-cursor-menu-toggle transient-quit-one
    smear-cursor-menu-rest-effect
    transient-set-level)
  "What the menu shows before anyone asks it for more.")

(defconst smear-cursor-menu-tests--fiddly
  '(smear-cursor-menu-min-distance smear-cursor-menu-long-jump-rows
    smear-cursor-menu-long-jump-duration smear-cursor-menu-pad-line-ends
    smear-cursor-menu-while-selecting smear-cursor-menu-pulse-commands
    smear-cursor-menu-delete-min smear-cursor-menu-pulse-rows
    smear-cursor-menu-pulse-flashes smear-cursor-menu-pulse-strength
    smear-cursor-menu-noise smear-cursor-menu-typing-highlight-strength
    smear-cursor-menu-typing-highlight-duration
    smear-cursor-menu-blink-strength smear-cursor-menu-blink-duration
    smear-cursor-menu-blink-radius smear-cursor-menu-lightning-lines
    smear-cursor-menu-lightning-noise smear-cursor-menu-lightning-vary
    smear-cursor-menu-plasma-arcs smear-cursor-menu-plasma-noise
    smear-cursor-menu-fire-embers smear-cursor-menu-fire-height
    smear-cursor-menu-idle-while-prompting smear-cursor-menu-roam-columns
    smear-cursor-menu-scan-passes smear-cursor-menu-scan-pace
    smear-cursor-menu-while-prompting smear-cursor-menu-prompt-settle
    smear-cursor-menu-rest-strength smear-cursor-menu-rest-duration
    smear-cursor-menu-rest-dip smear-cursor-menu-rest-size
    smear-cursor-menu-pacman-ghosts smear-cursor-menu-pacman-power
    smear-cursor-menu-pacman-berserk
    smear-cursor-menu-trace
    smear-cursor-menu-watch smear-cursor-menu-ignored)
  "What it keeps back until asked, set once and forgotten.")

(ert-deftest smear-cursor-menu-test-the-fiddly-settings-are-held-back ()
  ;; GIVEN the menu at the level it opens with
  ;; WHEN its suffixes are read with the levels they carry
  ;; THEN the everyday ones are there and the fiddly ones are not.
  ;;
  ;;      Fifty settings at once is a wall rather than a menu.  What is
  ;;      reached for while writing is in front; what is set once and
  ;;      forgotten is behind `C-x l', which transient remembers for
  ;;      next time.
  (let ((shown (smear-cursor-menu-tests--shown)))
    (dolist (cmd smear-cursor-menu-tests--everyday)
      (should (memq cmd shown)))
    (dolist (cmd smear-cursor-menu-tests--fiddly)
      (should-not (memq cmd shown)))
    ;; and the whole menu is still reachable at the level above
    (let ((all (mapcar (lambda (s) (plist-get s :command))
                       (smear-cursor-menu-tests--suffixes))))
      (dolist (cmd smear-cursor-menu-tests--fiddly)
        (should (memq cmd all))))))

(ert-deftest smear-cursor-menu-test-a-changed-setting-is-kept-only-when-saved ()
  ;; GIVEN a setting changed through the menu
  ;; WHEN it has not been saved
  ;; THEN the menu counts it as unsaved, and saving writes it out.
  ;;
  ;;      Changing a setting here lasts as long as the session, which
  ;;      looks the same as lasting until it is not.  The menu says how
  ;;      many are in that state and takes one key to keep them.
  (let ((sym 'smear-cursor-idle-delay)
        (saved nil))
    (unwind-protect
        (progn
          (put sym 'customized-value (list (custom-quote 4.0)))
          (should (memq sym (smear-cursor-menu--unsaved)))
          (cl-letf (((symbol-function 'customize-save-variable)
                     (lambda (s _v) (push s saved))))
            (smear-cursor-menu-save))
          (should (memq sym saved)))
      (put sym 'customized-value nil))
    ;; nothing changed, nothing to save
    (cl-letf (((symbol-function 'smear-cursor-menu--unsaved) (lambda () nil))
              ((symbol-function 'customize-save-variable)
               (lambda (&rest _) (error "Saved nothing at all"))))
      (smear-cursor-menu-save))))

(ert-deftest smear-cursor-menu-test-every-line-is-the-same-width ()
  ;; GIVEN menu lines carrying a setting and its value
  ;; WHEN they are laid out beside each other
  ;; THEN short ones are all one width, and a long value still shows.
  ;;
  ;;      Transient pads a column to its widest line and each row of
  ;;      columns to itself, so lines of their own widths give a menu
  ;;      whose columns start somewhere different on every row.
  (let ((w (length (smear-cursor-menu--line "x" "y"))))
    (should (= w (length (smear-cursor-menu--line "a longer label" "val"))))
    (let ((long (smear-cursor-menu--line "x" "a value longer than the column")))
      (should (> (length long) w))
      (should (string-match-p "a value longer than the column" long)))
    ;; and the settings themselves are laid out the same way
    (dolist (sym '(smear-cursor-fps smear-cursor-idle-delay
                   smear-cursor-trail-style))
      (should (>= (length (smear-cursor-menu--describe sym "label")) w)))))

(ert-deftest smear-cursor-menu-test-every-key-takes-the-same-width ()
  ;; GIVEN keys of one letter and of two, in every row of the menu
  ;; WHEN transient lays the rows out
  ;; THEN every key is padded to the widest in the whole menu, so a
  ;;      label and its value start in the same column whatever key
  ;;      stands in front of them, in every group of every row.
  ;;
  ;;      Transient pads keys only for a group that asks, and then only
  ;;      to that group's widest key.  Padding group by group left the
  ;;      two-letter groups a character to the right of the one-letter
  ;;      ones, and a group showing one short key, padded for its hidden
  ;;      long ones, pushed the column beside it over as well.  A column
  ;;      that asks for `t' pads to its own widest key, not the menu's.
  (let ((widest (apply #'max (mapcar (lambda (s) (length (plist-get s :key)))
                                     (smear-cursor-menu-tests--suffixes)))))
    (dolist (row (aref (get 'smear-cursor-menu 'transient--layout) 2))
      (let ((pad (plist-get (smear-cursor-menu-tests--plist row) :pad-keys)))
        (should (and (integerp pad) (>= pad widest))))
      (dolist (column (aref row 2))
        (let ((own (plist-get (smear-cursor-menu-tests--plist column)
                              :pad-keys)))
          (should (or (null own) (and (integerp own) (>= own widest)))))))))

(ert-deftest smear-cursor-menu-test-a-command-starts-where-a-setting-does ()
  ;; GIVEN the lines for commands that have no value to show
  ;; WHEN they are drawn under settings
  ;; THEN their text starts in the column a setting's label starts in.
  ;;
  ;;      A setting's line opens with the column its star goes in, so a
  ;;      command line without that column sits a character to the left
  ;;      of everything above it.
  (let ((start (string-match "label" (smear-cursor-menu--line "label" "v"))))
    (dolist (command '(smear-cursor-menu-preview smear-cursor-menu-toggle
                       transient-quit-one))
      (should (= start (string-match "[^ ]" (smear-cursor-menu-tests--description
                                             command)))))))

(ert-deftest smear-cursor-menu-test-transients-own-save-key-keeps-the-settings ()
  ;; GIVEN transient's `C-x C-s', which saves a menu's value
  ;; WHEN it is pressed on this menu
  ;; THEN the settings are what gets saved.
  ;;
  ;;      What transient saves is a prefix's own value: the arguments a
  ;;      magit-style menu builds out of infixes.  This menu has none of
  ;;      those -- its entries set Emacs options there and then -- so
  ;;      left alone `C-x C-s' would write an empty value and look for
  ;;      all the world like it had kept something.
  (should (eq 'smear-cursor-menu-prefix
              (eieio-object-class (get 'smear-cursor-menu 'transient--prefix))))
  (let ((saved nil))
    (cl-letf (((symbol-function 'smear-cursor-menu--save)
               (lambda () (setq saved t))))
      (transient-save-value
       (make-instance 'smear-cursor-menu-prefix :command 'smear-cursor-menu))
      (should saved))))

(ert-deftest smear-cursor-menu-test-transients-set-key-has-nothing-to-do ()
  ;; GIVEN transient's `C-x s', which sets a value for this session
  ;; WHEN it is pressed on this menu
  ;; THEN it says so, and writes nothing.
  ;;
  ;;      Every key here already sets what it names, for this session.
  ;;      The one thing worth doing is keeping it, which is `C-x C-s'.
  (let ((transient-values nil)
        (said nil))
    (cl-letf (((symbol-function 'message) (lambda (&rest a) (setq said a))))
      (transient-set-value
       (make-instance 'smear-cursor-menu-prefix :command 'smear-cursor-menu)))
    (should said)
    (should-not transient-values)))

(ert-deftest smear-cursor-menu-test-the-level-is-on-a-key-of-its-own ()
  ;; GIVEN a menu whose fiddly half is held back by level
  ;; WHEN it is opened
  ;; THEN the key that reveals it is in the menu, showing the level.
  ;;
  ;;      `C-x l' is transient's own and works here as anywhere, but
  ;;      nothing on screen says so, and a reader of this menu has no
  ;;      reason to guess that half of it is hidden.  The rest of the
  ;;      `C-x' commands stay where they are: whether to list them is
  ;;      `transient-show-common-commands', which belongs to whoever
  ;;      owns the Emacs rather than to this package.
  (should (memq 'transient-set-level (smear-cursor-menu-tests--shown)))
  ;; the description reads outside a transient too, where there is no
  ;; level to report
  (let ((transient--prefix nil))
    (should-not (smear-cursor-menu--level))
    (should (smear-cursor-menu--line "detail level"
                                     (smear-cursor-menu--level-text)))))

(ert-deftest smear-cursor-menu-test-a-value-is-coloured-by-what-it-says ()
  ;; GIVEN a setting that is on and one that is off
  ;; WHEN the menu describes them
  ;; THEN the live value is coloured and the empty one is faded.
  ;;
  ;;      Fifty lines of one colour is a wall of text to read a value
  ;;      out of.  The value is the part being read, so it is the part
  ;;      that is coloured, and a setting doing nothing says so by
  ;;      being dim rather than by being read.
  (let ((smear-cursor-pad-line-ends t))
    (let ((line (smear-cursor-menu--describe 'smear-cursor-pad-line-ends "pad")))
      (should (eq 'smear-cursor-menu-value
                  (get-text-property (string-match "on" line) 'face line)))))
  (let ((smear-cursor-pad-line-ends nil))
    (let ((line (smear-cursor-menu--describe 'smear-cursor-pad-line-ends "pad")))
      (should (eq 'smear-cursor-menu-off
                  (get-text-property (string-match "off" line) 'face line)))))
  ;; a nil that means something other than off is faded the same way
  (let ((smear-cursor-color nil))
    (let ((line (smear-cursor-menu--describe 'smear-cursor-color "colour"
                                             "style's own")))
      (should (eq 'smear-cursor-menu-off
                  (get-text-property (string-match "style" line) 'face line)))))
  ;; and colour costs no width, or the columns would not line up
  (should (= (length (smear-cursor-menu--line "x" "y"))
             (length (smear-cursor-menu--line
                      "x" (propertize "y" 'face 'smear-cursor-menu-value))))))

(ert-deftest smear-cursor-menu-test-the-heading-says-how-things-stand ()
  ;; GIVEN the mode on, then off
  ;; WHEN the heading is drawn
  ;; THEN it is coloured to match, and the backend reads as a value.
  (let ((smear-cursor-mode t))
    (let ((s (smear-cursor-menu--state)))
      (should (eq 'smear-cursor-menu-value
                  (get-text-property (string-match "on" s) 'face s)))))
  (let ((smear-cursor-mode nil))
    (let ((s (smear-cursor-menu--state)))
      (should (eq 'smear-cursor-menu-off
                  (get-text-property (string-match "off" s) 'face s))))))

(ert-deftest smear-cursor-menu-test-an-unsaved-setting-is-starred ()
  ;; GIVEN a setting changed this session but not saved
  ;; WHEN the menu describes it
  ;; THEN it carries a star, and a saved one does not.
  ;;
  ;;      The count in the last column says how many are unsaved; the
  ;;      star says which, which is the half worth knowing when the
  ;;      answer is "three" and the menu has fifty lines.
  (let ((sym 'smear-cursor-fps))
    (unwind-protect
        (progn
          (put sym 'customized-value (list (custom-quote 60)))
          (let ((line (smear-cursor-menu--describe sym "fps")))
            (should (string-prefix-p smear-cursor-menu--mark line))
            (should (eq 'smear-cursor-menu-alert
                        (get-text-property 0 'face line))))
          (put sym 'customized-value nil)
          (let ((line (smear-cursor-menu--describe sym "fps")))
            (should-not (string-prefix-p smear-cursor-menu--mark line))))
      (put sym 'customized-value nil)))
  ;; the star takes a column of its own, so a starred line is no wider
  ;; than a plain one and the grid holds
  (should (= (length (smear-cursor-menu--line "x" "y"))
             (length (smear-cursor-menu--line "x" "y" nil t)))))

(ert-deftest smear-cursor-menu-test-every-setting-line-can-be-starred ()
  ;; GIVEN each kind of line that stands for a setting
  ;; WHEN that setting is changed and not saved
  ;; THEN every one of them is starred, not just the plain ones.
  ;;
  ;;      Which effect plays on an occasion is a setting like any
  ;;      other, and so are the trace file and the commands that
  ;;      pulse -- but each is drawn by its own suffix, and a star
  ;;      wired only into `smear-cursor-menu--describe' misses all
  ;;      three.  The one that is easiest to change from this menu was
  ;;      the one that could not show it.
  (dolist (case (list (list 'smear-cursor-effects
                            (lambda () (smear-cursor-menu--occasion-line
                                        'pulse "pulse")))
                      (list 'smear-cursor-trace
                            (lambda () (smear-cursor-menu--trace-line)))
                      (list 'smear-cursor-pulse-commands
                            (lambda () (smear-cursor-menu--commands-line)))))
    (let ((sym (nth 0 case)) (line (nth 1 case)))
      (unwind-protect
          (progn
            (put sym 'customized-value (list (custom-quote (symbol-value sym))))
            (should (string-prefix-p smear-cursor-menu--mark (funcall line)))
            (put sym 'customized-value nil)
            (should-not (string-prefix-p smear-cursor-menu--mark (funcall line))))
        (put sym 'customized-value nil)))))

(ert-deftest smear-cursor-menu-test-only-the-changed-occasion-is-starred ()
  ;; GIVEN five occasions that are five lines of one alist
  ;; WHEN one of them is changed
  ;; THEN that one is starred and the other four are not.
  ;;
  ;;      The unsaved thing really is the whole alist, so marking every
  ;;      line of it is honest and useless: a star is there to say
  ;;      which one was touched.
  (let ((smear-cursor-effects (list (cons 'pulse 'line-pulse)
                                    (cons 'copy 'region-flash)))
        (before (get 'smear-cursor-effects 'customized-value)))
    (unwind-protect
        (progn
          ;; already marked customized before the menu was ever used,
          ;; which is what a session that changed one earlier looks
          ;; like: with nothing recorded to compare against, nothing is
          ;; starred rather than everything
          (put 'smear-cursor-effects 'customized-value '(nil))
          (smear-cursor-menu--effects-forget)
          (should-not (string-prefix-p
                       smear-cursor-menu--mark
                       (smear-cursor-menu--occasion-line 'pulse "pulse")))
          (smear-cursor-menu--set-occasion 'pulse 'line-scan)
          (should (string-prefix-p
                   smear-cursor-menu--mark
                   (smear-cursor-menu--occasion-line 'pulse "pulse")))
          (should-not (string-prefix-p
                       smear-cursor-menu--mark
                       (smear-cursor-menu--occasion-line 'copy "copy")))
          ;; saving clears them: nothing is outstanding any more
          (put 'smear-cursor-effects 'customized-value nil)
          (should-not (string-prefix-p
                       smear-cursor-menu--mark
                       (smear-cursor-menu--occasion-line 'pulse "pulse"))))
      (put 'smear-cursor-effects 'customized-value before)
      (smear-cursor-menu--effects-forget))))

(ert-deftest smear-cursor-menu-test-the-ghost-count-shows-its-limit ()
  ;; GIVEN a setting with a limit
  ;; WHEN the menu draws it and reads a new value for it
  ;; THEN the line says what the limit is and the value stays inside it.
  ;;
  ;;      A number with a ceiling that only the drawing knows about is
  ;;      a setting you can put four hundred in and see three.  The
  ;;      line says "2 of 4", and asking for more gives the four that
  ;;      there are.
  (let ((smear-cursor-pacman-ghosts 2))
    (should (string-match-p (format "2 of %d" smear-cursor--pacman-most-ghosts)
                            (substring-no-properties
                             (smear-cursor-menu--describe-of
                              'smear-cursor-pacman-ghosts "ghosts"
                              smear-cursor--pacman-most-ghosts "none")))))
  ;; none is still none, not "0 of 4"
  (let ((smear-cursor-pacman-ghosts 0))
    (should (string-match-p "none"
                            (substring-no-properties
                             (smear-cursor-menu--describe-of
                              'smear-cursor-pacman-ghosts "ghosts"
                              smear-cursor--pacman-most-ghosts "none")))))
  ;; and what is read is held inside the limit
  (let ((smear-cursor-pacman-ghosts 1))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "99")))
      (smear-cursor-menu--read-number 'smear-cursor-pacman-ghosts "Ghosts"
                                      smear-cursor--pacman-most-ghosts))
    (should (= smear-cursor--pacman-most-ghosts smear-cursor-pacman-ghosts))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "-3")))
      (smear-cursor-menu--read-number 'smear-cursor-pacman-ghosts "Ghosts"
                                      smear-cursor--pacman-most-ghosts))
    (should (= 0 smear-cursor-pacman-ghosts))))

(ert-deftest smear-cursor-menu-test-the-glow-shows-what-is-being-typed ()
  ;; GIVEN a number being read for one of the glow's settings
  ;; WHEN a value is typed
  ;; THEN the setting takes it there and then, and the glow is played
  ;;      again so it can be seen.
  ;;
  ;;      A number for a thing you are looking at is guesswork until
  ;;      you see it: the glow is on the screen while the menu is open,
  ;;      so it may as well answer.
  (let ((replayed 0))
    (cl-letf (((symbol-function 'smear-cursor--rest-play)
               (lambda (&rest _) (setq replayed (1+ replayed))))
              ((symbol-function 'smear-cursor--rest-forget) #'ignore)
              ((symbol-function 'minibuffer-contents)
               (lambda () "0.35")))
      (let ((smear-cursor-menu-live-preview t)
            (smear-cursor-rest-strength 1.0)
            (smear-cursor-menu--previewed nil))
        (smear-cursor-menu--preview-glow 'smear-cursor-rest-strength)
        (should (= 0.35 smear-cursor-rest-strength))
        (should (= 1 replayed))
        ;; the same text again is not a second play
        (smear-cursor-menu--preview-glow 'smear-cursor-rest-strength)
        (should (= 1 replayed)))
      ;; and half a number is not a number
      (cl-letf (((symbol-function 'minibuffer-contents) (lambda () "0.")))
        (let ((smear-cursor-menu-live-preview t)
              (smear-cursor-rest-strength 1.0)
              (smear-cursor-menu--previewed nil))
          (smear-cursor-menu--preview-glow 'smear-cursor-rest-strength)
          (should (= 1.0 smear-cursor-rest-strength))))
      ;; nor is anything at all when the preview is turned off
      (cl-letf (((symbol-function 'minibuffer-contents) (lambda () "2.0")))
        (let ((smear-cursor-menu-live-preview nil)
              (smear-cursor-rest-strength 1.0)
              (smear-cursor-menu--previewed nil))
          (smear-cursor-menu--preview-glow 'smear-cursor-rest-strength)
          (should (= 1.0 smear-cursor-rest-strength)))))))

(ert-deftest smear-cursor-menu-test-a-glow-preview-puts-the-setting-back ()
  ;; GIVEN a value tried out in the preview
  ;; WHEN the read is quit rather than finished
  ;; THEN the setting is as it was, and the glow with it.
  (let ((smear-cursor-rest-strength 0.4)
        (smear-cursor-rest-effect nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _)
                 (setq smear-cursor-rest-strength 9.0)   ; as the preview would
                 (signal 'quit nil)))
              ((symbol-function 'smear-cursor--rest-setup) #'ignore)
              ((symbol-function 'smear-cursor--rest-play) #'ignore))
      (smear-cursor-menu--read-glow 'smear-cursor-rest-strength "Strength"))
    (should (= 0.4 smear-cursor-rest-strength))
    ;; and the glow is left as it was found
    (should-not smear-cursor-rest-effect)))

(ert-deftest smear-cursor-menu-test-a-number-prompt-says-what-it-ships-as ()
  ;; GIVEN a setting moved away from what it ships as
  ;; WHEN the menu asks for a new value
  ;; THEN the prompt offers the value it has as the default, and says
  ;;      the shipped one so there is a way back.
  ;;
  ;;      Pressing return keeps what you have, which is what a default
  ;;      is for; but a number you have been tuning for ten minutes is
  ;;      one you can no longer undo without going to `customize'.
  (let ((smear-cursor-rest-strength 0.35))
    (should (string-match-p "ships 1\\.0"
                            (smear-cursor-menu--number-prompt
                             'smear-cursor-rest-strength "Strength"))))
  ;; and says nothing extra when it is at the shipped value
  (let ((smear-cursor-rest-strength
         (eval (car (get 'smear-cursor-rest-strength 'standard-value)) t)))
    (should-not (string-match-p "ships"
                                (smear-cursor-menu--number-prompt
                                 'smear-cursor-rest-strength "Strength"))))
  ;; a setting with no shipped value of its own says nothing either
  (let ((sym (make-symbol "smear-cursor-test-not-a-custom")))
    (set sym 3)
    (should-not (string-match-p "ships"
                                (smear-cursor-menu--number-prompt sym "Thing")))))

(ert-deftest smear-cursor-menu-test-the-value-is-in-the-line-not-in-a-default ()
  ;; GIVEN a number being read for a setting
  ;; WHEN the prompt is put together
  ;; THEN the value it has is in the input line and the shipped one is
  ;;      named, with no second number called a default.
  ;;
  ;;      `read-number' writes its own "(default N)", where N is what
  ;;      return keeps.  Beside a prompt naming the shipped value that
  ;;      is two numbers, one of them under a word that in Emacs
  ;;      usually means the other one.
  (let ((smear-cursor-rest-strength 0.35)
        (seen nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &optional initial &rest _)
                 (setq seen (list prompt initial))
                 "0.6")))
      (should (= 0.6 (smear-cursor-menu--read-a-number
                      'smear-cursor-rest-strength "Strength"))))
    (should (equal "0.35" (nth 1 seen)))
    (should (string-match-p "ships 1\\.0" (nth 0 seen)))
    (should-not (string-match-p "default" (nth 0 seen))))
  ;; and anything that is not a number leaves the setting alone
  (let ((smear-cursor-rest-strength 0.35))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  ")))
      (should (= 0.35 (smear-cursor-menu--read-a-number
                       'smear-cursor-rest-strength "Strength"))))))

(ert-deftest smear-cursor-menu-test-an-effect-in-use-brings-its-settings ()
  ;; GIVEN an effect assigned to something, and one not
  ;; WHEN the menu asks whether to show its settings
  ;; THEN only the one in use says yes.
  ;;
  ;;      Half the menu is knobs for effects nobody has turned on.
  ;;      Assigned to an occasion, chosen for the idle spell, worn by
  ;;      the cursor -- any of those is in use; named nowhere is not.
  (let ((smear-cursor-effects '((pulse . line-pulse) (insert . lightning)))
        (smear-cursor-idle-effect nil)
        (smear-cursor-rest-effect nil))
    (should (smear-cursor-menu--using-p 'lightning))
    (should (smear-cursor-menu--using-p 'line-pulse))
    (should-not (smear-cursor-menu--using-p 'fire))
    (should-not (smear-cursor-menu--using-p 'plasma)))
  ;; the idle effect counts
  (let ((smear-cursor-effects nil)
        (smear-cursor-idle-effect 'pacman)
        (smear-cursor-rest-effect nil))
    (should (smear-cursor-menu--using-p 'pacman))
    (should-not (smear-cursor-menu--using-p 'lightning)))
  ;; and so does what the cursor wears
  (let ((smear-cursor-effects nil)
        (smear-cursor-idle-effect nil)
        (smear-cursor-rest-effect 'cursor-rest))
    (should (smear-cursor-menu--using-p 'cursor-rest))))

(ert-deftest smear-cursor-menu-test-the-menu-hides-what-is-not-in-use ()
  ;; GIVEN the menu's layout
  ;; WHEN the groups of effect settings are read
  ;; THEN each carries the question of whether its effect is in use.
  ;;
  ;;      Asked as a predicate rather than baked in, because the answer
  ;;      changes while the menu is open: choose `lightning' for typing
  ;;      and its settings should be there when the menu redraws.
  (let ((groups (aref (get 'smear-cursor-menu 'transient--layout) 2))
        (asking 0))
    (letrec ((walk (lambda (node)
                     (when (vectorp node)
                       (let ((plist (and (> (length node) 1)
                                         (let ((x (aref node 1)))
                                           (and (consp x) (keywordp (car x)) x)))))
                         (when (plist-get plist :if) (setq asking (1+ asking))))
                       (mapc walk (append node nil)))
                     (when (consp node)
                       (if (and (symbolp (car node)) (not (keywordp (car node)))
                                (plist-member (cdr node) :key))
                           (when (plist-get (cdr node) :if)
                             (setq asking (1+ asking)))
                         (mapc walk node))))))
      (mapc walk groups))
    ;; the columns for lightning, sparks and fire and the line scan,
    ;; and the settings gated one at a time: the glow's four, Pacman's
    ;; three, the pulse's three, the blink's three
    (should (>= asking 10))))

(ert-deftest smear-cursor-menu-test-the-menu-notices-a-setting-it-just-changed ()
  ;; GIVEN a menu whose columns depend on what is in use
  ;; WHEN one of its own keys puts an effect into use
  ;; THEN the columns are worked out again rather than left as they
  ;;      were when the menu opened.
  ;;
  ;;      Transient filters the layout when the prefix is set up, so
  ;;      `:if' was asked once: setting lightning on the return key
  ;;      left its settings hidden until the menu was closed and
  ;;      opened again.
  (should (oref (get 'smear-cursor-menu 'transient--prefix) refresh-suffixes)))

(ert-deftest smear-cursor-menu-test-g-is-left-for-what-it-usually-means ()
  ;; GIVEN a menu in an editor where `g' reverts or refreshes
  ;; WHEN its keys are read
  ;; THEN nothing is on `g'.
  ;;
  ;;      Turning the mode off is the last thing somebody wants from a
  ;;      key they press to redraw a buffer, and the muscle memory
  ;;      comes from dired, magit and half of Emacs besides.
  (let ((keys (delq nil (mapcar (lambda (s) (plist-get s :key))
                                (smear-cursor-menu-tests--suffixes)))))
    (should-not (member "g" keys))
    ;; the mode is still one key away
    (should (member "M" keys))))
