;;; run-x.el --- run the tests on a real frame -*- lexical-binding: t -*-

;; The drawing tests need a graphical frame, which batch mode cannot
;; make, and `ert-run-tests-batch-and-exit' runs only in batch.  Run them
;; here instead, write the result out, then leave.

(defun smear-cursor-x11-run-x-tests ()
  (let ((out nil) (bad 0))
    (dolist (test (ert-select-tests t t))
      (let* ((name (ert-test-name test))
             (res (ert-run-test test))
             (kind (cond ((ert-test-passed-p res) "PASS")
                         ((ert-test-skipped-p res) "skip")
                         (t (setq bad (1+ bad)) "FAIL"))))
        (push (format "%-6s %s%s" kind name
                      (if (string= kind "FAIL")
                          (format "\n         %S"
                                  (ert-test-result-with-condition-condition res))
                        ""))
              out)))
    (push (format "\n%d failed" bad) out)
    (with-temp-file "/tmp/smear-cursor-x11-results.txt"
      (insert (mapconcat #'identity (nreverse out) "\n") "\n"))
    (kill-emacs (if (> bad 0) 1 0))))

(smear-cursor-x11-run-x-tests)
