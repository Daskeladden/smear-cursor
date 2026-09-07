EMACS ?= emacs

.PHONY: test test-x x11 demo compile build clean

# Everything that runs without a display, the performance budgets
# among them: what a flight costs is decided when it is laid out, so it
# is arithmetic rather than a stopwatch and needs no screen.  The x11 backend's drawing
# tests skip here: batch mode cannot create an X frame ("Unknown
# terminal type"), so there is nothing for them to draw on.
test:
	$(EMACS) -Q --batch -L . -L tests \
	  -l tests/smear-cursor-tests.el \
	  -l tests/smear-cursor-x11-tests.el \
	  -l tests/smear-cursor-menu-tests.el \
	  -l tests/smear-cursor-perf-tests.el \
	  --eval '(ert-run-tests-batch-and-exit)'

# The drawing tests too, on a real frame.  Opens a window for a moment.
# Not `ert-run-tests-batch-and-exit': it refuses to run outside batch
# mode, and batch cannot make the frame -- so between them these tests
# have nowhere to run.  tests/run-x.el reports through a file instead.
test-x: x11
	$(EMACS) -Q -L . -L tests -l tests/smear-cursor-x11-tests.el \
	  -l tests/run-x.el 2>/dev/null; cat /tmp/smear-cursor-x11-results.txt

# The X compositing backend's module, and its standalone demo.
x11:
	$(MAKE) -C x11

demo:
	$(MAKE) -C x11 demo

# Byte-compile the Lisp.  Not a nicety: measured at 300 keystrokes,
# marking each one costs 0.28 ms compiled and four to eight
# milliseconds as source -- and that is on the thread that also runs
# redisplay, so it is felt as the editor hesitating.  A package loaded
# from a working copy with `:load-path' is source unless someone
# compiles it.
compile:
	$(EMACS) -Q --batch -L . --eval '(byte-compile-file "smear-cursor.el")'
	$(EMACS) -Q --batch -L . --eval '(byte-compile-file "smear-cursor-x11.el")'
	$(EMACS) -Q --batch -L . --eval '(byte-compile-file "smear-cursor-menu.el")'

# Everything a working copy needs to be fast: the module and the
# bytecode.  Run it after pulling or editing.
build: x11 compile

clean:
	rm -f *.elc tests/*.elc
	$(MAKE) -C x11 clean
