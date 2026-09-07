# The X compositing backend's module, and a standalone demo of it.
#
# The module is what `smear-cursor-backend' set to `x11' loads.  The
# demo needs no Emacs at all and is the quickest way to find out
# whether a display can do this: it prints what kind of X it found and
# what, if anything, is missing.

CFLAGS   ?= -O2 -Wall -Wextra
# emacs-module.h ships with Emacs; on Debian-ish boxes built from
# source it lands under /usr/local.  The EGL and GLES headers come from
# libegl-dev and libgles-dev; see the README's Requirements.
INCLUDES ?= -I/usr/local/include
# libGL/libEGL ship without dev symlinks on some boxes; the
# versioned names are what is actually there.
LDLIBS    = -lX11 -lXrender -lXfixes -lXext -lm -lpthread \
            -l:libEGL.so.1 -l:libGL.so.1
CORE      = smear-cursor-x11-core.c smear-cursor-x11-core.h \
            smear-cursor-x11-gl.c smear-cursor-x11-gl.h \
            smear-cursor-x11-play.c smear-cursor-x11-play.h

all: smear-cursor-x11-module.so

smear-cursor-x11-module.so: smear-cursor-x11-module.c $(CORE)
	$(CC) $(CFLAGS) $(INCLUDES) -fPIC -shared -o $@ \
	  smear-cursor-x11-module.c smear-cursor-x11-core.c \
	  smear-cursor-x11-gl.c smear-cursor-x11-play.c $(LDLIBS)

demo: demo.c $(CORE)
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ demo.c smear-cursor-x11-core.c \
	  smear-cursor-x11-gl.c smear-cursor-x11-play.c $(LDLIBS)

# The GL renderer's geometry, checked against a display rather than
# reasoned about.  Needs $DISPLAY and a working EGL; skips without.
smear-cursor-x11-test: smear-cursor-x11-test.c $(CORE)
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ smear-cursor-x11-test.c \
	  smear-cursor-x11-core.c smear-cursor-x11-gl.c \
	  smear-cursor-x11-play.c $(LDLIBS)

check: smear-cursor-x11-test
	./smear-cursor-x11-test

# Watch the overlay from outside Emacs: whether it went up at all, and
# what its shape let through.  Everything else here reports what we
# asked the server for; this reports what the server has.  Needs no
# Emacs and no module -- run it beside a session and move the cursor.
#
#   ./smear-cursor-x11-watch 60 20    # 60 samples a second, 20 seconds
watch: smear-cursor-x11-watch smear-cursor-x11-watchdog

smear-cursor-x11-watch: smear-cursor-x11-watch.c
	$(CC) $(CFLAGS) -o $@ smear-cursor-x11-watch.c -lX11 -lXext

# Runs from before the fault until after it, writing only when
# something changes, so nobody has to do anything at the moment it
# matters -- and asking is itself a focus change and a restack, which
# are what the fault turns on.
smear-cursor-x11-watchdog: smear-cursor-x11-watchdog.c
	$(CC) $(CFLAGS) -o $@ smear-cursor-x11-watchdog.c -lX11 -lXext

# What the overlay window itself holds, as an image.  The watch tool
# says whether the overlay is up and what its shape lets through; a
# screen grab cannot answer for an override-redirect window under a
# compositor.  This reads the window's own pixels, which separates
# "never drawn" from "drawn and not presented".
shot: smear-cursor-x11-shot
smear-cursor-x11-shot: smear-cursor-x11-shot.c
	$(CC) $(CFLAGS) -o $@ smear-cursor-x11-shot.c -lX11

clean:
	rm -f smear-cursor-x11-module.so demo smear-cursor-x11-test \
	      smear-cursor-x11-watch smear-cursor-x11-watchdog \
	      smear-cursor-x11-shot

.PHONY: all check clean demo watch shot
