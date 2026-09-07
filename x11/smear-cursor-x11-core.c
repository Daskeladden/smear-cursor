/* See smear-cursor-x11-core.h. */
#define _POSIX_C_SOURCE 200809L
#include "smear-cursor-x11-core.h"
#include "smear-cursor-x11-gl.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/extensions/Xrender.h>
#include <X11/extensions/Xfixes.h>
#include <X11/extensions/shape.h>
#include <dlfcn.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* -------------------------------------------------- Composite, dlopened */

/* The target window's own contents, kept current by the server and
 * free of whatever is stacked on top, our overlay included.
 *
 * Without this the background has to be saved and put back, and both
 * halves of that are wrong.  Unmapping the overlay only *damages* what
 * is underneath, so copying straight afterwards captures the damage,
 * which is black; and on a forwarded connection the repaint is the
 * other client's to do, so no amount of XSync helps.  A saved copy
 * then goes stale the moment the text under it changes.  Both faults
 * look the same on screen: black blocks stamped over the buffer.
 *
 * dlopen because the headers are not always installed and the
 * fallback still works without them.  Redirection is released when the
 * client disconnects, so a crash always restores the display.
 */
#define REDIRECT_AUTOMATIC 0

static Bool   (*p_query)(Display *, int *, int *);
static void   (*p_redirect)(Display *, Window, int);
static void   (*p_unredirect)(Display *, Window, int);
static Pixmap (*p_name_pixmap)(Display *, Window);
static int     composite_tried;

static int composite_load(Display *d)
{
    if (!composite_tried) {
        composite_tried = 1;
        void *lib = dlopen("libXcomposite.so.1", RTLD_LAZY);
        if (!lib) lib = dlopen("libXcomposite.so", RTLD_LAZY);
        if (lib) {
            p_query       = dlsym(lib, "XCompositeQueryExtension");
            p_redirect    = dlsym(lib, "XCompositeRedirectWindow");
            p_unredirect  = dlsym(lib, "XCompositeUnredirectWindow");
            p_name_pixmap = dlsym(lib, "XCompositeNameWindowPixmap");
        }
    }
    if (!p_query || !p_redirect || !p_unredirect || !p_name_pixmap) return 0;
    int ev, err;
    return p_query(d, &ev, &err) ? 1 : 0;
}

/* --------------------------------------------------------- X errors */

/* Xlib error handlers are per *process*, not per connection.  So a
 * BadDrawable on our own display is delivered to whatever handler the
 * host installed.  Inside Emacs that is Emacs's, which reports it as
 * an Emacs bug and asks the user to mail bug-gnu-emacs.  Asking for a
 * window that has gone away is an ordinary thing to happen here, not a
 * bug in the editor.
 *
 * So: take the handler, answer for the displays we opened, and hand
 * everything else to whoever had it before.
 */
#define MAX_GUARDED 8
static Display *guarded[MAX_GUARDED];
static int      guarded_err[MAX_GUARDED];
static int (*prev_error_handler)(Display *, XErrorEvent *);
static int handler_installed;

/* Per display, not one global.  Closing a connection can deliver an
 * error that was still in flight, and with a single flag that error
 * lands on whichever stage asks next, which then says a perfectly
 * good window "is not on this display". */
static int on_x_error(Display *d, XErrorEvent *e)
{
    for (int i = 0; i < MAX_GUARDED; i++)
        if (guarded[i] == d) { guarded_err[i] = e->error_code; return 0; }
    return prev_error_handler ? prev_error_handler(d, e) : 0;
}

static void guard_display(Display *d)
{
    if (!handler_installed) {
        prev_error_handler = XSetErrorHandler(on_x_error);
        handler_installed = 1;
    }
    for (int i = 0; i < MAX_GUARDED; i++)
        if (!guarded[i]) { guarded[i] = d; guarded_err[i] = 0; return; }
}

static void unguard_display(Display *d)
{
    for (int i = 0; i < MAX_GUARDED; i++)
        if (guarded[i] == d) { guarded[i] = NULL; guarded_err[i] = 0; }
}

/* Reset, do the thing, then ask.  Checking without clearing first
 * reports whatever went wrong last. */
static void clear_errors(Display *d)
{
    XSync(d, False);
    for (int i = 0; i < MAX_GUARDED; i++)
        if (guarded[i] == d) guarded_err[i] = 0;
}

static int took_error(Display *d)
{
    XSync(d, False);
    for (int i = 0; i < MAX_GUARDED; i++)
        if (guarded[i] == d) {
            int e = guarded_err[i];
            guarded_err[i] = 0;
            return e;
        }
    return 0;
}

/* ------------------------------------------------------------ the stage */

struct SmearStage {
    Display *d;
    Window   root, target, ov;
    GC       gc;
    Picture  dst;
    /* A frame is composed here and copied to the overlay when it is
     * finished, rather than drawn into the window a piece at a time.
     *
     * The first thing a frame does is lay the window's own text down
     * as its backdrop, and the effect goes over that.  Drawn straight
     * into the overlay, the window therefore holds the live text for
     * the moment between the two, and a compositor that samples in
     * that moment presents it.  A trail over the same text hides
     * nothing and never showed it; a band whose whole job is to cover
     * text flickered, at random, because it depends on the
     * compositor's timing rather than on ours. */
    Pixmap   buf;
    Picture  bufpic;
    Pixmap   wpix;                 /* target's live contents */
    XRenderPictFormat *a8;
    Visual  *visual;
    unsigned depth;
    int      tx, ty;               /* target's origin on the root */
    unsigned tw, th;               /* and its size */
    int      w, h;                 /* the overlay's size */
    int      mapped, redirected, own_display;
    int      last_x, last_y, last_w, last_h;
    int      can_blur;            /* 0 unknown, 1 yes, -1 no */
    int      have_fixes;
    int      pending_show;        /* up on this flight's first frame */
    int      shape_x, shape_y, shape_w, shape_h;
    int      gl_x, gl_y, gl_w, gl_h;   /* what the next flush covers */
    int      renderer;
    SmearPaint batch[SMEAR_MAX_LAYERS];  /* held for one GL pass */
    int      nbatch;
    char     trouble[192];
    char     describe[192];
    void    *play;            /* the player thread's state, or NULL */
    /* A photograph of some text, kept while an effect marks it.  See
     * smear_stage_freeze. */
    Pixmap   frozen;
    int      frozen_w, frozen_h;              /* what the pixmap holds */
    int      fx0, fy0, fw, fh;                /* and where it came from */
    /* One flight's pixels, kept while it plays.  See
     * smear_stage_stamp_take. */
    struct {
        Pixmap  pm;
        Picture pic;
        int     x, y, w, h;
    } stamp[SMEAR_STAGE_STAMPS];
    Picture  amask;           /* 1x1 repeating A8, for a constant alpha */
    double   resynced;        /* when the target's geometry was last asked */
};

void **smear_stage_play_slot(SmearStage *st) { return st ? &st->play : NULL; }

/* How many times the renderer has read a box back off the GPU and
 * pushed it to the display.  The one number that decides what an
 * animation costs on a display reached over a network, and the only
 * way to check that a still flight uploads once rather than once a
 * frame. */
static unsigned long smear_uploads;
unsigned long smear_stage_uploads(void) { return smear_uploads; }

/* How many times the overlay has been put on screen.  A map is the
 * only thing some displays honour for getting on top, and it costs
 * 33.6 ms on a forwarded one, so this is what a flight costs before
 * it has drawn anything, and it should be once a burst of typing
 * rather than once a keystroke. */
static unsigned long smear_maps;
unsigned long smear_stage_maps(void) { return smear_maps; }

static XFixed fx(double v) { return XDoubleToFixed(v); }

/* Re-read where the target is.  A resize invalidates the name pixmap
 * (it names the contents at the old size), and a move would leave the
 * overlay against a stale origin, which is the right text in the wrong
 * place. */
/* How often the target's geometry is worth asking for, in seconds.
 *
 * Asking is round trips, and a round trip is the expensive thing here:
 * measured over ssh -X, one `XGetWindowAttributes' takes 52 ms while
 * copying the whole 1920 by 1027 window takes 2.7.  Asking once a
 * flight meant asking once a keystroke, which is where typing lag came
 * from.  It was never the pixels.
 *
 * A frame that moves or resizes is therefore drawn against a stale
 * origin for up to this long.  Both are things a person does with the
 * mouse and then looks at, not things that happen mid-keystroke. */
#define RESYNC_EVERY 1.0

static double stage_now(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + ts.tv_nsec / 1e9;
}

static void resync(SmearStage *st)
{
    Window rr; int gx, gy; unsigned gw, gh, bw, gd;
    if (!XGetGeometry(st->d, st->target, &rr, &gx, &gy, &gw, &gh, &bw, &gd))
        return;
    int ax, ay; Window kid;
    XTranslateCoordinates(st->d, st->target, st->root, 0, 0, &ax, &ay, &kid);
    int resized = (gw != st->tw || gh != st->th);
    st->tw = gw; st->th = gh; st->tx = ax; st->ty = ay;
    if (resized && st->redirected) {
        if (st->wpix) XFreePixmap(st->d, st->wpix);
        st->wpix = p_name_pixmap(st->d, st->target);
    }
}

SmearStage *smear_stage_open(const char *display, unsigned long window)
{
    Display *d = XOpenDisplay(display);
    if (!d) return NULL;

    SmearStage *st = calloc(1, sizeof *st);
    if (!st) { XCloseDisplay(d); return NULL; }
    guard_display(d);
    st->d = d;
    st->own_display = 1;
    st->root = DefaultRootWindow(d);
    st->target = (Window)window;

    int ev, err;
    int have_render = XRenderQueryExtension(d, &ev, &err);
    int have_fixes  = XFixesQueryExtension(d, &ev, &err);
    st->have_fixes = have_fixes;
    int have_comp   = composite_load(d);

    int op, e1, e2;
    int xwayland = XQueryExtension(d, "XWAYLAND", &op, &e1, &e2);
    snprintf(st->describe, sizeof st->describe, "%s (%s), %s",
             DisplayString(d), ServerVendor(d),
             xwayland ? "XWayland" : "a real X server");

    if (!have_render)
        snprintf(st->trouble, sizeof st->trouble,
                 "this display has no RENDER extension");
    else if (!have_comp)
        snprintf(st->trouble, sizeof st->trouble,
                 "this display has no Composite extension (or "
                 "libXcomposite is not installed)");

    Window rr; int gx, gy; unsigned gw, gh, bw, gd;
    clear_errors(d);
    int ok = XGetGeometry(d, st->target, &rr, &gx, &gy, &gw, &gh, &bw, &gd);
    if (!ok || took_error(d)) {
        snprintf(st->trouble, sizeof st->trouble,
                 "window 0x%lx is not on this display", (unsigned long)window);
        return st;
    }
    st->depth = gd;
    st->visual = DefaultVisual(d, DefaultScreen(d));
    st->a8 = XRenderFindStandardFormat(d, PictStandardA8);
    st->gc = XCreateGC(d, st->target, 0, NULL);
    resync(st);

    if (st->trouble[0]) return st;

    p_redirect(d, st->target, REDIRECT_AUTOMATIC);
    XSync(d, False);
    st->redirected = 1;
    st->wpix = p_name_pixmap(d, st->target);
    if (!st->wpix) {
        snprintf(st->trouble, sizeof st->trouble,
                 "the server gave no pixmap for window 0x%lx",
                 (unsigned long)window);
        return st;
    }

    XSetWindowAttributes at;
    at.override_redirect = True;   /* a tiling WM must not manage this */
    at.background_pixmap = None;
    /* No events at all.  Nothing here needs to hear from the server,
     * and a connection that leaves events unread blocks the server
     * sending them.  With `SubstructureNotifyMask' on the root for a
     * while, that stalled every client on the display for seconds. */
    at.event_mask = 0;
    st->ov = XCreateWindow(d, st->root, st->tx, st->ty, 1, 1, 0,
                           st->depth, InputOutput, CopyFromParent,
                           CWOverrideRedirect | CWBackPixmap | CWEventMask,
                           &at);
    /* Named so a compositing manager can be told to leave it alone:
     * picom composites override-redirect windows like any other, and
     * will put a shadow round the trail and fade it in and out over
     * the top of the animation already running. */
    {
        XClassHint ch;
        char nm[] = "smear-cursor", cl[] = "SmearCursorX11";
        ch.res_name = nm; ch.res_class = cl;
        XSetClassHint(d, st->ov, &ch);
        XStoreName(d, st->ov, "smear-cursor overlay");
        Atom type = XInternAtom(d, "_NET_WM_WINDOW_TYPE", False);
        Atom dnd = XInternAtom(d, "_NET_WM_WINDOW_TYPE_DND", False);
        XChangeProperty(d, st->ov, type, XA_ATOM, 32, PropModeReplace,
                        (unsigned char *)&dnd, 1);
    }
    if (have_fixes) {              /* it must never take a click */
        XserverRegion empty = XFixesCreateRegion(d, NULL, 0);
        XFixesSetWindowShapeRegion(d, st->ov, ShapeInput, 0, 0, empty);
        XFixesDestroyRegion(d, empty);
    }
    return st;
}

const char *smear_stage_trouble(SmearStage *st)
{
    if (!st) return "no stage";
    return st->trouble[0] ? st->trouble : NULL;
}

const char *smear_stage_describe(SmearStage *st)
{
    return st ? st->describe : "";
}

int smear_stage_begin(SmearStage *st)
{
    if (!st || st->trouble[0]) return 0;
    /* Not every time.  See RESYNC_EVERY: this is round trips, and
     * round trips are what an effect on every keystroke cannot
     * afford. */
    double now = stage_now();
    if (!st->mapped || now - st->resynced > RESYNC_EVERY) {
        resync(st);
        st->resynced = now;
    }

    /* Name the window's contents afresh every time the overlay goes
     * up, not only when the frame has been resized.
     *
     * `XCompositeNameWindowPixmap' hands back an id for the pixmap
     * holding the window's contents *now*; the server is free to stop
     * backing that id at any point, and a resize is only the most
     * obvious way.  A copy from a stale one raises BadDrawable and
     * draws nothing, so the overlay keeps whatever it had, which
     * moments after being mapped is undefined and looks black.  A
     * trail composited over that is a red dot in a black box, which is
     * how this was reported.
     *
     * The call makes an id on this side and sends one request; it does
     * not wait for the server, so doing it once a flight costs
     * nothing worth measuring. */
    if (st->redirected) {
        Pixmap fresh = p_name_pixmap(st->d, st->target);
        if (fresh) {
            if (st->wpix) XFreePixmap(st->d, st->wpix);
            st->wpix = fresh;
        }
    }

    /* The overlay is the whole window and never moves.
     *
     * Sizing it to the trail meant moving it whenever the trail left
     * its region, and XMoveResizeWindow on an already-mapped window
     * puts it at the new place still holding its old contents, because
     * the copy that fixes that is a separate request.  Until the
     * server gets there the screen shows text from where the overlay
     * just was. */
    if (st->w != (int)st->tw || st->h != (int)st->th) {
        if (st->mapped) { XUnmapWindow(st->d, st->ov); st->mapped = 0; }
        XMoveResizeWindow(st->d, st->ov, st->tx, st->ty, st->tw, st->th);
        st->w = st->tw; st->h = st->th;
        if (st->dst) { XRenderFreePicture(st->d, st->dst); st->dst = 0; }
        if (st->bufpic) { XRenderFreePicture(st->d, st->bufpic); st->bufpic = 0; }
        if (st->buf) { XFreePixmap(st->d, st->buf); st->buf = 0; }
    }
    if (!st->buf) {
        st->buf = XCreatePixmap(st->d, st->root, st->w, st->h, st->depth);
        st->bufpic = st->buf
            ? XRenderCreatePicture(st->d, st->buf,
                                   XRenderFindVisualFormat(st->d, st->visual),
                                   0, NULL)
            : 0;
    }
    if (!st->dst)
        st->dst = st->bufpic
            ? st->bufpic
            : XRenderCreatePicture(
                st->d, st->ov, XRenderFindVisualFormat(st->d, st->visual),
                0, NULL);
    /* A whole window copy costs 127 ms on a 1920 by 1027 frame, about
     * two million pixels.  No copy is needed here: the overlay is hidden
     * until a frame sets its shape, and `smear_stage_frame_begin' paints
     * the backdrop across the entire visible area every frame. */
    st->shape_w = st->shape_h = 0;
    st->nbatch = 0;
    if (st->have_fixes) {
        XserverRegion none = XFixesCreateRegion(st->d, NULL, 0);
        XFixesSetWindowShapeRegion(st->d, st->ov, ShapeBounding, 0, 0, none);
        XFixesDestroyRegion(st->d, none);
    }
    /* Not up yet.  The first frame of this flight puts it up, once it
     * has said what shape the overlay is.  `smear_stage_frame_begin'
     * says why the order matters.  From here on the stage counts
     * itself as up: it is this flight's, and nothing else may draw. */
    st->mapped = 1;
    st->pending_show = 1;
    st->last_w = st->last_h = 0;
    return 1;
}

/* Can this server blur for us?  Asked once; a display without the
 * convolution filter simply draws hard-edged layers. */
int smear_stage_set_renderer(SmearStage *st, int which, char *err, size_t errlen)
{
    if (!st) return SMEAR_RENDERER_RENDER;
    if (which == SMEAR_RENDERER_GL) {
        if (smear_gl_init(err, errlen) == 0) {
            st->renderer = SMEAR_RENDERER_GL;
            return SMEAR_RENDERER_GL;
        }
        /* the caller is told why, and gets the renderer that works */
    }
    st->renderer = SMEAR_RENDERER_RENDER;
    return SMEAR_RENDERER_RENDER;
}

int smear_stage_renderer(SmearStage *st)
{
    return st ? st->renderer : SMEAR_RENDERER_RENDER;
}

int smear_stage_can_blur(SmearStage *st)
{
    if (!st || st->trouble[0]) return 0;
    if (st->can_blur == 0) {
        st->can_blur = -1;
        XFilters *f = XRenderQueryFilters(st->d, st->root);
        if (f) {
            for (int i = 0; i < f->nfilter; i++)
                if (!strcmp(f->filter[i], "convolution")) { st->can_blur = 1; break; }
            XFree(f);
        }
    }
    return st->can_blur > 0;
}

/* Show only the part of the overlay the trail is in, and grow that
 * rather than setting it every frame.
 *
 * Outside the bounding shape the window is not there at all, so the
 * real Emacs is what is on screen: always current, nothing to copy,
 * nothing to go stale.  That is what lets a frame paint a trail-sized
 * rectangle instead of the two million pixels of a whole window.
 *
 * But changing the shape is itself expensive: the server recomputes
 * clipping and exposes whatever the shape stopped covering, and Emacs
 * repaints it.  Measured over four styles, reshaping every frame cost
 * between two and three times the whole rest of the frame.  So the
 * shape only ever grows, with slack, and most frames do not touch it.
 */
#define SHAPE_SLACK 40

static void shape_to(SmearStage *st, int x, int y, int w, int h)
{
    if (!st->have_fixes) return;
    XRectangle r = { (short)x, (short)y, (unsigned short)w, (unsigned short)h };
    XserverRegion reg = XFixesCreateRegion(st->d, &r, (w > 0 && h > 0) ? 1 : 0);
    XFixesSetWindowShapeRegion(st->d, st->ov, ShapeBounding, 0, 0, reg);
    XFixesDestroyRegion(st->d, reg);
    st->shape_x = x; st->shape_y = y; st->shape_w = w; st->shape_h = h;
}

void smear_stage_frame_begin(SmearStage *st, int x, int y, int w, int h)
{
    if (!st || st->trouble[0] || !st->mapped) return;
    if (x < 0) { w += x; x = 0; }
    if (y < 0) { h += y; y = 0; }
    if (x + w > st->w) w = st->w - x;
    if (y + h > st->h) h = st->h - y;
    if (w < 1 || h < 1) return;

    if (st->shape_w < 1) {
        shape_to(st, x, y, w, h);
    } else if ((long)w * h * 2 < (long)st->shape_w * st->shape_h) {
        /* Grossly oversized: take it back to what is needed.
         *
         * Growing only is fine for a flight that lasts a fifth of a
         * second.  It is not fine for one that keeps being retargeted
         * (scrolling does that for as long as the wheel turns),
         * because the shape then unions the whole path the cursor has
         * wandered and ends up the size of the window, which is the
         * two million pixels a frame this exists to avoid. */
        shape_to(st, x, y, w, h);
    } else if (x < st->shape_x || y < st->shape_y ||
               x + w > st->shape_x + st->shape_w ||
               y + h > st->shape_y + st->shape_h) {
        /* union with slack, so a settling trail does not reshape on
         * every frame it drifts a pixel */
        int nx = x < st->shape_x ? x - SHAPE_SLACK : st->shape_x;
        int ny = y < st->shape_y ? y - SHAPE_SLACK : st->shape_y;
        int rx = x + w > st->shape_x + st->shape_w
                 ? x + w + SHAPE_SLACK : st->shape_x + st->shape_w;
        int ry = y + h > st->shape_y + st->shape_h
                 ? y + h + SHAPE_SLACK : st->shape_y + st->shape_h;
        if (nx < 0) nx = 0;
        if (ny < 0) ny = 0;
        if (rx > st->w) rx = st->w;
        if (ry > st->h) ry = st->h;
        shape_to(st, nx, ny, rx - nx, ry - ny);
    }
    /* Up now, and not before: the shape is set, so whatever is
     * watching sees the overlay arrive already the size of the band.
     *
     * A compositor works out the region it will composite for a window
     * when that window comes up, and drops damage falling outside it.
     * Coming up shapeless and growing afterwards is how a full-width
     * line pulse reached the screen as the 96 pixels a cursor trail
     * had shaped the flight before, with the rest of it drawn, shaped
     * and never seen.  Measured under picom.
     *
     * A map rather than a raise, because a raise is not enough.  On an
     * X server reached over ssh -X, `XRaiseWindow' on the already
     * mapped overlay left the stacking order untouched, and so did
     * `XConfigureWindow' with `Above'; the overlay stayed under the
     * frame holding a perfectly good band nobody could see.  The only
     * restack that server honours is the one that comes with a map.
     * So the overlay goes away between flights, which
     * `smear_stage_end' wants anyway, and comes back raised and shaped
     * once a flight. */
    if (st->pending_show) {
        smear_maps++;
        XMapRaised(st->d, st->ov);
        st->pending_show = 0;
    }
    /* Paint all of what is shown, not just what this frame asked for:
     * the rest of the shape would otherwise still be holding the last
     * frame's trail. */
    XCopyArea(st->d, st->wpix, st->dst == st->bufpic ? st->buf : st->ov,
              st->gc, st->shape_x, st->shape_y, st->shape_w, st->shape_h,
              st->shape_x, st->shape_y);
    st->nbatch = 0;
    smear_stage_region(st, x, y, w, h);
}

void smear_stage_region(SmearStage *st, int x, int y, int w, int h)
{
    if (!st) return;
    if (x < 0) { w += x; x = 0; }
    if (y < 0) { h += y; y = 0; }
    if (x + w > st->w) w = st->w - x;
    if (y + h > st->h) h = st->h - y;
    st->gl_x = x; st->gl_y = y;
    st->gl_w = w > 0 ? w : 0; st->gl_h = h > 0 ? h : 0;
}

/* Render what is held and composite it over the frame's box.
 *
 * Splitting a frame across two passes gives the same picture as one:
 * the shader composites its layers over each other, and the second
 * pass composites over what the first put down.  It costs an extra
 * upload, which is why the batch is as large as a frame can be and
 * this only ever runs at the end of one. */
static void gl_flush(SmearStage *st)
{
    if (st->renderer != SMEAR_RENDERER_GL || st->nbatch < 1) return;
    smear_uploads++;
    smear_gl_draw(st->d, st->root, st->dst, st->depth,
                  st->batch, st->nbatch,
                  st->gl_x, st->gl_y, st->gl_w, st->gl_h, 0.0);
    st->nbatch = 0;
}

void smear_stage_frame_end(SmearStage *st)
{
    /* Not while it is down: `smear_stage_frame_begin' lays the
     * backdrop and refuses when the overlay is unmapped, so drawing
     * anyway would put ink on a background nobody laid. */
    if (!st || st->trouble[0] || !st->mapped) return;
    /* Over the region asked for, not over the whole shape.
     *
     * The shape is what the overlay *shows*, and it is allowed to run
     * larger than any one frame needs, since reshaping costs a round
     * trip, so it grows with slack and shrinks only when it is
     * grossly oversized.  The backdrop is laid across all of it, so
     * the parts no layer reaches are already showing the window's own
     * live text and there is nothing to composite there.  Rendering
     * the shape instead made every frame after a full-width effect
     * pay for an area nothing was drawn in. */
    gl_flush(st);
    /* Out in one copy.  Mid-copy the overlay holds the top of this
     * frame over the bottom of the last, which for an effect that
     * moves a little between frames is not something an eye can catch
     * -- and never the bare text. */
    if (st->dst == st->bufpic)
        XCopyArea(st->d, st->buf, st->ov, st->gc,
                  st->shape_x, st->shape_y, st->shape_w, st->shape_h,
                  st->shape_x, st->shape_y);
    XFlush(st->d);
}

/* A 1-D box kernel, W by H.  One of the two is always 1: see below. */
static void set_kernel(Display *d, Picture pic, int w, int h)
{
    int total = w * h;
    XFixed *k = calloc(total + 2, sizeof *k);
    if (!k) return;
    k[0] = XDoubleToFixed(w);
    k[1] = XDoubleToFixed(h);
    double weight = 1.0 / total;
    for (int i = 0; i < total; i++) k[i + 2] = XDoubleToFixed(weight);
    XRenderSetPictureFilter(d, pic, "convolution", k, total + 2);
    free(k);
}

#define MAX_BLUR 8

/* How much smaller to rasterise a blurred layer.
 *
 * A blur does not need full resolution, which is what a blur is.  So
 * the quad is drawn into a mask a few times smaller and sampled back
 * up, which costs the square of the scale less: the convolution runs
 * over a ninth of the pixels at scale 3.  On this display the blur was
 * 20 ms of a 16.7 ms frame at full resolution.
 *
 * The upsampling does some of the softening itself, so the kernel that
 * remains is small. */
static int blur_scale(double radius)
{
    int s = (int)(radius / 1.5 + 0.5);
    if (s < 1) s = 1;
    if (s > 4) s = 4;
    return s;
}

/* Blur MASK, and return the picture to use in its place.
 *
 * Two passes of a 1-D box rather than one square kernel.  A box blur
 * is separable: box(n by n) is box(n by 1) then box(1 by n), and at
 * radius 5 that is 22 samples a pixel against 121.
 *
 * The second pass costs nothing extra: a filter applies when a picture
 * is *sampled*, so setting it on the intermediate and using that as
 * the mask blurs it on the way into the final composite. */
static Picture blur_mask(SmearStage *st, Picture mask, Pixmap *mp,
                         int mw, int mh, double radius, int scale)
{
    Display *d = st->d;
    int r = (int)(radius / scale + 0.5);
    if (r < 1) r = 1;
    if (r > MAX_BLUR) r = MAX_BLUR;
    int n = 2 * r + 1;

    Pixmap tmp = XCreatePixmap(d, st->root, mw, mh, 8);
    Picture tp = XRenderCreatePicture(d, tmp, st->a8, 0, NULL);
    XRenderColor clear = { 0, 0, 0, 0 };
    XRenderFillRectangle(d, PictOpSrc, tp, &clear, 0, 0, mw, mh);

    set_kernel(d, mask, n, 1);                    /* across */
    XRenderComposite(d, PictOpSrc, mask, None, tp, 0, 0, 0, 0, 0, 0, mw, mh);
    set_kernel(d, tp, 1, n);                      /* and down, on sampling */

    XRenderFreePicture(d, mask);
    XFreePixmap(d, *mp);
    *mp = tmp;
    return tp;
}

/* Sample a small mask across a big destination. */
static void mask_scaled_up(Display *d, Picture mask, int scale)
{
    if (scale <= 1) return;
    XTransform t = {{
        { XDoubleToFixed(scale), 0, 0 },
        { 0, XDoubleToFixed(scale), 0 },
        { 0, 0, XDoubleToFixed(1.0) }
    }};
    XRenderSetPictureTransform(d, mask, &t);
}

/* Offset the quad's outline by GROW, outward for positive.
 *
 * Along the edge normals, not away from the centre.  Pushing corners
 * radially from the centroid is the obvious thing and it is wrong for
 * the shape this draws: a trail is long and thin, so a radial push
 * mostly makes it *longer* rather than wider, and a negative one
 * barely narrows it at all.
 *
 * That disagreed with the GL renderer, where grow is an offset on a
 * signed distance and so perpendicular by construction.  Measured on
 * one style, `:grow -7' left a trail 4502 pixels wide here and erased
 * it completely there, correctly, since the quad's perpendicular
 * half-width was about seven.  A style has to mean the
 * same thing whichever renderer draws it, and this is the one that
 * was wrong. */
static void grow_quad(double *qx, double *qy, double grow)
{
    if (grow == 0.0) return;
    double ox[4], oy[4];
    for (int i = 0; i < 4; i++) {
        int prev = (i + 3) & 3, next = (i + 1) & 3;
        /* outward normals of the two edges meeting at this corner */
        double e1x = qx[i] - qx[prev], e1y = qy[i] - qy[prev];
        double e2x = qx[next] - qx[i], e2y = qy[next] - qy[i];
        double l1 = hypot(e1x, e1y), l2 = hypot(e2x, e2y);
        if (l1 < 1e-6 || l2 < 1e-6) { ox[i] = qx[i]; oy[i] = qy[i]; continue; }
        double n1x = e1y / l1, n1y = -e1x / l1;
        double n2x = e2y / l2, n2y = -e2x / l2;
        double bx_ = n1x + n2x, by_ = n1y + n2y;
        double bl = hypot(bx_, by_);
        if (bl < 1e-6) { ox[i] = qx[i]; oy[i] = qy[i]; continue; }
        bx_ /= bl; by_ /= bl;
        /* a corner has to move further than an edge to keep the offset
         * perpendicular; bounded, or a near-degenerate corner flies off */
        double cosang = bx_ * n1x + by_ * n1y;
        double scale = 1.0 / (cosang > 0.25 ? cosang : 0.25);
        ox[i] = qx[i] + bx_ * grow * scale;
        oy[i] = qy[i] + by_ * grow * scale;
    }
    for (int i = 0; i < 4; i++) { qx[i] = ox[i]; qy[i] = oy[i]; }
}

static Picture gradient_for(Display *d, const SmearPaint *p,
                            double hx, double hy, double fx_, double fy_)
{
    int n = p->nstops < 2 ? 2 : (p->nstops > SMEAR_MAX_STOPS
                                 ? SMEAR_MAX_STOPS : p->nstops);
    XFixed stops[SMEAR_MAX_STOPS];
    XRenderColor cols[SMEAR_MAX_STOPS];
    for (int i = 0; i < n; i++) {
        double at = p->nstops >= 2 ? p->stop_at[i] : (i ? 1.0 : 0.0);
        double a  = p->nstops >= 2 ? p->stop_alpha[i] : (i ? 0.0 : 1.0);
        stops[i] = XDoubleToFixed(at);
        cols[i].red   = (unsigned short)(p->r * 65535 * a);
        cols[i].green = (unsigned short)(p->g * 65535 * a);
        cols[i].blue  = (unsigned short)(p->b * 65535 * a);
        cols[i].alpha = (unsigned short)(a * 65535);
    }
    if (p->kind == SMEAR_KIND_RADIAL) {
        XRadialGradient g = {
            { XDoubleToFixed(hx), XDoubleToFixed(hy), XDoubleToFixed(0) },
            { XDoubleToFixed(hx), XDoubleToFixed(hy),
              XDoubleToFixed(p->radius > 1 ? p->radius : 1) }
        };
        return XRenderCreateRadialGradient(d, &g, stops, cols, n);
    }
    if (hypot(fx_ - hx, fy_ - hy) < 2.0) {
        XRenderColor c = cols[0];
        return XRenderCreateSolidFill(d, &c);
    }
    XLinearGradient g = { { XDoubleToFixed(hx), XDoubleToFixed(hy) },
                          { XDoubleToFixed(fx_), XDoubleToFixed(fy_) } };
    return XRenderCreateLinearGradient(d, &g, stops, cols, n);
}

void smear_stage_draw(SmearStage *st, const SmearPaint *p)
{
    if (!st || st->trouble[0] || !st->mapped) return;
    Display *d = st->d;

    /* The shader draws every layer in one pass, so they are collected
     * here and rendered when the frame closes.  Compositing them one
     * at a time would be one upload per layer. */
    if (st->renderer == SMEAR_RENDERER_GL) {
        /* A full batch is rendered now rather than dropped.  Dropping
         * is silent -- the frame still draws, just without its last
         * layers -- and an effect loses whatever it built last, which
         * is the part meant to be on top. */
        if (st->nbatch == SMEAR_MAX_LAYERS) gl_flush(st);
        st->batch[st->nbatch++] = *p;
        return;
    }

    double qx[4], qy[4];
    for (int i = 0; i < 4; i++) {
        qx[i] = p->corners[2 * i];
        qy[i] = p->corners[2 * i + 1];
    }
    grow_quad(qx, qy, p->grow);

    /* the box this layer needs, blur and glow included */
    double minx = qx[0], maxx = qx[0], miny = qy[0], maxy = qy[0];
    for (int i = 1; i < 4; i++) {
        if (qx[i] < minx) minx = qx[i];
        if (qx[i] > maxx) maxx = qx[i];
        if (qy[i] < miny) miny = qy[i];
        if (qy[i] > maxy) maxy = qy[i];
    }
    if (p->kind == SMEAR_KIND_RADIAL) {
        minx = p->head[0] - p->radius; maxx = p->head[0] + p->radius;
        miny = p->head[1] - p->radius; maxy = p->head[1] + p->radius;
    }
    double pad = 2 + (p->blur > 0 ? p->blur + 1 : 0);
    int bx = (int)floor(minx - pad), by = (int)floor(miny - pad);
    int bx1 = (int)ceil(maxx + pad), by1 = (int)ceil(maxy + pad);
    if (bx < 0) bx = 0;
    if (by < 0) by = 0;
    if (bx1 > st->w) bx1 = st->w;
    if (by1 > st->h) by1 = st->h;
    if (bx1 <= bx || by1 <= by) return;
    int bw = bx1 - bx, bh = by1 - by;

    double hx = p->head[0] - bx, hy = p->head[1] - by;
    Picture mask = 0;
    Pixmap mp = 0;

    int blurring = (p->blur > 0 && smear_stage_can_blur(st));
    int scale = blurring ? blur_scale(p->blur) : 1;
    if (p->kind == SMEAR_KIND_QUAD) {
        /* The quad goes into an A8 coverage mask first.  Compositing
         * two triangles straight onto the destination blends their
         * shared edge twice and leaves a seam down the middle.
         *
         * A blurred layer is rasterised into a smaller mask and
         * sampled back up; nothing is lost that the blur would not
         * have thrown away anyway. */
        int mw = (bw + scale - 1) / scale, mh = (bh + scale - 1) / scale;
        mp = XCreatePixmap(d, st->root, mw, mh, 8);
        mask = XRenderCreatePicture(d, mp, st->a8, 0, NULL);
        XRenderColor clear = { 0, 0, 0, 0 };
        XRenderFillRectangle(d, PictOpSrc, mask, &clear, 0, 0, mw, mh);
        XRenderColor white = { 0xffff, 0xffff, 0xffff, 0xffff };
        Picture solid = XRenderCreateSolidFill(d, &white);
        double lx[4], ly[4];
        for (int i = 0; i < 4; i++) {
            lx[i] = (qx[i] - bx) / scale;
            ly[i] = (qy[i] - by) / scale;
        }
        XTriangle tri[2];
        /* split on the shorter diagonal: for a quad bent the other way
         * the long-diagonal split falls outside the shape */
        if (hypot(lx[0] - lx[2], ly[0] - ly[2]) <=
            hypot(lx[1] - lx[3], ly[1] - ly[3])) {
            tri[0] = (XTriangle){ {fx(lx[0]), fx(ly[0])}, {fx(lx[1]), fx(ly[1])},
                                  {fx(lx[2]), fx(ly[2])} };
            tri[1] = (XTriangle){ {fx(lx[0]), fx(ly[0])}, {fx(lx[2]), fx(ly[2])},
                                  {fx(lx[3]), fx(ly[3])} };
        } else {
            tri[0] = (XTriangle){ {fx(lx[1]), fx(ly[1])}, {fx(lx[2]), fx(ly[2])},
                                  {fx(lx[3]), fx(ly[3])} };
            tri[1] = (XTriangle){ {fx(lx[1]), fx(ly[1])}, {fx(lx[3]), fx(ly[3])},
                                  {fx(lx[0]), fx(ly[0])} };
        }
        XRenderCompositeTriangles(d, PictOpOver, solid, mask, st->a8,
                                  0, 0, tri, 2);
        XRenderFreePicture(d, solid);
        if (blurring)
            mask = blur_mask(st, mask, &mp, mw, mh, p->blur, scale);
        mask_scaled_up(d, mask, scale);
    }

    /* the far end, for the gradient's other stop */
    double far = 0, fxx = hx, fyy = hy;
    for (int i = 0; i < 4; i++) {
        double dd = hypot((qx[i] - bx) - hx, (qy[i] - by) - hy);
        if (dd > far) { far = dd; fxx = qx[i] - bx; fyy = qy[i] - by; }
    }
    Picture src = gradient_for(d, p, hx, hy, fxx, fyy);
    XRenderComposite(d, PictOpOver, src, mask, st->dst, 0, 0, 0, 0,
                     bx, by, bw, bh);
    XRenderFreePicture(d, src);
    if (mask) XRenderFreePicture(d, mask);
    if (mp) XFreePixmap(d, mp);

    st->last_x = bx; st->last_y = by; st->last_w = bw; st->last_h = bh;
}

static int region_has_detail(SmearStage *st, int x, int y, int w, int h)
{
    if (x < 0 || y < 0 || x + w > st->w || y + h > st->h) return -1;
    XImage *im = XGetImage(st->d, st->ov, x, y, w, h, AllPlanes, ZPixmap);
    if (!im) return -1;
    double sum = 0, sum2 = 0; long n = 0;
    for (int j = 0; j < h; j++)
        for (int i = 0; i < w; i++) {
            double g = (XGetPixel(im, i, j) >> 8) & 0xff;
            sum += g; sum2 += g * g; n++;
        }
    double mean = sum / n, var = sum2 / n - mean * mean;
    XDestroyImage(im);
    return var > 4.0 ? 1 : 0;
}

int smear_stage_background_has_detail(SmearStage *st)
{
    if (!st || !st->mapped || st->w < 64 || st->h < 64) return -1;
    int w = st->w / 4 < 240 ? st->w / 4 : 240;
    int h = st->h / 8 < 80 ? st->h / 8 : 80;
    if (w < 16 || h < 16) return -1;
    int spots[3][2] = {
        { st->w / 8,     st->h / 4 },
        { st->w / 2,     st->h / 2 },
        { st->w / 8, 3 * st->h / 5 },
    };
    int any = -1;
    for (int i = 0; i < 3; i++) {
        int r = region_has_detail(st, spots[i][0], spots[i][1], w, h);
        if (r == 1) return 1;
        if (r == 0) any = 0;
    }
    return any;
}

void smear_stage_sample(SmearStage *st, int x, int y, int w, int h,
                        double *mean, double *sd, long *changed)
{
    *mean = 0; *sd = 0; *changed = 0;
    if (!st || !st->mapped) return;
    if (x < 0 || y < 0 || w < 1 || h < 1 ||
        x + w > st->w || y + h > st->h) return;
    XImage *drawn = XGetImage(st->d, st->ov, x, y, w, h, AllPlanes, ZPixmap);
    XImage *plain = XGetImage(st->d, st->wpix, x, y, w, h, AllPlanes, ZPixmap);
    if (!drawn) { if (plain) XDestroyImage(plain); return; }
    double sum = 0, sum2 = 0; long n = 0, diff = 0;
    for (int j = 0; j < h; j++)
        for (int i = 0; i < w; i++) {
            unsigned long a = XGetPixel(drawn, i, j);
            double g = (a >> 8) & 0xff;
            sum += g; sum2 += g * g; n++;
            if (plain && a != XGetPixel(plain, i, j)) diff++;
        }
    *mean = sum / n;
    double var = sum2 / n - (*mean) * (*mean);
    *sd = var > 0 ? sqrt(var) : 0;
    *changed = diff;
    XDestroyImage(drawn);
    if (plain) XDestroyImage(plain);
}

void smear_stage_stamp_drop(SmearStage *st, int slot)
{
    if (!st || slot < 0 || slot >= SMEAR_STAGE_STAMPS) return;
    if (st->stamp[slot].pic) XRenderFreePicture(st->d, st->stamp[slot].pic);
    if (st->stamp[slot].pm) XFreePixmap(st->d, st->stamp[slot].pm);
    memset(&st->stamp[slot], 0, sizeof st->stamp[slot]);
}

int smear_stage_stamp_take(SmearStage *st, int slot, int x, int y, int w, int h)
{
    if (!st || st->trouble[0] || slot < 0 || slot >= SMEAR_STAGE_STAMPS)
        return 0;
    if (st->renderer != SMEAR_RENDERER_GL || st->nbatch < 1) return 0;
    if (w < 1 || h < 1) return 0;

    smear_stage_stamp_drop(st, slot);
    smear_uploads++;
    Pixmap pm = smear_gl_render(st->d, st->root, st->depth,
                                st->batch, st->nbatch, x, y, w, h);
    st->nbatch = 0;
    if (!pm) return 0;

    st->stamp[slot].pm = pm;
    st->stamp[slot].pic = XRenderCreatePicture(
        st->d, pm, XRenderFindStandardFormat(st->d, PictStandardARGB32),
        0, NULL);
    st->stamp[slot].x = x; st->stamp[slot].y = y;
    st->stamp[slot].w = w; st->stamp[slot].h = h;
    return 1;
}

int smear_stage_stamp_replay(SmearStage *st, int slot, double alpha)
{
    if (!st || st->trouble[0] || !st->mapped) return 0;
    if (slot < 0 || slot >= SMEAR_STAGE_STAMPS) return 0;
    if (!st->stamp[slot].pic || !st->dst) return 0;
    if (alpha < 0.0) alpha = 0.0;
    if (alpha > 1.0) alpha = 1.0;

    /* A one-pixel mask, repeated: XRender scales the source by it, and
     * for a premultiplied ARGB source that scales colour and coverage
     * together, which is what fading a rendered shape means. */
    if (!st->amask) {
        Pixmap mp = XCreatePixmap(st->d, st->root, 1, 1, 8);
        XRenderPictureAttributes pa;
        pa.repeat = True;
        st->amask = XRenderCreatePicture(st->d, mp, st->a8, CPRepeat, &pa);
        XFreePixmap(st->d, mp);      /* the picture holds it open */
        if (!st->amask) return 0;
    }
    XRenderColor c = { 0, 0, 0, (unsigned short)(alpha * 65535.0) };
    XRenderFillRectangle(st->d, PictOpSrc, st->amask, &c, 0, 0, 1, 1);
    XRenderComposite(st->d, PictOpOver, st->stamp[slot].pic, st->amask,
                     st->dst, 0, 0, 0, 0,
                     st->stamp[slot].x, st->stamp[slot].y,
                     st->stamp[slot].w, st->stamp[slot].h);
    return 1;
}

int smear_stage_freeze(SmearStage *st, int x, int y, int w, int h)
{
    if (!st || st->trouble[0]) return 0;
    if (x < 0) { w += x; x = 0; }
    if (y < 0) { h += y; y = 0; }
    if (x + w > (int)st->tw) w = (int)st->tw - x;
    if (y + h > (int)st->th) h = (int)st->th - y;
    if (w < 1 || h < 1) return 0;
    if (!st->wpix) return 0;

    /* Grown, never shrunk: a deletion is a common thing and a pixmap
     * is a round trip to allocate. */
    if (!st->frozen || w > st->frozen_w || h > st->frozen_h) {
        if (st->frozen) XFreePixmap(st->d, st->frozen);
        st->frozen_w = w > st->frozen_w ? w : st->frozen_w;
        st->frozen_h = h > st->frozen_h ? h : st->frozen_h;
        st->frozen = XCreatePixmap(st->d, st->root,
                                   (unsigned)st->frozen_w,
                                   (unsigned)st->frozen_h, st->depth);
        if (!st->frozen) { st->frozen_w = st->frozen_h = 0; return 0; }
    }
    XCopyArea(st->d, st->wpix, st->frozen, st->gc, x, y,
              (unsigned)w, (unsigned)h, 0, 0);
    /* Flushed here, and this is the whole reason the call exists.  The
     * caller is inside `before-change-functions': the text is still on
     * screen and is about to stop being, and Emacs draws over it on
     * its own connection at the next redisplay.  A request still
     * sitting in our output buffer would photograph the wrong thing. */
    XFlush(st->d);
    st->fx0 = x; st->fy0 = y; st->fw = w; st->fh = h;
    return 1;
}

unsigned long smear_stage_overlay(SmearStage *st) { return st ? st->ov : 0; }

void smear_stage_restore_frozen(SmearStage *st)
{
    if (!st || !st->frozen || st->fw < 1 || st->fh < 1 || !st->mapped) return;
    /* Into the frame being composed, like everything else a frame
     * draws.  Put straight into the overlay it would be painted over
     * when the finished frame is copied out. */
    XCopyArea(st->d, st->frozen, st->buf ? st->buf : st->ov, st->gc, 0, 0,
              (unsigned)st->fw, (unsigned)st->fh, st->fx0, st->fy0);
}

double smear_stage_drain(SmearStage *st)
{
    if (!st || st->trouble[0]) return 0.0;
    /* One round trip, and what it costs is the answer.
     *
     * `XFlush' hands the frame to the socket and returns; it does not
     * wait for the server to have it, let alone to have drawn it.  On
     * a display reached over a network, such as the ssh -X where this
     * is drawn as often as not, a whole flight can be queued inside
     * its budget and arrive after it is over, and every number in
     * `smear-cursor-report' would still read as healthy.  A single
     * `XSync' at the end of a flight is the cheapest thing that can
     * tell the difference: it returns when the server has caught up,
     * so how long it took is how far behind the server was. */
    struct timespec a, b;
    clock_gettime(CLOCK_MONOTONIC, &a);
    XSync(st->d, False);
    clock_gettime(CLOCK_MONOTONIC, &b);
    return (double)(b.tv_sec - a.tv_sec) + (double)(b.tv_nsec - a.tv_nsec) / 1e9;
}

void smear_stage_end(SmearStage *st)
{
    if (!st) return;
    st->pending_show = 0;       /* a flight that never drew a frame */
    if (!st->mapped) return;
    /* Down between flights, and down means gone.
     *
     * Not for tidiness: an override-redirect window left sitting on
     * top of the frame costs it the keyboard.  Watched continuously,
     * focus was `None' for as long as this window was above the Emacs
     * window and came back the instant the frame was above it again.
     * An editor that stops taking keys is not a cheaper editor.
     *
     * Lowering it instead looks like the gentler answer and is not: a
     * mapped window that is merely lowered has to be raised again, and
     * `smear_stage_frame_begin' has the measurement showing a raise is
     * a request some servers simply do not act on.  Unmapping is what
     * both ends of this want: the frame gets the keyboard back, and
     * the next flight gets a map to ride on top with.
     *
     * The shape is left as it stands.  It is cleared in
     * `smear_stage_begin', while the overlay is down, so the next
     * flight shapes it afresh before anything sees it come up. */
    XUnmapWindow(st->d, st->ov);
    st->mapped = 0;
    XFlush(st->d);
}

void smear_stage_close(SmearStage *st)
{
    if (!st) return;
    smear_stage_end(st);
    for (int i = 0; i < SMEAR_STAGE_STAMPS; i++) smear_stage_stamp_drop(st, i);
    if (st->amask) XRenderFreePicture(st->d, st->amask);
    if (st->dst) XRenderFreePicture(st->d, st->dst);
    if (st->frozen) XFreePixmap(st->d, st->frozen);
    if (st->wpix) XFreePixmap(st->d, st->wpix);
    if (st->bufpic) XRenderFreePicture(st->d, st->bufpic);
    if (st->buf) XFreePixmap(st->d, st->buf);
    if (st->ov) XDestroyWindow(st->d, st->ov);
    if (st->redirected) p_unredirect(st->d, st->target, REDIRECT_AUTOMATIC);
    XSync(st->d, False);
    unguard_display(st->d);
    if (st->own_display) XCloseDisplay(st->d);
    free(st);
}
