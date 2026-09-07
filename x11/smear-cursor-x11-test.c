/* Does the GL renderer put ink where the caller asked for it?
 *
 * The shader works in the window's own coordinates, but GL and X
 * disagree about which way is up in two places, gl_FragCoord and
 * glReadPixels, and those two cancel.  A third flip added "to correct
 * for GL" mirrors the trail inside its own box: nearly invisible on a
 * horizontal move, plainly wrong on a diagonal one.
 *
 * So every case below is asymmetric in both axes.  A shape that reads
 * the same upside down proves nothing.
 *
 * Needs a display and a working EGL; exits 77 (skip) without them.
 */
#include "smear-cursor-x11-gl.h"
#include "smear-cursor-x11-play.h"
#include <X11/Xlib.h>
#include <X11/extensions/shape.h>
#include <X11/extensions/Xrender.h>
#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <time.h>

#define W 128
#define H 128

static Display *D;
static Pixmap PM;
static Picture DST;
static int failures;

/* What is on the pixmap: where the ink sits, and how much. */
typedef struct { double cx, cy, count; } Ink;

static void clear_target(void)
{
    XRenderColor transparent = {0, 0, 0, 0};
    XRenderFillRectangle(D, PictOpSrc, DST, &transparent, 0, 0, W, H);
}

static double alpha_at(XImage *im, int x, int y)
{
    if (x < 0 || y < 0 || x >= W || y >= H) return 0.0;
    return (double)((XGetPixel(im, x, y) >> 24) & 0xff) / 255.0;
}

/* Alpha-weighted centroid of everything drawn, in pixmap pixels. */
static Ink measure(XImage *im)
{
    Ink k = {0, 0, 0};
    for (int y = 0; y < H; y++)
        for (int x = 0; x < W; x++) {
            double a = alpha_at(im, x, y);
            if (a > 0.02) { k.cx += x * a; k.cy += y * a; k.count += a; }
        }
    if (k.count > 0) { k.cx /= k.count; k.cy /= k.count; }
    return k;
}

/* Mean alpha over a rectangle, for comparing one end against another. */
static double mean_alpha(XImage *im, int x, int y, int w, int h)
{
    double s = 0;
    for (int j = y; j < y + h; j++)
        for (int i = x; i < x + w; i++) s += alpha_at(im, i, j);
    return s / (w * h);
}

/* Draw LAYERS over the box at BX BY and read the result back.
 *
 * The box origin is deliberately not zero: the shader offsets by it,
 * and an origin of zero would hide getting that wrong.  The pixmap
 * stands in for the window, and the box is composited onto it at
 * BX BY, so a pixel's position in the returned image is already the
 * window coordinate the caller asked in, with no arithmetic. */
static XImage *render(const SmearPaint *layers, int n, int bx, int by)
{
    clear_target();
    if (!smear_gl_draw(D, DefaultRootWindow(D), DST, 32, layers, n,
                       bx, by, W - bx, H - by, 0.0))
        return NULL;
    XSync(D, False);
    return XGetImage(D, PM, 0, 0, W, H, AllPlanes, ZPixmap);
}

static double mono_test(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + ts.tv_nsec / 1e9;
}

static void ok(int cond, const char *name, const char *detail)
{
    printf("%s %s%s%s\n", cond ? "ok  " : "FAIL", name,
           cond ? "" : " -- ", cond ? "" : detail);
    if (!cond) failures++;
}

static void quad(SmearPaint *p, double x0, double y0, double x1, double y1)
{
    memset(p, 0, sizeof *p);
    p->kind = SMEAR_KIND_QUAD;
    double c[8] = {x0, y0, x1, y0, x1, y1, x0, y1};
    memcpy(p->corners, c, sizeof c);
    p->head[0] = x0; p->head[1] = (y0 + y1) / 2;
    p->r = p->g = p->b = 1.0;
    p->nstops = 2;
    p->stop_at[0] = 0.0; p->stop_alpha[0] = 1.0;
    p->stop_at[1] = 1.0; p->stop_alpha[1] = 1.0;
}

/* A bar in the top-left of its box lands in the top-left. */
static void test_lands_where_asked(void)
{
    const int bx = 17, by = 23;   /* not zero: exercises iOrigin */
    SmearPaint p;
    quad(&p, 30, 30, 90, 42);            /* centre (60, 36) in the window */
    XImage *im = render(&p, 1, bx, by);
    if (!im) { ok(0, "lands-where-asked", "draw failed"); return; }
    Ink k = measure(im);
    char why[128];
    snprintf(why, sizeof why, "asked (60,36), got (%.1f,%.1f)", k.cx, k.cy);
    ok(fabs(k.cx - 60) < 2 && fabs(k.cy - 36) < 2, "lands-where-asked", why);
    XDestroyImage(im);
}

/* A quad running top-left to bottom-right stays that way.  A mirror in
 * either axis turns it into bottom-left to top-right, which puts ink
 * where this asserts there is none. */
static void test_diagonal_keeps_its_slope(void)
{
    SmearPaint p;
    memset(&p, 0, sizeof p);
    p.kind = SMEAR_KIND_QUAD;
    double c[8] = {20, 20, 34, 20, 100, 96, 86, 96};
    memcpy(p.corners, c, sizeof c);
    p.head[0] = 27; p.head[1] = 20;
    p.r = p.g = p.b = 1.0;
    p.nstops = 2;
    p.stop_at[0] = 0.0; p.stop_alpha[0] = 1.0;
    p.stop_at[1] = 1.0; p.stop_alpha[1] = 1.0;

    XImage *im = render(&p, 1, 0, 0);
    if (!im) { ok(0, "diagonal-keeps-its-slope", "draw failed"); return; }
    double tl = mean_alpha(im, 20, 20, 14, 10);
    double br = mean_alpha(im, 86, 86, 14, 10);
    double tr = mean_alpha(im, 86, 20, 14, 10);
    double bl = mean_alpha(im, 20, 86, 14, 10);
    char why[160];
    snprintf(why, sizeof why, "tl %.2f br %.2f (want ink) tr %.2f bl %.2f (want none)",
             tl, br, tr, bl);
    ok(tl > 0.5 && br > 0.5 && tr < 0.05 && bl < 0.05,
       "diagonal-keeps-its-slope", why);
    XDestroyImage(im);
}

/* The gradient starts at the head, so the head end is the bright one. */
static void test_head_end_is_bright(void)
{
    SmearPaint p;
    quad(&p, 20, 50, 108, 70);
    p.head[0] = 20; p.head[1] = 60;      /* the left end */
    p.stop_alpha[1] = 0.05;
    XImage *im = render(&p, 1, 0, 0);
    if (!im) { ok(0, "head-end-is-bright", "draw failed"); return; }
    double at_head = mean_alpha(im, 22, 54, 12, 12);
    double at_tail = mean_alpha(im, 94, 54, 12, 12);
    char why[128];
    snprintf(why, sizeof why, "head %.2f tail %.2f", at_head, at_tail);
    ok(at_head > at_tail * 2, "head-end-is-bright", why);
    XDestroyImage(im);
}

/* A glow is centred on the head, not on the box. */
static void test_glow_centres_on_head(void)
{
    const int bx = 11, by = 29;   /* not zero: exercises iOrigin */
    SmearPaint p;
    memset(&p, 0, sizeof p);
    p.kind = SMEAR_KIND_RADIAL;
    p.head[0] = 40; p.head[1] = 96;      /* low and left, unlike the box */
    p.radius = 18;
    p.r = p.g = p.b = 1.0;
    p.nstops = 2;
    p.stop_at[0] = 0.0; p.stop_alpha[0] = 1.0;
    p.stop_at[1] = 1.0; p.stop_alpha[1] = 0.0;
    XImage *im = render(&p, 1, bx, by);
    if (!im) { ok(0, "glow-centres-on-head", "draw failed"); return; }
    Ink k = measure(im);
    char why[128];
    snprintf(why, sizeof why, "head (40,96), ink at (%.1f,%.1f)", k.cx, k.cy);
    ok(fabs(k.cx - 40) < 2 && fabs(k.cy - 96) < 2, "glow-centres-on-head", why);
    XDestroyImage(im);
}

/* ---- the player thread ------------------------------------------
 *
 * The point of the thread is that a flight finishes while the caller
 * is blocked, which is what Emacs does to itself constantly.  So the
 * test blocks the calling thread for longer than the flight lasts and
 * then asks how many frames were painted.  Driven from the caller, as
 * it used to be, the answer would be none.
 */

static void nap(double seconds)
{
    struct timespec ts;
    ts.tv_sec = (time_t)seconds;
    ts.tv_nsec = (long)((seconds - (double)ts.tv_sec) * 1e9);
    nanosleep(&ts, NULL);
}

/* A window to composite over.  Small and short-lived: it is a real
 * window on a real screen for as long as the test runs. */
static Window make_window(void)
{
    XSetWindowAttributes a;
    a.override_redirect = True;
    a.background_pixel = BlackPixel(D, DefaultScreen(D));
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, 240, 160, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             CWOverrideRedirect | CWBackPixel, &a);
    XMapWindow(D, w);
    XSync(D, False);
    return w;
}

/* A flight sliding a bar left to right across NF frames. */
static void build_flight(SmearPaint *layer, SmearFrame *f, int nf)
{
    quad(layer, 0, 0, 0, 0);            /* colour and stops; corners per frame */
    for (int k = 0; k < nf; k++) {
        double x = 10 + k * 8.0;
        f[k].bx = (int)x - 2; f[k].by = 58; f[k].bw = 44; f[k].bh = 24;
        double c[8] = {x, 60, x + 40, 60, x + 40, 80, x, 80};
        memcpy(f[k].corners[0], c, sizeof c);
        f[k].head[0][0] = x + 40; f[k].head[0][1] = 70;
        f[k].alpha[0] = 1.0;
    }
}

static void test_flight_finishes_while_the_caller_is_blocked(void)
{
    Window w = make_window();
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing: %s\n",
               st ? smear_stage_trouble(st) : "cannot open stage");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w);
        return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        ok(0, "flight-finishes-while-blocked", err);
        smear_stage_close(st); XDestroyWindow(D, w);
        return;
    }

    SmearPaint layer;
    SmearFrame frames[12];
    build_flight(&layer, frames, 12);
    smear_play_flight(st, SMEAR_TRACK_TRAIL, &layer, 1, frames, 12, 60.0, 0, 0);

    /* 12 frames at 60 fps is 200 ms.  Block for longer than that:
     * this is the 235 ms stall from a real session, reproduced. */
    nap(0.40);

    SmearPlayStats stats;
    int had = smear_play_stats(st, &stats);
    char why[160];
    snprintf(why, sizeof why, "painted %d of 12 frames while the caller slept",
             had ? stats.frames : 0);
    ok(had && stats.frames >= 10, "flight-finishes-while-blocked", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

static void test_retarget_replaces_a_flight_in_progress(void)
{
    Window w = make_window();
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w);
        return;                          /* already reported as a skip */
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        smear_stage_close(st); XDestroyWindow(D, w);
        return;
    }
    SmearPaint layer;
    SmearFrame frames[24];
    build_flight(&layer, frames, 24);
    smear_play_flight(st, SMEAR_TRACK_TRAIL, &layer, 1, frames, 24, 60.0, 0, 0);

    /* Wait for it to be under way rather than assuming a nap is long
     * enough.  Putting the overlay up copies the whole window and the
     * flight's clock starts after that, so how far 100 ms gets is not
     * a fixed thing.  It was three frames on one run and none on the
     * next, which is a flaky test rather than a finding. */
    int mid = -1;
    for (int i = 0; i < 200 && mid < 2; i++) {
        nap(0.005);
        mid = smear_play_position(st, SMEAR_TRACK_TRAIL);
    }
    /* a second movement arrives: the flight restarts from frame 0 */
    smear_play_flight(st, SMEAR_TRACK_TRAIL, &layer, 1, frames, 24, 60.0, 0, 0);
    int after = smear_play_position(st, SMEAR_TRACK_TRAIL);
    char why[160];
    snprintf(why, sizeof why, "was at frame %d, restarted at %d", mid, after);
    ok(mid >= 2 && after < mid, "retarget-restarts-the-flight", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* Effects and the trail share the screen: typing moves the cursor, so
 * a keystroke's flash and the cursor's trail are live at the same
 * moment.  A short effect ending must not take the overlay down under
 * a trail that is still flying. */
static void test_a_short_track_does_not_end_a_long_one(void)
{
    Window w = make_window();
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w);
        return;                          /* already reported as a skip */
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        smear_stage_close(st); XDestroyWindow(D, w);
        return;
    }
    SmearPaint layer;
    SmearFrame slow[36], quick[36];
    build_flight(&layer, slow, 36);
    build_flight(&layer, quick, 36);
    smear_play_flight(st, SMEAR_TRACK_TRAIL, &layer, 1, slow, 30, 60.0, 0, 0);
    smear_play_flight(st, 1, &layer, 1, quick, 5, 60.0, 0, 0);

    nap(0.02);
    int both_trail = smear_play_position(st, SMEAR_TRACK_TRAIL);
    int both_effect = smear_play_position(st, 1);
    nap(0.20);                            /* the short one is long over */
    int after_trail = smear_play_position(st, SMEAR_TRACK_TRAIL);
    int after_effect = smear_play_position(st, 1);

    char why[200];
    snprintf(why, sizeof why,
             "together trail %d effect %d; later trail %d effect %d",
             both_trail, both_effect, after_trail, after_effect);
    ok(both_trail >= 0 && both_effect >= 0
       && after_trail >= 0 && after_effect < 0,
       "short-track-does-not-end-a-long-one", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* Stopping the trail must not stop an effect.  The trail restarts on
 * every cursor movement, and a movement is exactly when an effect
 * tends to have just started, so a stop that took everything down
 * would kill the pulse the same movement asked for, depending on which
 * of two timers happened to win. */
static void test_stopping_one_track_leaves_the_others(void)
{
    Window w = make_window();
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w);
        return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        smear_stage_close(st); XDestroyWindow(D, w);
        return;
    }
    SmearPaint layer;
    SmearFrame frames[36];
    build_flight(&layer, frames, 36);
    smear_play_flight(st, SMEAR_TRACK_TRAIL, &layer, 1, frames, 30, 60.0, 0, 0);
    smear_play_flight(st, 1, &layer, 1, frames, 30, 60.0, 0, 0);
    nap(0.03);

    smear_play_stop_track(st, SMEAR_TRACK_TRAIL);
    int trail = smear_play_position(st, SMEAR_TRACK_TRAIL);
    int effect = smear_play_position(st, 1);
    char why[160];
    snprintf(why, sizeof why, "after stopping the trail: trail %d effect %d",
             trail, effect);
    ok(trail < 0 && effect >= 0, "stopping-one-track-leaves-the-others", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* One pixel of the overlay, through a pixmap of our own: reading a
 * window directly is only legal while it is viewable and unobscured,
 * and this one is a stage's overlay on somebody's desktop.  Zero when
 * it cannot be seen at all. */
static unsigned long overlay_pixel(SmearStage *st, int x, int y)
{
    XWindowAttributes a;
    Window ov = smear_stage_overlay(st);
    if (!XGetWindowAttributes(D, ov, &a) || a.map_state != IsViewable) return 0;
    Pixmap shot = XCreatePixmap(D, ov, 1, 1, a.depth);
    GC sgc = XCreateGC(D, shot, 0, NULL);
    XCopyArea(D, ov, shot, sgc, x, y, 1, 1, 0, 0);
    XSync(D, False);
    unsigned long v = 0;
    XImage *im = XGetImage(D, shot, 0, 0, 1, 1, AllPlanes, ZPixmap);
    if (im) { v = XGetPixel(im, 0, 0) & 0x00ffffff; XDestroyImage(im); }
    XFreeGC(D, sgc);
    XFreePixmap(D, shot);
    return v;
}

/* The photograph holds what was there, not what replaced it.
 *
 * The whole claim, end to end and in the order it really happens: the
 * text is on screen, the effect is handed over, the deletion draws
 * over it, and the first frame is painted after that.  Without the
 * photograph the frame shows what closed up into the gap. */
static void test_the_photograph_holds_the_old_pixels(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);

    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing: %s\n",
               st ? smear_stage_trouble(st) : "cannot open stage");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w);
        return;
    }

    GC gc = XCreateGC(D, w, 0, NULL);
    const unsigned long WAS = 0x00ff0000;   /* the text, before */
    const unsigned long NOW = 0x000000ff;   /* what closed up over it */
    const int RX = 20, RY = 20, RW = 60, RH = 24;

    XSetForeground(D, gc, WAS);
    XFillRectangle(D, w, gc, RX, RY, RW, RH);
    XSync(D, False);

    /* before-change-functions: the pixels are still the old ones */
    int took = smear_stage_freeze(st, RX, RY, RW, RH);

    /* Once before the deletion is drawn.  Partly to tell a photograph
       holding the wrong pixels from one that is never laid down, and
       partly because this is the frame the overlay is mapped on: a
       paint that lands while it is being mapped can be lost, and in
       use nothing depends on the first frame -- the photograph goes
       back down on every one of them. */
    int up0 = smear_stage_begin(st);
    smear_stage_frame_begin(st, RX, RY, RW, RH);
    smear_stage_restore_frozen(st);
    smear_stage_frame_end(st);
    nap(0.08);
    unsigned long before = up0 ? overlay_pixel(st, RX + RW / 2, RY + RH / 2) : 0;
    {
        char w0[120];
        snprintf(w0, sizeof w0, "overlay up %d, pixel 0x%06lx, wanted 0x%06lx",
                 up0, before, WAS);
        ok(up0 && before == WAS, "the-overlay-shows-the-text-it-marks", w0);
    }

    /* the deletion happens, and Emacs redraws */
    XSetForeground(D, gc, NOW);
    XFillRectangle(D, w, gc, RX, RY, RW, RH);
    XSync(D, False);

    /* and only now is the first frame painted, in the order
       paint_snapshot paints it: the backdrop, which is the window as
       it now is, then the photograph over it, then the layers. */
    int up = smear_stage_begin(st);
    smear_stage_frame_begin(st, RX, RY, RW, RH);
    smear_stage_restore_frozen(st);
    /* The stage has its own connection, so a round trip on ours orders
       nothing against it, and `begin' does not flush; in use the
       flush comes with the frame.  So flush it, and let the server
       catch up before looking. */
    smear_stage_frame_end(st);
    nap(0.08);
    XSync(D, False);

    /* Through a pixmap of our own.  Reading a window directly is only
       legal while it is viewable and unobscured, and this one is a
       stage's overlay on somebody's desktop: XGetImage on it answers
       BadMatch as often as not.  A pixmap can always be read. */
    unsigned long seen = up ? overlay_pixel(st, RX + RW / 2, RY + RH / 2) : 0;
    int readable = (seen != 0);
    char why[200];
    snprintf(why, sizeof why,
             "freeze %d, overlay up %d, pixel 0x%06lx (was 0x%06lx, now 0x%06lx)",
             took, up, seen, WAS, NOW);
    if (!took || !up || !readable)
        printf("skip: cannot see the overlay -- %s\n", why);
    else
        ok(seen == WAS, "the-photograph-holds-the-old-pixels", why);

    XFreeGC(D, gc);
    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* A photograph outlives what it is of.
 *
 * The case: text is about to be deleted, the effect marking it is
 * handed over before the deletion, and the first frame is painted
 * after it.  Freeze has to hold the old pixels through that. */
static void test_a_photograph_outlives_the_text(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapWindow(D, w);
    XSync(D, False);

    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing: %s\n",
               st ? smear_stage_trouble(st) : "cannot open stage");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w);
        return;
    }

    /* Frozen before anything is drawn over it, then asked for again
       after the window has moved on.  What matters here is that the
       freeze takes and the stage still holds its rectangle: the pixels
       themselves are the server's business, and reading them back
       through a compositing manager tests the manager. */
    int took = smear_stage_freeze(st, 10, 10, 40, 20);
    smear_stage_restore_frozen(st);          /* must not crash unmapped */
    XSync(D, False);
    ok(took == 1, "a-photograph-outlives-the-text", "freeze did not take");

    /* And one that cannot be taken says so rather than half-taking. */
    int refused = smear_stage_freeze(st, 0, 0, 0, 0);
    ok(refused == 0, "an-empty-photograph-is-refused", "empty freeze took");

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* A stamp is the same pixels at a different alpha.
 *
 * The claim behind rendering an effect once: frames after the first
 * are a composite the server does by itself, and what comes out is
 * what the renderer would have produced again anyway, only fainter
 * where the envelope says fainter. */
static void test_a_stamp_replays_at_the_alpha_asked_for(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);

    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing: %s\n",
               st ? smear_stage_trouble(st) : "cannot open stage");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w);
        return;
    }
    {
        char gerr[256] = {0};
        smear_stage_set_renderer(st, SMEAR_RENDERER_GL, gerr, sizeof gerr);
    }
    if (smear_stage_renderer(st) != SMEAR_RENDERER_GL) {
        printf("skip: stamps are the GL renderer's\n");
        smear_stage_close(st);
        XDestroyWindow(D, w);
        return;
    }

    const int RX = 20, RY = 20, RW = 60, RH = 30;
    SmearPaint p;
    quad(&p, RX, RY, RX + RW, RY + RH);
    p.r = 1.0; p.g = 1.0; p.b = 1.0;
    p.nstops = 2;
    p.stop_at[0] = 0.0; p.stop_alpha[0] = 1.0;
    p.stop_at[1] = 1.0; p.stop_alpha[1] = 1.0;

    if (!smear_stage_begin(st)) {
        printf("skip: the overlay would not go up\n");
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }
    /* black underneath, so what is read back is the stamp's own doing */
    smear_stage_frame_begin(st, RX, RY, RW, RH);
    GC gc = XCreateGC(D, w, 0, NULL);
    XSetForeground(D, gc, 0x000000);
    XFillRectangle(D, w, gc, 0, 0, W, H);
    XSync(D, False);

    smear_stage_frame_begin(st, RX, RY, RW, RH);
    smear_stage_draw(st, &p);
    int took = smear_stage_stamp_take(st, 0, RX, RY, RW, RH);
    int full = smear_stage_stamp_replay(st, 0, 1.0);
    smear_stage_frame_end(st);
    nap(0.08);
    unsigned long at_full = overlay_pixel(st, RX + RW / 2, RY + RH / 2);

    /* the same stamp again, a quarter as strong, over a fresh backdrop */
    smear_stage_frame_begin(st, RX, RY, RW, RH);
    int faint = smear_stage_stamp_replay(st, 0, 0.25);
    smear_stage_frame_end(st);
    nap(0.08);
    unsigned long at_quarter = overlay_pixel(st, RX + RW / 2, RY + RH / 2);

    char why[220];
    snprintf(why, sizeof why,
             "took %d, replays %d/%d, full 0x%06lx, quarter 0x%06lx",
             took, full, faint, at_full, at_quarter);
    if (!took || !full || !faint || !at_full)
        printf("skip: cannot see the overlay -- %s\n", why);
    else
        /* white over black: brighter at one than at a quarter, and the
           quarter is not nothing */
        ok((at_full & 0xff) > (at_quarter & 0xff) && (at_quarter & 0xff) > 0,
           "a-stamp-replays-at-the-alpha-asked-for", why);

    XFreeGC(D, gc);
    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* A still flight uploads once, not once a frame.
 *
 * The whole point of the stamp, stated as the only number that
 * matters: boxes read back off the GPU and pushed to the display. */
static void test_a_still_flight_uploads_once(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char gerr[256] = {0};
    smear_stage_set_renderer(st, SMEAR_RENDERER_GL, gerr, sizeof gerr);
    if (smear_stage_renderer(st) != SMEAR_RENDERER_GL) {
        printf("skip: stamps are the GL renderer's\n");
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }
    if (smear_play_start(st, gerr, sizeof gerr)) {
        printf("skip: no player: %s\n", gerr);
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }

    /* twenty frames of a shape that does not move, only fades */
    SmearPaint layer;
    quad(&layer, 20, 20, 120, 50);
    layer.nstops = 2;
    layer.stop_at[0] = 0.0; layer.stop_alpha[0] = 1.0;
    layer.stop_at[1] = 1.0; layer.stop_alpha[1] = 1.0;
    SmearFrame frames[20];
    memset(frames, 0, sizeof frames);
    for (int k = 0; k < 20; k++) {
        frames[k].bx = 18; frames[k].by = 18;
        frames[k].bw = 104; frames[k].bh = 34;
        double c[8] = {20, 20, 120, 20, 120, 50, 20, 50};
        memcpy(frames[k].corners[0], c, sizeof c);
        frames[k].head[0][0] = 20; frames[k].head[0][1] = 35;
        frames[k].alpha[0] = 1.0 - (double)k / 20.0;
    }

    unsigned long before = smear_stage_uploads();
    smear_play_flight(st, 1, &layer, 1, frames, 20, 60.0, 0, 1);
    nap(0.6);                            /* the whole flight and then some */
    unsigned long still_cost = smear_stage_uploads() - before;

    before = smear_stage_uploads();
    smear_play_flight(st, 1, &layer, 1, frames, 20, 60.0, 0, 0);
    nap(0.6);
    unsigned long moving_cost = smear_stage_uploads() - before;

    char why[200];
    snprintf(why, sizeof why, "still %lu upload(s), the same flight moving %lu",
             still_cost, moving_cost);
    if (moving_cost < 5)
        printf("skip: the flight did not play -- %s\n", why);
    else
        ok(still_cost == 1 && moving_cost > 5,
           "a-still-flight-uploads-once", why);
    printf("     (%s)\n", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* Two tracks, two rates.
 *
 * The player kept one frame rate for all of them, set by whichever
 * flight was handed over last, so an effect at thirty frames a second
 * re-paces the trail beside it to thirty and a trail at sixty plays at
 * half speed for as long as an effect is on screen.
 *
 * The two tests next door hand two tracks flights of different
 * lengths, and neither hands two different rates, because until
 * effects had a rate of their own nothing could.  This is that case. */
static void test_two_tracks_keep_their_own_rates(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        printf("skip: no player: %s\n", err);
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }

    SmearPaint layer;
    quad(&layer, 10, 10, 60, 40);
    SmearFrame frames[60];
    memset(frames, 0, sizeof frames);
    for (int k = 0; k < 60; k++) {
        frames[k].bx = 8; frames[k].by = 8;
        frames[k].bw = 54; frames[k].bh = 34;
        double c[8] = {10, 10, 60, 10, 60, 40, 10, 40};
        memcpy(frames[k].corners[0], c, sizeof c);
        frames[k].alpha[0] = 1.0;
    }

    /* the trail at sixty, an effect beside it at fifteen */
    smear_play_flight(st, SMEAR_TRACK_TRAIL, &layer, 1, frames, 60, 60.0, 0, 0);
    smear_play_flight(st, 1, &layer, 1, frames, 60, 15.0, 0, 0);
    nap(0.30);
    int fast = smear_play_position(st, SMEAR_TRACK_TRAIL);
    int slow = smear_play_position(st, 1);

    char why[160];
    snprintf(why, sizeof why,
             "after 0.30 s the sixty-a-second track is on frame %d "
             "and the fifteen-a-second one on %d", fast, slow);
    /* about 18 and about 4; what matters is that they differ by a lot
       and in the right direction */
    ok(fast > slow * 2 && slow >= 1, "two-tracks-keep-their-own-rates", why);
    printf("     (%s)\n", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* The overlay comes down between flights.
 *
 * Not for tidiness: an override-redirect window sitting on top of the
 * frame costs it the keyboard.  Watched continuously on the display
 * this runs on, focus was `None' for as long as the overlay was above
 * the Emacs window and returned the moment the window was above it
 * again.  Keystrokes go nowhere, which from the outside is
 * indistinguishable from Emacs having hung.
 *
 * Staying mapped was worth 33.6 ms a flight, which is what a map costs
 * here.  It was not worth the keyboard. */
static void test_the_overlay_comes_down_between_flights(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        printf("skip: no player: %s\n", err);
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }

    SmearPaint layer;
    quad(&layer, 20, 20, 80, 50);
    SmearFrame frames[4];
    memset(frames, 0, sizeof frames);
    for (int k = 0; k < 4; k++) {
        frames[k].bx = 18; frames[k].by = 18;
        frames[k].bw = 64; frames[k].bh = 34;
        double c[8] = {20, 20, 80, 20, 80, 50, 20, 50};
        memcpy(frames[k].corners[0], c, sizeof c);
        frames[k].alpha[0] = 1.0;
    }
    smear_play_flight(st, 1, &layer, 1, frames, 4, 60.0, 0, 1);
    /* past the end of the flight *and* past the moment it lingers for
       in case another follows */
    nap(0.9);

    /* Down, and down means unmapped.
     *
     * A raise is not a substitute.  Measured on an X server reached
     * over ssh -X, `XRaiseWindow' on the mapped overlay left the
     * stacking order exactly as it was, and so did an explicit
     * `XConfigureWindow' with `Above'.  The overlay stayed under the
     * frame, correctly shaped and full of ink nobody could see.  The
     * only restack that server honours is the one that comes with a
     * map.  So the overlay goes down between flights and comes up
     * raised, and `smear_stage_frame_begin' shapes it before it does.
     *
     * The keyboard is the reason this was ever wanted.  Watched
     * continuously, focus was `None' for as long as the overlay was
     * above the Emacs window and came back the moment the window was
     * above it again. */
    XWindowAttributes a;
    int got = XGetWindowAttributes(D, smear_stage_overlay(st), &a);
    char why[160];
    snprintf(why, sizeof why, "after the flight the overlay is %s",
             !got ? "gone" : a.map_state == IsViewable ? "still up" : "down");
    ok(got && a.map_state != IsViewable,
       "the-overlay-comes-down-between-flights", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* The overlay is shaped before it is shown.
 *
 * The order is the whole point, and both displays this has been broken
 * on broke on it.
 *
 * A compositor works out the region it will composite for a window
 * when that window comes up, and drops damage falling outside it.  Come
 * up shapeless and grow afterwards and the growth is never composited:
 * under picom a full-width line pulse played after a cursor-sized
 * trail reached the screen as the 96 pixels the trail had shaped and
 * nothing else.  Shape first and the region is right from the start.
 *
 * The map has to stay, though.  See
 * `the-overlay-comes-down-between-flights' for the server that
 * restacks on nothing else.  So: shape, then map, in that order, and
 * this watches the two events go by to say so.
 *
 * Watched from this connection, because the stage's own asks for no
 * events at all. */
static void test_the_overlay_is_shaped_before_it_is_shown(void)
{
    int shape_ev = 0, shape_err = 0;
    if (!XShapeQueryExtension(D, &shape_ev, &shape_err)) {
        printf("skip: no SHAPE\n");
        return;
    }
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        printf("skip: no player: %s\n", err);
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }

    Window ov = smear_stage_overlay(st);
    XSelectInput(D, ov, StructureNotifyMask);
    XShapeSelectInput(D, ov, ShapeNotifyMask);
    XSync(D, False);
    while (XPending(D)) { XEvent e; XNextEvent(D, &e); }   /* drain */

    SmearPaint layer;
    quad(&layer, 20, 20, 80, 50);
    SmearFrame frames[4];
    memset(frames, 0, sizeof frames);
    for (int k = 0; k < 4; k++) {
        frames[k].bx = 18; frames[k].by = 18;
        frames[k].bw = 64; frames[k].bh = 34;
        double c[8] = {20, 20, 80, 20, 80, 50, 20, 50};
        memcpy(frames[k].corners[0], c, sizeof c);
        frames[k].alpha[0] = 1.0;
    }
    smear_play_flight(st, 1, &layer, 1, frames, 4, 60.0, 0, 1);
    nap(0.5);

    /* The shape standing when the map went by.  `smear_stage_begin'
       clears the shape while the overlay is down, so an empty one here
       is exactly the fault being watched for. */
    int mapped = 0, shaped_first = 0;
    int sw = -1, sh = -1;
    while (XPending(D)) {
        XEvent e;
        XNextEvent(D, &e);
        if (e.type == shape_ev + ShapeNotify) {
            XShapeEvent *se = (XShapeEvent *)&e;
            if (se->kind == ShapeBounding && !mapped) {
                sw = (int)se->width; sh = (int)se->height;
            }
        } else if (e.type == MapNotify && !mapped) {
            mapped = 1;
            shaped_first = (sw > 0 && sh > 0);
        }
    }

    char why[190];
    if (!mapped)
        snprintf(why, sizeof why, "the overlay never came up");
    else
        snprintf(why, sizeof why,
                 "the shape standing when it came up was %dx%d", sw, sh);
    ok(mapped && shaped_first,
       "the-overlay-is-shaped-before-it-is-shown", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* Photographing from another thread while a flight plays.
 *
 * Which is what happens on every deletion: the player is painting a
 * typing blink on its own thread while Emacs's thread calls
 * `smear_stage_freeze' to keep the pixels about to be removed.  Xlib
 * is not thread-safe, and two threads in one connection corrupt the
 * protocol stream or block on the socket, seen as Emacs freezing
 * until it is killed.
 *
 * A thousand freezes against a running flight; the test is that it
 * finishes at all, and that the display is still answering. */
struct freezer { SmearStage *st; int stop; int done; };

static void *freeze_away(void *arg)
{
    struct freezer *f = arg;
    while (!f->stop) {
        smear_stage_freeze(f->st, 10, 10, 40, 20);
        f->done++;
    }
    return NULL;
}

static void test_freezing_while_a_flight_plays(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        printf("skip: no player: %s\n", err);
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }

    SmearPaint layer;
    quad(&layer, 20, 20, 120, 60);
    SmearFrame frames[60];
    memset(frames, 0, sizeof frames);
    for (int k = 0; k < 60; k++) {
        frames[k].bx = 18; frames[k].by = 18;
        frames[k].bw = 104; frames[k].bh = 44;
        double c[8] = {20, 20, 120, 20, 120, 60, 20, 60};
        memcpy(frames[k].corners[0], c, sizeof c);
        frames[k].alpha[0] = 1.0;
    }
    smear_play_flight(st, 1, &layer, 1, frames, 60, 60.0, 0, 0);

    struct freezer f = { st, 0, 0 };
    pthread_t tid;
    pthread_create(&tid, NULL, freeze_away, &f);
    nap(0.5);                            /* both threads in Xlib together */
    f.stop = 1;
    pthread_join(tid, NULL);

    /* the connection still speaks: a round trip that would hang or
       raise if the stream had been corrupted */
    XWindowAttributes a;
    int alive = XGetWindowAttributes(D, w, &a);

    char why[160];
    snprintf(why, sizeof why, "%d freezes against a running flight, display %s",
             f.done, alive ? "still answering" : "gone");
    ok(alive && f.done > 10, "freezing-while-a-flight-plays", why);
    printf("     (%s)\n", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* A flight with nothing to draw must not wedge the caller.
 *
 * `snapshot' returns zero when no track has a box worth painting.  The
 * loop then goes round again, holding the lock and not waiting, so a
 * track that is still playing but has an empty box spins at
 * whatever rate the machine manages, with the mutex held.
 *
 * That mutex is what `smear_play_flight' takes, and
 * `smear_play_flight' runs on Emacs's thread.  So a spin here is not a
 * bad animation: it is Emacs stopped dead, which is the one failure
 * this whole file is arranged to prevent. */
static void test_a_flight_with_nothing_to_draw_does_not_wedge(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        printf("skip: no player: %s\n", err);
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }

    SmearPaint layer;
    quad(&layer, 20, 20, 80, 50);
    /* forty frames, every one of them empty */
    SmearFrame empty[40];
    memset(empty, 0, sizeof empty);
    for (int k = 0; k < 40; k++) {
        empty[k].bw = 0; empty[k].bh = 0;
        empty[k].alpha[0] = 1.0;
    }
    smear_play_flight(st, 1, &layer, 1, empty, 40, 60.0, 0, 0);
    nap(0.10);                          /* let it get going */

    /* now hand over another, as a keystroke would, and time the wait */
    SmearFrame real[4];
    memset(real, 0, sizeof real);
    for (int k = 0; k < 4; k++) {
        real[k].bx = 18; real[k].by = 18; real[k].bw = 64; real[k].bh = 34;
        double c[8] = {20, 20, 80, 20, 80, 50, 20, 50};
        memcpy(real[k].corners[0], c, sizeof c);
        real[k].alpha[0] = 1.0;
    }
    double a = mono_test();
    smear_play_flight(st, 0, &layer, 1, real, 4, 60.0, 0, 0);
    double waited = (mono_test() - a) * 1000.0;

    char why[160];
    snprintf(why, sizeof why, "handing over took %.2f ms", waited);
    ok(waited < 20.0, "a-flight-with-nothing-to-draw-does-not-wedge", why);
    printf("     (%s)\n", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* A wedged player must not take Emacs with it.
 *
 * Whatever goes wrong in the player, whether a spin, a blocked socket
 * or a bug not yet found, the calls Emacs's thread makes on every
 * keystroke must come back.  This one wedges the player deliberately,
 * by handing it a flight of frames that take a very long time, then
 * holding its lock from another thread, and checks that a handover
 * still returns.  A dropped animation is a shrug; a frozen editor is
 * not. */
struct wedger { SmearStage *st; int go; int held; };

static void *hold_the_lock(void *arg)
{
    struct wedger *w = arg;
    /* stop the player dead by keeping it from its own state, the way a
       spin or a blocked X call inside the critical section would */
    smear_play_wedge_for_test(w->st, 400);   /* milliseconds */
    w->held = 1;
    return NULL;
}

static void test_a_wedged_player_does_not_freeze_the_caller(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        printf("skip: no player: %s\n", err);
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }

    struct wedger wg = { st, 0, 0 };
    pthread_t tid;
    pthread_create(&tid, NULL, hold_the_lock, &wg);
    nap(0.05);                          /* let it take the lock */

    SmearPaint layer;
    quad(&layer, 20, 20, 80, 50);
    SmearFrame fr[4];
    memset(fr, 0, sizeof fr);
    for (int k = 0; k < 4; k++) {
        fr[k].bx = 18; fr[k].by = 18; fr[k].bw = 64; fr[k].bh = 34;
        double c[8] = {20, 20, 80, 20, 80, 50, 20, 50};
        memcpy(fr[k].corners[0], c, sizeof c);
        fr[k].alpha[0] = 1.0;
    }
    double a = mono_test();
    smear_play_flight(st, 0, &layer, 1, fr, 4, 60.0, 0, 0);
    int pos = smear_play_position(st, 0);
    SmearPlayStats stats;
    smear_play_stats(st, &stats);
    double waited = (mono_test() - a) * 1000.0;
    (void)pos;

    pthread_join(tid, NULL);
    char why[160];
    snprintf(why, sizeof why,
             "three calls against a player held for 400 ms took %.2f ms",
             waited);
    ok(waited < 100.0, "a-wedged-player-does-not-freeze-the-caller", why);
    printf("     (%s)\n", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* A burst of typing pays one map, not one per keystroke.
 *
 * Putting the overlay on screen costs a map, 33.6 ms on a forwarded
 * display and the cheapest it honours, so a flight per keystroke
 * paying a map per flight is the typing lag itself.  It lingers
 * between flights instead, and goes away when the typing stops. */
static void test_a_burst_of_flights_pays_one_map(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        printf("skip: no player: %s\n", err);
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }

    SmearPaint layer;
    quad(&layer, 20, 20, 80, 50);
    SmearFrame fr[6];
    memset(fr, 0, sizeof fr);
    for (int k = 0; k < 6; k++) {
        fr[k].bx = 18; fr[k].by = 18; fr[k].bw = 64; fr[k].bh = 34;
        double c[8] = {20, 20, 80, 20, 80, 50, 20, 50};
        memcpy(fr[k].corners[0], c, sizeof c);
        fr[k].alpha[0] = 1.0;
    }

    /* Ten keystrokes at a comfortable typing rate.  Each flight is
       six frames -- a tenth of a second -- and they are far enough
       apart that every one of them ends before the next arrives, so
       the overlay really does fall idle in between.  Overlapping them
       would keep it up for a quite different reason and prove
       nothing. */
    unsigned long before = smear_stage_maps();
    for (int i = 0; i < 10; i++) {
        smear_play_flight(st, 2, &layer, 1, fr, 6, 60.0, 0, 1);
        nap(0.22);
    }
    unsigned long burst = smear_stage_maps() - before;

    /* then a pause long enough for it to go away, and one more */
    nap(0.8);
    before = smear_stage_maps();
    smear_play_flight(st, 2, &layer, 1, fr, 6, 60.0, 0, 1);
    nap(0.25);
    unsigned long after_pause = smear_stage_maps() - before;

    char why[190];
    /* One, not ten: a flight must not pay a map each. */
    snprintf(why, sizeof why,
             "ten flights a tenth of a second apart cost %lu map(s); "
             "one after a pause cost %lu", burst, after_pause);
    ok(burst == 1 && after_pause == 1, "a-burst-of-flights-pays-one-map", why);
    printf("     (%s)\n", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* Two tracks at sixty do not make a hundred and twenty.
 *
 * Waking for whichever track's next frame comes first is not enough.
 * Two tracks at the same rate but out of phase, a typing blink over a
 * trail being what typing while moving looks like, then wake it twice
 * per frame interval, painting a whole frame each time and
 * putting the trail on the wire twice as often as it has frames.
 * Reported as "gap 8.4 typical" against a 16.7 ms budget. */
static void test_two_tracks_do_not_double_the_frame_rate(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    if (smear_play_start(st, err, sizeof err)) {
        printf("skip: no player: %s\n", err);
        smear_stage_close(st); XDestroyWindow(D, w); return;
    }

    SmearPaint layer;
    quad(&layer, 20, 20, 80, 50);
    SmearFrame fr[36];                   /* 36 frames = 0.6 s at sixty */
    memset(fr, 0, sizeof fr);
    for (int k = 0; k < 36; k++) {
        fr[k].bx = 18; fr[k].by = 18; fr[k].bw = 64; fr[k].bh = 34;
        double c[8] = {20, 20, 80, 20, 80, 50, 20, 50};
        memcpy(fr[k].corners[0], c, sizeof c);
        fr[k].alpha[0] = 1.0;
    }

    /* Get the overlay up first and let that flight end.  The two that
       matter are then handed over while it is already mapped, which is
       what the linger arranges in a real session -- and is the case
       where the clocks are not reset into phase by the mapping.
       Without this the bug hides: both tracks start together and the
       fault needs them apart. */
    smear_play_flight(st, 0, &layer, 1, fr, 6, 60.0, 0, 0);
    nap(0.25);

    smear_play_flight(st, 0, &layer, 1, fr, 36, 60.0, 0, 0);
    nap(0.008);                          /* half a frame out of phase */
    smear_play_flight(st, 2, &layer, 1, fr, 36, 60.0, 0, 0);
    nap(0.9);                            /* both finish */

    SmearPlayStats got;
    int have = smear_play_stats(st, &got);
    double per_frame = have && got.frames > 1
        ? (got.gap_sum / (got.frames - 1)) * 1000.0 : 0.0;

    char why[190];
    snprintf(why, sizeof why,
             "two tracks of 36 frames at sixty painted %d frames, "
             "%.1f ms apart", have ? got.frames : -1, per_frame);
    /* 36 frames' worth, not 72; and paced at the frame interval */
    ok(have && got.frames <= 48 && per_frame > 12.0,
       "two-tracks-do-not-double-the-frame-rate", why);
    printf("     (%s)\n", why);

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* The backdrop under a trail is the window, not black.
 *
 * Reported as a red dot sitting in a black box.  Everything about the
 * trail was right; what was wrong was underneath it.  The overlay
 * shows the window's own pixels wherever no layer reaches, copied
 * there by `smear_stage_frame_begin' from the pixmap that holds the
 * window's contents, and a copy from a stale one raises BadDrawable
 * and draws nothing, leaving whatever the overlay had, which shortly
 * after being mapped is undefined. */
static void test_the_backdrop_under_a_trail_is_the_window(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    smear_stage_set_renderer(st, SMEAR_RENDERER_GL, err, sizeof err);

    /* the window has a colour of its own, which is what should show
       through wherever the trail does not reach */
    const unsigned long PAPER = 0x00204060;
    GC gc = XCreateGC(D, w, 0, NULL);
    XSetForeground(D, gc, PAPER);
    XFillRectangle(D, w, gc, 0, 0, W, H);
    XSync(D, False);

    /* a layer in one corner of a much larger box */
    SmearPaint layer;
    quad(&layer, 20, 20, 40, 40);
    if (!smear_stage_begin(st)) {
        printf("skip: the overlay would not go up\n");
        XFreeGC(D, gc); smear_stage_close(st); XDestroyWindow(D, w); return;
    }
    smear_stage_frame_begin(st, 8, 8, 110, 110);
    smear_stage_draw(st, &layer);
    smear_stage_frame_end(st);
    nap(0.10);

    /* far from the layer, still inside the box and inside the window */
    unsigned long seen = overlay_pixel(st, 100, 100);

    char why[190];
    snprintf(why, sizeof why,
             "the box away from the trail reads 0x%06lx, the window is 0x%06lx",
             seen, PAPER);
    /* Nought is not "cannot see it" here: black is the failure this
       exists to catch, and skipping on it would hide the very thing. */
    ok(seen == PAPER, "the-backdrop-under-a-trail-is-the-window", why);

    XFreeGC(D, gc);
    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* Every layer handed to the stage is drawn, up to the capacity the
 * module advertises.
 *
 * The GL renderer holds layers back and draws them in one pass, so the
 * number it can hold is a second capacity behind the one callers are
 * told about.  When the advertised one was raised the holding array
 * was not, and the layers past it were dropped without a word: the
 * draw succeeded, the pixmap came back, and an effect of thirteen
 * layers arrived with the last five missing.  For Pacman that meant
 * the ghosts turned up and the character they chase did not.
 *
 * Drawn through the stage rather than through the renderer, because
 * the renderer was never the part that was wrong.  One opaque bar per
 * layer, side by side, so a dropped layer is a gap at a known place. */
static void test_the_stage_draws_every_layer(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    char err[256] = {0};
    smear_stage_set_renderer(st, SMEAR_RENDERER_GL, err, sizeof err);

    const unsigned long PAPER = 0x00204060;
    GC gc = XCreateGC(D, w, 0, NULL);
    XSetForeground(D, gc, PAPER);
    XFillRectangle(D, w, gc, 0, 0, W, H);
    XSync(D, False);

    if (!smear_stage_begin(st)) {
        printf("skip: the overlay would not go up\n");
        XFreeGC(D, gc); smear_stage_close(st); XDestroyWindow(D, w); return;
    }
    /* As many as the module tells callers it holds: the flight's
       arrays, or the shader's share of them where that is smaller. */
    int cap = SMEAR_MAX_PLAY_LAYERS;
    int gl = smear_gl_max_layers();
    if (gl > 0 && gl < cap) cap = gl;

    smear_stage_frame_begin(st, 0, 0, W, H);
    for (int i = 0; i < cap; i++) {
        SmearPaint p;
        quad(&p, 2 + i * 4, 40, 2 + i * 4 + 3, 80);
        smear_stage_draw(st, &p);
    }
    smear_stage_frame_end(st);
    nap(0.10);

    int drawn = 0, first_missing = -1;
    for (int i = 0; i < cap; i++) {
        if (overlay_pixel(st, 2 + i * 4 + 1, 60) != PAPER) drawn++;
        else if (first_missing < 0) first_missing = i;
    }
    char why[190];
    snprintf(why, sizeof why, "handed %d layers, %d drawn, first missing is %d",
             cap, drawn, first_missing);
    ok(drawn == cap, "the-stage-draws-every-layer", why);

    XFreeGC(D, gc);
    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* A frame reaches the overlay finished, or not at all.
 *
 * The first thing a frame does is lay the window's own text down as
 * its backdrop, and the effect goes over that.  Drawn straight into
 * the overlay, the window holds the live text for the moment between
 * the two, and a compositor that samples then presents it.  A trail
 * over the same text hides nothing and never showed it.  A band whose
 * whole job is to cover text flickered at random, because whether it
 * showed depended on the compositor's timing rather than on ours.
 *
 * So: with a frame begun and not yet ended, the overlay must still be
 * holding the frame before it. */
static void test_a_half_made_frame_is_not_shown(void)
{
    Window w = XCreateWindow(D, DefaultRootWindow(D), 0, 0, W, H, 0,
                             CopyFromParent, InputOutput, CopyFromParent,
                             0, NULL);
    XMapRaised(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing\n");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w); return;
    }
    GC gc = XCreateGC(D, w, 0, NULL);
    const unsigned long FIRST = 0x00204060, SECOND = 0x00c08040;
    XSetForeground(D, gc, FIRST);
    XFillRectangle(D, w, gc, 0, 0, W, H);
    XSync(D, False);

    if (!smear_stage_begin(st)) {
        printf("skip: the overlay would not go up\n");
        XFreeGC(D, gc); smear_stage_close(st); XDestroyWindow(D, w); return;
    }
    smear_stage_frame_begin(st, 0, 0, W, H);
    smear_stage_frame_end(st);
    nap(0.05);
    unsigned long shown = overlay_pixel(st, 60, 60);

    /* The window changes underneath, and a frame begins: the backdrop
       for it is the new colour, but nothing is finished yet. */
    XSetForeground(D, gc, SECOND);
    XFillRectangle(D, w, gc, 0, 0, W, H);
    XSync(D, False);
    smear_stage_frame_begin(st, 0, 0, W, H);
    XSync(D, False);
    unsigned long during = overlay_pixel(st, 60, 60);
    smear_stage_frame_end(st);
    nap(0.05);
    unsigned long after = overlay_pixel(st, 60, 60);

    char why[220];
    snprintf(why, sizeof why,
             "was 0x%06lx, mid-frame 0x%06lx (wanted the old one), "
             "after 0x%06lx (wanted 0x%06lx)",
             shown, during, after, SECOND);
    ok(during == shown && after == SECOND,
       "a-half-made-frame-is-not-shown", why);

    XFreeGC(D, gc);
    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

/* Every track the header offers can be flown on, and every one of them
 * can keep a stamp.
 *
 * The cursor's own glow is a flight that never ends, so it cannot
 * share a track with anything: given the trail's, it could only be
 * aimed again between flights and it lagged a moving cursor by a whole
 * turn.  A stage and a player of its own, because a flight offered
 * while the player is painting four others waits on the lock and may
 * be refused, and that is a busy moment rather than a track that is
 * not there. */
static void test_every_track_takes_a_flight(void)
{
    Window w = XCreateSimpleWindow(D, DefaultRootWindow(D), 0, 0, 200, 100,
                                   0, 0, 0);
    XMapWindow(D, w);
    XSync(D, False);
    SmearStage *st = smear_stage_open(NULL, w);
    if (!st || smear_stage_trouble(st)) {
        printf("skip: no compositing: %s\n",
               st ? smear_stage_trouble(st) : "cannot open stage");
        if (st) smear_stage_close(st);
        XDestroyWindow(D, w);
        return;
    }
    char err[128];
    if (smear_play_start(st, err, sizeof err)) {
        printf("skip: no player: %s\n", err);
        smear_stage_close(st);
        XDestroyWindow(D, w);
        return;
    }
    SmearPaint one = { 0 };
    SmearFrame f[2] = { { 0 } };
    int flew = 1, refused_at = -1;
    for (int i = 0; i < 2; i++) { f[i].bw = 10; f[i].bh = 10; f[i].alpha[0] = 1.0; }
    for (int t = 0; t < SMEAR_MAX_TRACKS; t++)
        if (!smear_play_flight(st, t, &one, 1, f, 2, 1.0, 0, 0)) {
            flew = 0;
            refused_at = t;
        }
    char why[64] = "";
    if (refused_at >= 0)
        snprintf(why, sizeof why, "track %d of %d refused a flight",
                 refused_at, SMEAR_MAX_TRACKS);
    ok(flew == 1, "every-track-takes-a-flight", why);
    ok(SMEAR_STAGE_STAMPS >= SMEAR_MAX_TRACKS,
       "every-track-can-keep-a-stamp", "fewer stamps than tracks");

    smear_play_shutdown(st);
    smear_stage_close(st);
    XDestroyWindow(D, w);
    XSync(D, False);
}

int main(void)
{
    D = XOpenDisplay(NULL);
    if (!D) { printf("skip: no display\n"); return 77; }
    char err[256] = {0};
    if (smear_gl_init(err, sizeof err)) { printf("skip: no gl: %s\n", err); return 77; }

    PM = XCreatePixmap(D, DefaultRootWindow(D), W, H, 32);
    DST = XRenderCreatePicture(D, PM,
                               XRenderFindStandardFormat(D, PictStandardARGB32),
                               0, NULL);

    test_lands_where_asked();
    test_diagonal_keeps_its_slope();
    test_head_end_is_bright();
    test_glow_centres_on_head();
    test_flight_finishes_while_the_caller_is_blocked();
    test_retarget_replaces_a_flight_in_progress();
    test_a_short_track_does_not_end_a_long_one();
    test_stopping_one_track_leaves_the_others();
    test_two_tracks_keep_their_own_rates();
    test_two_tracks_do_not_double_the_frame_rate();
    test_the_overlay_comes_down_between_flights();
    test_the_overlay_is_shaped_before_it_is_shown();
    test_a_burst_of_flights_pays_one_map();
    test_freezing_while_a_flight_plays();
    test_a_flight_with_nothing_to_draw_does_not_wedge();
    test_every_track_takes_a_flight();
    test_a_wedged_player_does_not_freeze_the_caller();
    test_a_photograph_outlives_the_text();
    test_the_photograph_holds_the_old_pixels();
    test_the_backdrop_under_a_trail_is_the_window();
    test_a_stamp_replays_at_the_alpha_asked_for();
    test_a_still_flight_uploads_once();
    test_the_stage_draws_every_layer();
    test_a_half_made_frame_is_not_shown();

    printf("%s\n", failures ? "FAILED" : "all pass");
    return failures ? 1 : 0;
}
