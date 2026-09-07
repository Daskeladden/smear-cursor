/* shot -- what the overlay window actually holds.
 *
 * The watch tool asks the server where the overlay is and what its
 * shape lets through; it says nothing about the pixels.  A screen
 * grab cannot answer that either: the overlay is override-redirect
 * and a compositor presents it, so what a grabber reads from the root
 * is whatever the compositor chose to show.
 *
 * This reads the window itself.  Under a compositor every window is
 * redirected to its own backing store, so XGetImage on the overlay
 * returns what was drawn into it whether or not it reached the
 * screen.  That separates "the effect was never drawn" from "the
 * effect was drawn and is not being presented", which is the question
 * an invisible animation always raises.
 *
 *   smear-cursor-x11-shot DIR HZ SECONDS [X Y W H]
 *
 * A region rather than the whole overlay: the overlay is the size of
 * the frame, and reading a screenful back over a forwarded connection
 * takes seconds, which is longer than the animation being caught.
 *
 * One PPM per sample in DIR and a line per written frame:
 * FILE ELAPSED W H DEPTH PAINTED-PIXELS COLOURS
 */
#define _POSIX_C_SOURCE 200809L
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static double mono(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + ts.tv_nsec / 1e9;
}

/* Windows come and go while the tree is walked, and the overlay
 * itself is released and remade between flights.  Asking about one
 * that has just gone is expected here, not a fault to die on. */
static int ignore_x_error(Display *d, XErrorEvent *e)
{
    (void)d; (void)e;
    return 0;
}

#define MAX_OV 16
static Window ovs[MAX_OV];
static int    novs;

static void find_overlays(Display *d, Window w)
{
    XClassHint ch;
    if (XGetClassHint(d, w, &ch)) {
        int hit = ch.res_class && !strcmp(ch.res_class, "SmearCursorX11");
        if (ch.res_name) XFree(ch.res_name);
        if (ch.res_class) XFree(ch.res_class);
        if (hit && novs < MAX_OV) { ovs[novs++] = w; return; }
    }
    Window root, parent, *kids = NULL;
    unsigned int n = 0;
    if (!XQueryTree(d, w, &root, &parent, &kids, &n)) return;
    for (unsigned int i = 0; i < n; i++) find_overlays(d, kids[i]);
    if (kids) XFree(kids);
}

/* Distinct colours, counted coarsely.  A frame holding only the
 * backdrop has one or two; every layer that drew adds its own, so
 * this is the cheapest signal that a layer went missing. */
static int count_colours(XImage *im, long *opaque)
{
    unsigned char seen[4096];
    memset(seen, 0, sizeof seen);
    int n = 0;
    *opaque = 0;
    for (int y = 0; y < im->height; y++)
        for (int x = 0; x < im->width; x++) {
            unsigned long p = XGetPixel(im, x, y);
            if (im->depth == 32 && !((p >> 24) & 0xff)) continue;
            (*opaque)++;
            int k = (int)(((p >> 20) & 0xf) << 8 | ((p >> 12) & 0xf) << 4
                          | ((p >> 4) & 0xf));
            if (!seen[k]) { seen[k] = 1; n++; }
        }
    return n;
}

static int write_ppm(const char *path, XImage *im)
{
    FILE *f = fopen(path, "wb");
    if (!f) return 0;
    fprintf(f, "P6\n%d %d\n255\n", im->width, im->height);
    for (int y = 0; y < im->height; y++)
        for (int x = 0; x < im->width; x++) {
            unsigned long p = XGetPixel(im, x, y);
            unsigned char rgb[3] = { (p >> 16) & 0xff, (p >> 8) & 0xff,
                                     p & 0xff };
            /* Unpainted pixels are transparent, and a viewer showing
             * them as black hides nothing: the text underneath is
             * black too.  Grey says "the overlay let this through". */
            if (im->depth == 32 && !((p >> 24) & 0xff))
                rgb[0] = rgb[1] = rgb[2] = 0x30;
            fwrite(rgb, 1, 3, f);
        }
    fclose(f);
    return 1;
}

int main(int argc, char **argv)
{
    const char *dir = argc > 1 ? argv[1] : ".";
    double hz   = argc > 2 ? atof(argv[2]) : 20.0;
    double secs = argc > 3 ? atof(argv[3]) : 5.0;
    int rx = argc > 4 ? atoi(argv[4]) : 0, ry = argc > 5 ? atoi(argv[5]) : 0;
    int rw = argc > 6 ? atoi(argv[6]) : 0, rh = argc > 7 ? atoi(argv[7]) : 0;
    XSetErrorHandler(ignore_x_error);
    Display *d = XOpenDisplay(NULL);
    if (!d) { fprintf(stderr, "no display\n"); return 1; }

    double t0 = mono(), last_seek = -1e9;
    int shot = 0;
    while (mono() - t0 < secs) {
        double now = mono();
        if (now - last_seek > 0.5) {
            novs = 0;
            find_overlays(d, DefaultRootWindow(d));
            last_seek = now;
        }
        for (int i = 0; i < novs; i++) {
            XWindowAttributes a;
            if (!XGetWindowAttributes(d, ovs[i], &a)) continue;
            if (a.map_state != IsViewable) continue;
            int cx = rx, cy = ry, cw = rw ? rw : a.width, ch = rh ? rh : a.height;
            if (cx + cw > a.width)  cw = a.width - cx;
            if (cy + ch > a.height) ch = a.height - cy;
            if (cw < 1 || ch < 1) continue;
            XImage *im = XGetImage(d, ovs[i], cx, cy, cw, ch,
                                   AllPlanes, ZPixmap);
            if (!im) continue;
            long opaque = 0;
            int colours = count_colours(im, &opaque);
            if (opaque > 0) {
                char path[512];
                snprintf(path, sizeof path, "%s/shot-%03d.ppm", dir, shot);
                if (write_ppm(path, im))
                    printf("%s %.2f %d %d %d %ld %d\n", path, now - t0,
                           im->width, im->height, im->depth, opaque, colours);
                fflush(stdout);
                shot++;
            }
            XDestroyImage(im);
        }
        struct timespec ts = { 0, (long)(1e9 / hz) };
        nanosleep(&ts, NULL);
    }
    XCloseDisplay(d);
    return 0;
}
