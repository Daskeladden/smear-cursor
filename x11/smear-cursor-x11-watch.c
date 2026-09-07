/* Watch the overlay from outside: is it up, and what is it showing?
 *
 * Everything else here measures what we asked the server for.  This
 * asks the server what it has: whether the overlay window is mapped,
 * where it is, and what its bounding shape lets through.  These three
 * checks can identify mapping, position or shape problems, but cannot
 * establish whether the rendered pixels are visible.
 *
 *   smear-cursor-x11-watch HZ SECONDS
 *
 * One line per sample: ELAPSED WALL-CLOCK WINDOW X Y W H SHAPE-RECTS BOX
 */
#define _POSIX_C_SOURCE 200809L
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>
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

/* Every window our stage names itself with, not the first: a machine
 * with more than one Emacs on the display has more than one, and the
 * first one found is as likely as not to be some other session's
 * sitting unmapped. */
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

int main(int argc, char **argv)
{
    double hz = argc > 1 ? atof(argv[1]) : 50.0;
    double secs = argc > 2 ? atof(argv[2]) : 10.0;
    Display *d = XOpenDisplay(NULL);
    if (!d) { fprintf(stderr, "no display\n"); return 1; }

    double t0 = mono(), last_seek = -1e9;
    while (mono() - t0 < secs) {
        double now = mono();
        /* The stage is made when the first smear runs, so keep looking
           for new ones as well as watching the ones already found. */
        if (now - last_seek > 0.5) {
            int was = novs;
            novs = 0;
            find_overlays(d, DefaultRootWindow(d));
            last_seek = now;
            if (novs != was)
                for (int i = 0; i < novs; i++)
                    printf("# overlay %d = 0x%lx\n", i, ovs[i]);
        }
        for (int i = 0; i < novs; i++) {
            Window ov = ovs[i];
            XWindowAttributes a;
            if (!XGetWindowAttributes(d, ov, &a)) continue;
            int nr = 0, ordering = 0;
            XRectangle *r = XShapeGetRectangles(d, ov, ShapeBounding,
                                                &nr, &ordering);
            int sx = 0, sy = 0, sw = 0, sh = 0;
            if (r && nr > 0) {
                int x0 = 1 << 30, y0 = 1 << 30, x1 = -(1 << 30), y1 = -(1 << 30);
                for (int i = 0; i < nr; i++) {
                    if (r[i].x < x0) x0 = r[i].x;
                    if (r[i].y < y0) y0 = r[i].y;
                    if (r[i].x + r[i].width  > x1) x1 = r[i].x + r[i].width;
                    if (r[i].y + r[i].height > y1) y1 = r[i].y + r[i].height;
                }
                sx = x0; sy = y0; sw = x1 - x0; sh = y1 - y0;
            }
            if (r) XFree(r);
            if (a.map_state != IsViewable) continue;   /* only what is shown */
            /* Wall clock as well as elapsed: `smear-cursor-trace'
               stamps its lines with `float-time', and the two files
               are meant to be read side by side. */
            struct timespec wall;
            clock_gettime(CLOCK_REALTIME, &wall);
            printf("%.3f %.3f ov%d UP %d %d %d %d  shape %d rects %dx%d+%d+%d\n",
                   now - t0,
                   (double)wall.tv_sec + wall.tv_nsec / 1e9,
                   i, a.x, a.y, a.width, a.height, nr, sw, sh, sx, sy);
            fflush(stdout);
        }
        struct timespec ts = { 0, (long)(1e9 / hz) };
        nanosleep(&ts, NULL);
    }
    XCloseDisplay(d);
    return 0;
}
