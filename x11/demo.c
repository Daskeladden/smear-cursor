/* demo -- a cursor trail composited over Emacs's own text.
 *
 * The standalone demo.  The compositing lives in smear-cursor-x11-core.c,
 * which the Emacs module uses too; what is here is the part Emacs
 * already has in Lisp: a spring, and something to chase.
 *
 *   make demo && ./demo --demo   scripted jumps; your mouse is untouched
 *              ./demo       follows the mouse pointer
 *
 * Ctrl-C to quit.  Run it and the first lines say whether this display
 * can do it at all.
 */
#include "smear-cursor-x11-core.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static double opt_stiffness   = 0.28;
static double opt_tail_factor = 0.42;
static double opt_alpha       = 0.72;
static double opt_tail_alpha  = 0.10;
static double opt_r = 0x9a / 255.0, opt_g = 0xb8 / 255.0, opt_b = 0xe8 / 255.0;
static int    opt_cw = 9, opt_ch = 20;
static double opt_settle = 0.6;
static int    opt_demo = 0;
static double opt_dwell = 800.0;

static volatile sig_atomic_t stopping = 0;
static void on_signal(int s) { (void)s; stopping = 1; }

typedef struct { double x, y; } Pt;

static double now_ms(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1e3 + t.tv_nsec / 1e6;
}

static Window find_emacs(Display *d, Window w, int depth)
{
    XClassHint ch;
    Window root, parent, *kids = NULL, found = 0;
    unsigned int n = 0;
    if (XGetClassHint(d, w, &ch)) {
        int hit = ch.res_class && strcasecmp(ch.res_class, "Emacs") == 0;
        if (ch.res_name)  XFree(ch.res_name);
        if (ch.res_class) XFree(ch.res_class);
        if (hit) return w;
    }
    if (depth > 4) return 0;
    if (!XQueryTree(d, w, &root, &parent, &kids, &n)) return 0;
    for (unsigned i = 0; i < n && !found; i++)
        found = find_emacs(d, kids[i], depth + 1);
    if (kids) XFree(kids);
    return found;
}

/* Four corners move toward four targets on separate springs.  Stiffness
 * decreases with distance rank: nearest is stiffest, farthest is weakest.
 * This stretches the rectangle along the direction of movement. */
typedef struct { Pt p[4]; Pt target; int flying; } Smear;

static void target_corners(Pt t, Pt out[4])
{
    out[0] = (Pt){ t.x,          t.y          };
    out[1] = (Pt){ t.x + opt_cw, t.y          };
    out[2] = (Pt){ t.x + opt_cw, t.y + opt_ch };
    out[3] = (Pt){ t.x,          t.y + opt_ch };
}

static void smear_step(Smear *s, double dt)
{
    Pt tc[4];
    double dist[4];
    int order[4] = { 0, 1, 2, 3 };
    target_corners(s->target, tc);
    for (int i = 0; i < 4; i++)
        dist[i] = hypot(tc[i].x - s->p[i].x, tc[i].y - s->p[i].y);
    for (int i = 0; i < 4; i++)
        for (int j = i + 1; j < 4; j++)
            if (dist[order[j]] < dist[order[i]]) {
                int t = order[i]; order[i] = order[j]; order[j] = t;
            }
    int settled = 1;
    for (int rank = 0; rank < 4; rank++) {
        int i = order[rank];
        double k = opt_stiffness * (1.0 - opt_tail_factor * (rank / 3.0));
        double a = 1.0 - pow(1.0 - k, dt);
        s->p[i].x += (tc[i].x - s->p[i].x) * a;
        s->p[i].y += (tc[i].y - s->p[i].y) * a;
        if (dist[i] > opt_settle) settled = 0;
    }
    if (settled) {
        for (int i = 0; i < 4; i++) s->p[i] = tc[i];
        s->flying = 0;
    }
}

static void usage(void)
{
    puts("demo -- cursor trail composited over Emacs's text\n"
         "  --alpha F --tail-alpha F   opacity at head / far end\n"
         "  --stiffness F --tail F     spring: pull, and tail slack\n"
         "  --color RRGGBB             trail colour\n"
         "  --cell WxH                 size of the rect being chased\n"
         "  --demo --dwell MS          scripted jumps, and the pause between\n");
}

int main(int argc, char **argv)
{
    for (int i = 1; i < argc; i++) {
        const char *a = argv[i];
        const char *v = (i + 1 < argc) ? argv[i + 1] : NULL;
        if (!strcmp(a, "--help")) { usage(); return 0; }
        else if (!strcmp(a, "--alpha") && v)      opt_alpha = atof(argv[++i]);
        else if (!strcmp(a, "--tail-alpha") && v) opt_tail_alpha = atof(argv[++i]);
        else if (!strcmp(a, "--stiffness") && v)  opt_stiffness = atof(argv[++i]);
        else if (!strcmp(a, "--tail") && v)       opt_tail_factor = atof(argv[++i]);
        else if (!strcmp(a, "--color") && v) {
            unsigned c = (unsigned)strtoul(argv[++i], NULL, 16);
            opt_r = ((c >> 16) & 0xff) / 255.0;
            opt_g = ((c >> 8) & 0xff) / 255.0;
            opt_b = (c & 0xff) / 255.0;
        }
        else if (!strcmp(a, "--cell") && v)
            sscanf(argv[++i], "%dx%d", &opt_cw, &opt_ch);
        else if (!strcmp(a, "--demo"))       opt_demo = 1;
        else if (!strcmp(a, "--dwell") && v) opt_dwell = atof(argv[++i]);
        else { fprintf(stderr, "unknown option %s\n", a); usage(); return 1; }
    }

    Display *d = XOpenDisplay(NULL);
    if (!d) { fprintf(stderr, "cannot open display\n"); return 1; }
    Window em = find_emacs(d, DefaultRootWindow(d), 0);
    if (!em) { fprintf(stderr, "no Emacs window on this display\n"); return 1; }
    Window rr; int gx, gy; unsigned ew, eh, bw, gd;
    XGetGeometry(d, em, &rr, &gx, &gy, &ew, &eh, &bw, &gd);

    SmearStage *stage = smear_stage_open(NULL, em);
    if (!stage) { fprintf(stderr, "cannot open a second connection\n"); return 1; }
    printf("display: %s\n", smear_stage_describe(stage));
    const char *why = smear_stage_trouble(stage);
    if (why) { fprintf(stderr, "cannot composite here: %s\n", why); return 1; }
    printf("emacs 0x%lx  %ux%u\n", em, ew, eh);
    printf("%s", opt_demo
           ? "scripted jumps -- your mouse is not touched; Ctrl-C to stop\n"
           : "move the mouse over the frame; Ctrl-C to stop\n");

    /* the moves worth looking at */
    Pt script[8];
    int nscript = 0;
    {
        double L = 90, R = ew * 0.62, T = 70, B = eh * 0.80;
        script[nscript++] = (Pt){ L,        T + 200 };
        script[nscript++] = (Pt){ L + 240,  T + 200 };
        script[nscript++] = (Pt){ L + 240,  T + 40  };
        script[nscript++] = (Pt){ R,        B       };
        script[nscript++] = (Pt){ L,        T       };
        script[nscript++] = (Pt){ R * 0.75, T + 320 };
        script[nscript++] = (Pt){ L + 60,   B * 0.7 };
        script[nscript++] = (Pt){ L + 60,   B * 0.7 - 19 * 12 };
    }
    int step = 0; double step_at = 0;

    signal(SIGINT, on_signal);
    signal(SIGTERM, on_signal);

    Smear s;
    memset(&s, 0, sizeof s);
    int have_prev = 0, checked = 0;
    double last = now_ms(), frames = 0, cost = 0;

    while (!stopping) {
        int cx, cy, inside;
        if (opt_demo) {
            double t0 = now_ms();
            if (step_at == 0) step_at = t0;
            if (!s.flying && t0 - step_at > opt_dwell) {
                step = (step + 1) % nscript;
                step_at = t0;
            }
            cx = (int)script[step].x; cy = (int)script[step].y; inside = 1;
        } else {
            Window r, c; int rx, ry; unsigned mask;
            XQueryPointer(d, em, &r, &c, &rx, &ry, &cx, &cy, &mask);
            inside = cx >= 0 && cy >= 0 && cx < (int)ew && cy < (int)eh;
        }

        double t = now_ms();
        double dt = (t - last) * 60.0 / 1000.0;
        if (dt > 3.0) dt = 3.0;
        last = t;

        if (inside && have_prev) {
            if (hypot(cx - s.target.x, cy - s.target.y) > 1.0) {
                s.target = (Pt){ cx, cy };
                if (!s.flying) {
                    s.flying = 1;
                    smear_stage_begin(stage);
                    checked = 0;
                }
            }
        } else if (inside && !have_prev) {
            s.target = (Pt){ cx, cy };
            target_corners(s.target, s.p);
            have_prev = 1;
        }

        if (s.flying) {
            double c0 = now_ms();
            smear_step(&s, dt);
            Pt tc[4];
            target_corners(s.target, tc);
            SmearPaint p = {
                .head = { (tc[0].x + tc[2].x) / 2.0,
                          (tc[0].y + tc[2].y) / 2.0 },
                .r = opt_r, .g = opt_g, .b = opt_b,
                .alpha = opt_alpha, .tail_alpha = opt_tail_alpha,
            };
            for (int i = 0; i < 4; i++) {
                p.corners[2 * i]     = s.p[i].x;
                p.corners[2 * i + 1] = s.p[i].y;
            }
            smear_stage_draw(stage, &p);
            cost += now_ms() - c0;
            frames++;
            if (!checked) {          /* standing on text, or on a box? */
                checked = 1;
                if (smear_stage_background_has_detail(stage) == 0)
                    fprintf(stderr, "!! the trail is standing on a blank "
                                    "rectangle, not on your text\n");
            }
            if (!s.flying) smear_stage_end(stage);
        }
        usleep(16000);
    }

    smear_stage_end(stage);
    if (frames > 0)
        printf("\n%.0f frames, %.2f ms each on the wire\n", frames,
               cost / frames);
    smear_stage_close(stage);
    XCloseDisplay(d);
    return 0;
}
