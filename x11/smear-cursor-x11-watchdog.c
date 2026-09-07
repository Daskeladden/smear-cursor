/* Watch, continuously, without being noticed.
 *
 *   smear-cursor-x11-watchdog [seconds-between-samples]
 *
 * The trouble with asking what is wrong while it is wrong is that
 * asking changes it: switching to a terminal to run something is a
 * focus change and a restack, and those are exactly what the fault
 * turns on: "if I alt-TAB to a different window it immediately
 * resumes".  So this runs from before the fault until after it, on one
 * connection opened once, and writes a line only when something
 * changes.  Nobody has to do anything at the moment it matters.
 *
 * Each line: the time, who has the keyboard, where the overlay sits in
 * the stacking order, what its bounding shape shows, and what Emacs's
 * threads are waiting on.  Between them those say whether Emacs is
 * blocked, whether the overlay has been buried, and whether input has
 * gone somewhere else.  Those are three different faults that look
 * identical from the outside.
 */
#define _POSIX_C_SOURCE 200809L
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Windows come and go between one request and the next.  A popup
 * closing between `XQueryTree' and asking its name is enough, and
 * the default handler treats that as fatal.  A watchdog that dies of
 * the thing it is watching is no watchdog. */
static int ignore_x_errors(Display *d, XErrorEvent *e)
{
    (void)d; (void)e;
    return 0;
}

static char *win_name(Display *d, Window w, char *out, size_t n)
{
    char *nm = NULL;
    XClassHint ch;
    out[0] = 0;
    if (w == None)        { snprintf(out, n, "None"); return out; }
    if (w == PointerRoot) { snprintf(out, n, "PointerRoot"); return out; }
    if (XFetchName(d, w, &nm) && nm) {
        snprintf(out, n, "%.40s", nm); XFree(nm); return out;
    }
    if (XGetClassHint(d, w, &ch)) {
        snprintf(out, n, "%.40s", ch.res_class ? ch.res_class : "?");
        if (ch.res_name) XFree(ch.res_name);
        if (ch.res_class) XFree(ch.res_class);
        return out;
    }
    snprintf(out, n, "0x%lx", w);
    return out;
}

/* Emacs's threads, as the kernel sees them.  No X, no cooperation from
 * Emacs: this works when Emacs answers nothing at all. */
static void threads_of(const char *pid, char *out, size_t n)
{
    char path[256];
    snprintf(path, sizeof path, "/proc/%s/task", pid);
    DIR *dir = opendir(path);
    out[0] = 0;
    if (!dir) { snprintf(out, n, "(restarted)"); return; }
    struct dirent *e;
    size_t used = 0;
    while ((e = readdir(dir))) {
        if (e->d_name[0] == '.') continue;
        char wp[320], comm[320], buf[128] = "", cm[64] = "";
        snprintf(wp, sizeof wp, "/proc/%s/task/%s/wchan", pid, e->d_name);
        snprintf(comm, sizeof comm, "/proc/%s/task/%s/comm", pid, e->d_name);
        FILE *f = fopen(wp, "r");
        if (f) { if (!fgets(buf, sizeof buf, f)) buf[0] = 0; fclose(f); }
        f = fopen(comm, "r");
        if (f) { if (fgets(cm, sizeof cm, f)) cm[strcspn(cm, "\n")] = 0; fclose(f); }
        if (strncmp(cm, "emacs", 5) != 0) continue;   /* Emacs's own only */
        int w = snprintf(out + used, n - used, "%s%.14s",
                         used ? "," : "", buf[0] ? buf : "-");
        if (w > 0 && (size_t)w < n - used) used += (size_t)w;
    }
    closedir(dir);
    if (!out[0]) snprintf(out, n, "(none)");
}

int main(int argc, char **argv)
{
    double every = argc > 1 ? atof(argv[1]) : 0.5;
    Display *d = XOpenDisplay(NULL);
    if (!d) { fprintf(stderr, "no display\n"); return 1; }
    XSetErrorHandler(ignore_x_errors);
    Window root = DefaultRootWindow(d);

    /* Emacs's pid.  Found once here; the sampling loop looks it up
     * again if Emacs restarts under the watchdog. */
    static char pid[32] = "";
    {
        FILE *p = popen("pgrep -u \"$(id -u)\" -f 'emacs' | head -1", "r");
        if (p) { if (fgets(pid, sizeof pid, p)) pid[strcspn(pid, "\n")] = 0;
                 pclose(p); }
    }
    printf("# watching emacs pid %s, sampling every %.2f s\n",
           pid[0] ? pid : "?", every);
    printf("# time  focus | overlay position and shape | emacs threads\n");
    fflush(stdout);

    char last[1024] = "";
    for (;;) {
        Window focus = None; int rev = 0;
        XGetInputFocus(d, &focus, &rev);
        Window r, p2, *kids = NULL;
        unsigned int n = 0;
        XQueryTree(d, root, &r, &p2, &kids, &n);

        int ovi = -1, emi = -1;
        Window ov = 0;
        char fname[64];
        win_name(d, focus, fname, sizeof fname);
        for (unsigned int i = 0; i < n; i++) {
            XClassHint ch;
            if (!XGetClassHint(d, kids[i], &ch)) continue;
            int is_ov = ch.res_class && !strcmp(ch.res_class, "SmearCursorX11");
            int is_em = ch.res_class && !strcmp(ch.res_class, "Emacs");
            if (ch.res_name) XFree(ch.res_name);
            if (ch.res_class) XFree(ch.res_class);
            if (is_ov) { ovi = (int)i; ov = kids[i]; }
            if (is_em) emi = (int)i;
        }
        if (kids) XFree(kids);

        int mapped = -1, rects = -1, area = 0;
        if (ov) {
            XWindowAttributes a;
            if (XGetWindowAttributes(d, ov, &a))
                mapped = (a.map_state == IsViewable);
            int nr = 0, ord = 0;
            XRectangle *rs = XShapeGetRectangles(d, ov, ShapeBounding, &nr, &ord);
            rects = nr;
            for (int i = 0; i < nr; i++) area += rs[i].width * rs[i].height;
            if (rs) XFree(rs);
        }

        char thr[256];
        threads_of(pid, thr, sizeof thr);
        /* Emacs restarts; the watchdog outlives it and should follow
         * rather than report on a pid that is not there any more. */
        if (!strcmp(thr, "(restarted)")) {
            FILE *pp = popen("pgrep -u \"$(id -u)\" -f 'emacs' | head -1", "r");
            if (pp) {
                if (fgets(pid, sizeof pid, pp)) pid[strcspn(pid, "\n")] = 0;
                pclose(pp);
            }
            threads_of(pid, thr, sizeof thr);
        }

        char line[1024];
        snprintf(line, sizeof line,
                 "focus=%s | overlay %s at %d, emacs at %d, mapped=%d "
                 "shape=%d rect(s) %d px | threads %s",
                 fname, ovi < 0 ? "absent" : (ovi > emi ? "ABOVE" : "BELOW"),
                 ovi, emi, mapped, rects, area, thr);

        if (strcmp(line, last) != 0) {
            struct timespec ts;
            clock_gettime(CLOCK_REALTIME, &ts);
            time_t sec = ts.tv_sec;
            struct tm tm;
            localtime_r(&sec, &tm);
            printf("%02d:%02d:%02d.%03ld  %s\n", tm.tm_hour, tm.tm_min,
                   tm.tm_sec, ts.tv_nsec / 1000000, line);
            fflush(stdout);
            snprintf(last, sizeof last, "%s", line);
        }
        struct timespec nap = { (time_t)every,
                                (long)((every - (long)every) * 1e9) };
        nanosleep(&nap, NULL);
    }
}
