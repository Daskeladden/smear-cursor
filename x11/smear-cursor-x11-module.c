/* smear-cursor-x11-module -- the compositing stage, callable from Emacs.
 *
 * Emacs hands over its frame's X window id and display string; this
 * lays the trail over that window's own live pixels.  All the geometry
 * and the spring stay in Lisp, where they already are.
 *
 *   (smear-cursor-x11--open WINDOW-ID DISPLAY)  -> stage or nil
 *   (smear-cursor-x11--trouble STAGE)           -> nil, or why it cannot draw
 *   (smear-cursor-x11--describe STAGE)          -> what kind of X this is
 *   (smear-cursor-x11--begin STAGE)             -> t
 *   (smear-cursor-x11--frame-begin STAGE X Y W H)
 *   (smear-cursor-x11--draw STAGE LAYER)        ; as many as the style has
 *   (smear-cursor-x11--frame-end STAGE)
 *   (smear-cursor-x11--end STAGE)
 *
 * And the same trail handed over whole, for the player thread to
 * paint without Emacs.  See smear-cursor-x11-play.h for why:
 *
 *   (smear-cursor-x11--play-start STAGE)   -> t, or why it cannot
 *   (smear-cursor-x11--play STAGE TRACK LAYERS FRAMES FPS)
 *   (smear-cursor-x11--play-position STAGE TRACK) -> frame index, or nil
 *   (smear-cursor-x11--play-stop STAGE)
 *   (smear-cursor-x11--play-stats STAGE)   -> [FRAMES PAINT MAX AT SUM GAPS]
 *   (smear-cursor-x11--close STAGE)
 *   (smear-cursor-x11--background-has-detail-p STAGE) -> t / nil / :unknown
 */
#include "smear-cursor-x11-core.h"
#include "smear-cursor-x11-gl.h"
#include "smear-cursor-x11-play.h"
#include <emacs-module.h>
#include <stdlib.h>
#include <string.h>

int plugin_is_GPL_compatible;

static emacs_value Qnil, Qt;

/* The player thread holds the stage, so it goes first.  Closing the
 * stage under a running thread leaves it painting into freed memory. */
static void stage_finalizer(void *ptr)
{
    smear_play_shutdown(ptr);
    smear_stage_close(ptr);
}

static char *copy_string(emacs_env *env, emacs_value v)
{
    ptrdiff_t n = 0;
    if (!env->copy_string_contents(env, v, NULL, &n)) return NULL;
    char *s = malloc(n);
    if (!s) return NULL;
    if (!env->copy_string_contents(env, v, s, &n)) { free(s); return NULL; }
    return s;
}

static SmearStage *stage_arg(emacs_env *env, emacs_value v)
{
    return env->get_user_ptr(env, v);
}

static emacs_value Fopen(emacs_env *env, ptrdiff_t n, emacs_value *args,
                         void *d)
{
    (void)n; (void)d;
    char *wid = copy_string(env, args[0]);
    char *disp = env->is_not_nil(env, args[1]) ? copy_string(env, args[1])
                                               : NULL;
    if (!wid) { free(disp); return Qnil; }
    unsigned long window = strtoul(wid, NULL, 0);
    free(wid);
    SmearStage *st = smear_stage_open(disp, window);
    free(disp);
    if (!st) return Qnil;
    return env->make_user_ptr(env, stage_finalizer, st);
}

static emacs_value Ftrouble(emacs_env *env, ptrdiff_t n, emacs_value *args,
                            void *d)
{
    (void)n; (void)d;
    const char *why = smear_stage_trouble(stage_arg(env, args[0]));
    return why ? env->make_string(env, why, (ptrdiff_t)strlen(why)) : Qnil;
}

static emacs_value Fdescribe(emacs_env *env, ptrdiff_t n, emacs_value *args,
                             void *d)
{
    (void)n; (void)d;
    const char *s = smear_stage_describe(stage_arg(env, args[0]));
    return env->make_string(env, s, (ptrdiff_t)strlen(s));
}

static emacs_value Fbegin(emacs_env *env, ptrdiff_t n, emacs_value *args,
                          void *d)
{
    (void)n; (void)d;
    return smear_stage_begin(stage_arg(env, args[0])) ? Qt : Qnil;
}

/* Any number, integer or float.
 *
 * `extract_float' signals on an integer, and Lisp hands out whichever
 * it happens to have: a colour is carried as [154 184 232] and a
 * corner that lands exactly on a pixel comes back as 0 rather than
 * 0.0.  Refusing those would make the caller cast defensively at every
 * call site for no reason. */
static double num(emacs_env *env, emacs_value v)
{
    emacs_value type = env->type_of(env, v);
    if (env->eq(env, type, env->intern(env, "integer")))
        return (double)env->extract_integer(env, v);
    return env->extract_float(env, v);
}

static int floats_from(emacs_env *env, emacs_value v, double *out, int want)
{
    if (env->vec_size(env, v) != want) return 0;
    for (int i = 0; i < want; i++)
        out[i] = num(env, env->vec_get(env, v, i));
    return 1;
}

static emacs_value Fframe_begin(emacs_env *env, ptrdiff_t n, emacs_value *args,
                                void *d)
{
    (void)n; (void)d;
    smear_stage_frame_begin(stage_arg(env, args[0]),
                            (int)num(env, args[1]), (int)num(env, args[2]),
                            (int)num(env, args[3]), (int)num(env, args[4]));
    return Qt;
}

static emacs_value Fframe_end(emacs_env *env, ptrdiff_t n, emacs_value *args,
                              void *d)
{
    (void)n; (void)d;
    smear_stage_frame_end(stage_arg(env, args[0]));
    return Qt;
}

static emacs_value Fcan_blur(emacs_env *env, ptrdiff_t n, emacs_value *args,
                             void *d)
{
    (void)n; (void)d;
    return smear_stage_can_blur(stage_arg(env, args[0])) ? Qt : Qnil;
}

/* Read one layer vector into P.  Zero if it is not one.
 *
 * A flight reuses this and then overwrites CORNERS and HEAD per frame,
 * so Lisp builds a layer the one way whether it is drawing a frame
 * itself or handing a whole flight over. */
static int parse_layer(emacs_env *env, emacs_value v, SmearPaint *p)
{
    if (env->vec_size(env, v) != 8) return 0;
    memset(p, 0, sizeof *p);
    double col[3];
    p->kind = (int)num(env, env->vec_get(env, v, 0));
    if (!floats_from(env, env->vec_get(env, v, 1), p->corners, 8)) return 0;
    if (!floats_from(env, env->vec_get(env, v, 2), p->head, 2)) return 0;
    if (!floats_from(env, env->vec_get(env, v, 3), col, 3)) return 0;
    p->r = col[0] / 255.0; p->g = col[1] / 255.0; p->b = col[2] / 255.0;
    p->grow   = num(env, env->vec_get(env, v, 4));
    p->blur   = num(env, env->vec_get(env, v, 5));
    p->radius = num(env, env->vec_get(env, v, 6));

    emacs_value stops = env->vec_get(env, v, 7);
    ptrdiff_t ns = env->vec_size(env, stops) / 2;
    if (ns > SMEAR_MAX_STOPS) ns = SMEAR_MAX_STOPS;
    p->nstops = (int)ns;
    for (int i = 0; i < p->nstops; i++) {
        p->stop_at[i]    = num(env, env->vec_get(env, stops, 2 * i));
        p->stop_alpha[i] = num(env, env->vec_get(env, stops, 2 * i + 1));
    }
    return 1;
}

/* One layer, as a vector:
 *
 *   [KIND CORNERS HEAD COLOR GROW BLUR RADIUS STOPS]
 *
 * KIND 0 is a quad, 1 a round glow at HEAD.  CORNERS is eight numbers
 * TL TR BR BL in the frame's own pixels, HEAD two, COLOR three of
 * 0-255.  STOPS is [AT ALPHA AT ALPHA ...] along head to tail. */
static emacs_value Fdraw(emacs_env *env, ptrdiff_t n, emacs_value *args,
                         void *d)
{
    (void)n; (void)d;
    SmearStage *st = stage_arg(env, args[0]);
    emacs_value v = args[1];
    if (env->vec_size(env, v) != 8) return Qnil;

    SmearPaint p;
    if (!parse_layer(env, v, &p)) return Qnil;
    smear_stage_draw(st, &p);
    return Qt;
}

/* ---- handing a whole flight to the player thread ---------------- */

static emacs_value Fplay_start(emacs_env *env, ptrdiff_t n, emacs_value *args,
                               void *d)
{
    (void)n; (void)d;
    char err[256] = {0};
    if (smear_play_start(stage_arg(env, args[0]), err, sizeof err) == 0)
        return Qt;
    return env->make_string(env, err, (ptrdiff_t)strlen(err));
}

/* FRAMES is one flat vector of numbers rather than a vector of vectors:
 * per frame, BX BY BW BH, then for each layer eight corners, two head
 * coordinates and one alpha scale.  A few dozen frames of a few layers is a couple of
 * thousand numbers, and reading them out of one vector costs the
 * module one call each instead of one call each plus a vector walk. */
static int frame_stride(int nlayers) { return 4 + nlayers * 11; }

static void read_frame(emacs_env *env, emacs_value v, ptrdiff_t at,
                       int nlayers, SmearFrame *f)
{
    f->bx = (int)num(env, env->vec_get(env, v, at));
    f->by = (int)num(env, env->vec_get(env, v, at + 1));
    f->bw = (int)num(env, env->vec_get(env, v, at + 2));
    f->bh = (int)num(env, env->vec_get(env, v, at + 3));
    ptrdiff_t k = at + 4;
    for (int L = 0; L < nlayers; L++) {
        for (int c = 0; c < 8; c++)
            f->corners[L][c] = num(env, env->vec_get(env, v, k++));
        f->head[L][0] = num(env, env->vec_get(env, v, k++));
        f->head[L][1] = num(env, env->vec_get(env, v, k++));
        f->alpha[L]   = num(env, env->vec_get(env, v, k++));
    }
}

/* How many frames a flight plays over its photograph.
 *
 * A number says how many; `t' says every one of them, which is what
 * the argument meant before there was a choice.  Held to the end, the
 * photograph keeps deleted text on the screen for the length of the
 * effect and the edit looks like it lagged. */
static int frozen_frames(emacs_env *env, ptrdiff_t n, emacs_value *args)
{
    if (n <= 5 || !env->is_not_nil(env, args[5])) return 0;
    if (env->eq(env, env->type_of(env, args[5]), env->intern(env, "integer")))
        return (int)env->extract_integer(env, args[5]);
    return SMEAR_MAX_FRAMES;
}

static emacs_value Fplay(emacs_env *env, ptrdiff_t n, emacs_value *args,
                         void *d)
{
    (void)n; (void)d;
    SmearStage *st = stage_arg(env, args[0]);
    int track = (int)num(env, args[1]);
    emacs_value lv = args[2], fv = args[3];
    double fps = num(env, args[4]);

    ptrdiff_t nl = env->vec_size(env, lv);
    if (nl < 1 || nl > SMEAR_MAX_PLAY_LAYERS) return Qnil;
    SmearPaint layers[SMEAR_MAX_PLAY_LAYERS];
    for (ptrdiff_t i = 0; i < nl; i++)
        if (!parse_layer(env, env->vec_get(env, lv, i), &layers[i]))
            return Qnil;

    int stride = frame_stride((int)nl);
    ptrdiff_t total = env->vec_size(env, fv);
    int nf = (int)(total / stride);
    if (nf < 1) return Qnil;
    if (nf > SMEAR_MAX_FRAMES) nf = SMEAR_MAX_FRAMES;

    SmearFrame *frames = calloc((size_t)nf, sizeof *frames);
    if (!frames) return Qnil;
    for (int k = 0; k < nf; k++)
        read_frame(env, fv, (ptrdiff_t)k * stride, (int)nl, &frames[k]);

    /* An optional sixth argument, non-nil for a flight that plays over
     * a photograph taken before the text it marks was removed. */
    int frozen = frozen_frames(env, n, args);
    /* And a seventh, non-nil for a flight whose shapes do not move. */
    int still = (n > 6 && env->is_not_nil(env, args[6]));
    int ok = smear_play_flight(st, track, layers, (int)nl, frames, nf, fps,
                               frozen, still);
    free(frames);
    return ok ? Qt : Qnil;
}

static emacs_value Fplay_position(emacs_env *env, ptrdiff_t n,
                                  emacs_value *args, void *d)
{
    (void)n; (void)d;
    int k = smear_play_position(stage_arg(env, args[0]), (int)num(env, args[1]));
    return k < 0 ? Qnil : env->make_integer(env, k);
}

static emacs_value Fplay_stop(emacs_env *env, ptrdiff_t n, emacs_value *args,
                              void *d)
{
    (void)n; (void)d;
    smear_play_stop(stage_arg(env, args[0]));
    return Qt;
}

static emacs_value Fplay_stop_track(emacs_env *env, ptrdiff_t n,
                                    emacs_value *args, void *d)
{
    (void)n; (void)d;
    smear_play_stop_track(stage_arg(env, args[0]), (int)num(env, args[1]));
    return Qt;
}

static emacs_value Ffreeze(emacs_env *env, ptrdiff_t n, emacs_value *args,
                           void *d)
{
    (void)n; (void)d;
    SmearStage *st = stage_arg(env, args[0]);
    if (!st) return Qnil;
    int x = (int)env->extract_integer(env, args[1]);
    int y = (int)env->extract_integer(env, args[2]);
    int w = (int)env->extract_integer(env, args[3]);
    int h = (int)env->extract_integer(env, args[4]);
    return smear_stage_freeze(st, x, y, w, h) ? Qt : Qnil;
}

static emacs_value Fplay_stats(emacs_env *env, ptrdiff_t n, emacs_value *args,
                               void *d)
{
    (void)n; (void)d;
    SmearPlayStats s;
    if (!smear_play_stats(stage_arg(env, args[0]), &s)) return Qnil;
    emacs_value vector = env->intern(env, "vector");
    emacs_value *gv = calloc((size_t)(s.ngaps > 0 ? s.ngaps : 1), sizeof *gv);
    if (!gv) return Qnil;
    for (int i = 0; i < s.ngaps; i++) gv[i] = env->make_float(env, s.gaps[i]);
    emacs_value gaps = env->funcall(env, vector, s.ngaps, gv);
    free(gv);
    emacs_value v[8] = {
        env->make_integer(env, s.frames),
        env->make_float(env, s.paint_total),
        env->make_float(env, s.gap_max),
        env->make_integer(env, s.gap_max_at),
        env->make_float(env, s.gap_sum),
        gaps,
        env->make_float(env, s.drain),
        env->make_float(env, s.drain_age),
    };
    return env->funcall(env, vector, 8, v);
}

static emacs_value Fend(emacs_env *env, ptrdiff_t n, emacs_value *args,
                        void *d)
{
    (void)n; (void)d;
    smear_stage_end(stage_arg(env, args[0]));
    return Qt;
}

static emacs_value Fclose(emacs_env *env, ptrdiff_t n, emacs_value *args,
                          void *d)
{
    (void)n; (void)d;
    SmearStage *st = stage_arg(env, args[0]);
    if (st) {
        smear_play_shutdown(st);        /* the thread holds the stage */
        smear_stage_close(st);
        env->set_user_ptr(env, args[0], NULL);
        env->set_user_finalizer(env, args[0], NULL);
    }
    return Qt;
}

/* For tests: is the trail standing on text, or on a blank rectangle?
 * The check whose absence let a black-background bug ship. */
static emacs_value Fdetail(emacs_env *env, ptrdiff_t n, emacs_value *args,
                           void *d)
{
    (void)n; (void)d;
    int r = smear_stage_background_has_detail(stage_arg(env, args[0]));
    if (r < 0) return env->intern(env, ":unknown");
    return r ? Qt : Qnil;
}

/* [MEAN SD CHANGED] for a region of the overlay: what is actually on
 * screen there, and how much of it the trail altered. */
static emacs_value Fsample(emacs_env *env, ptrdiff_t n, emacs_value *args,
                           void *d)
{
    (void)n; (void)d;
    double mean = 0, sd = 0; long changed = 0;
    smear_stage_sample(stage_arg(env, args[0]),
                       (int)num(env, args[1]), (int)num(env, args[2]),
                       (int)num(env, args[3]), (int)num(env, args[4]),
                       &mean, &sd, &changed);
    emacs_value v[3] = { env->make_float(env, mean),
                         env->make_float(env, sd),
                         env->make_integer(env, changed) };
    emacs_value vec = env->funcall(env, env->intern(env, "vector"), 3, v);
    return vec;
}

/* Ask for a renderer by name; returns the one actually in use, so a
 * caller finds out that GL declined rather than assuming it took. */
static emacs_value Frenderer(emacs_env *env, ptrdiff_t n, emacs_value *args,
                             void *d)
{
    (void)n; (void)d;
    SmearStage *st = stage_arg(env, args[0]);
    char err[256] = "";
    int want = env->is_not_nil(env, args[1]) ? SMEAR_RENDERER_GL
                                             : SMEAR_RENDERER_RENDER;
    int got = smear_stage_set_renderer(st, want, err, sizeof err);
    if (got == SMEAR_RENDERER_GL) return env->intern(env, "gl");
    if (want == SMEAR_RENDERER_GL && err[0])
        return env->make_string(env, err, (ptrdiff_t)strlen(err));
    return env->intern(env, "render");
}

/* How many layers one flight may carry.  Lisp asks rather than assumes:
 * a module built before the limit was raised is still loaded in a running
 * Emacs after the package is updated, and it refuses a whole flight that
 * asks for more than it holds.  Asking lets Lisp send fewer instead of
 * having the effect silently not appear. */
static emacs_value Fcapacity(emacs_env *env, ptrdiff_t n, emacs_value *a,
                             void *p)
{
    (void)n; (void)a; (void)p;
    /* The smaller of what a flight's arrays hold and what the shader
     * was built for.  One number for both renderers, even though
     * RENDER has no such limit: told a bigger number than GL can
     * draw, an effect built to it loses its last layers silently,
     * which is a whole character missing from a figure and no error
     * anywhere. */
    int cap = SMEAR_MAX_PLAY_LAYERS;
    char err[256] = {0};
    smear_gl_init(err, sizeof err);
    int gl = smear_gl_max_layers();
    if (gl > 0 && gl < cap) cap = gl;
    return env->make_integer(env, cap);
}

static void defun(emacs_env *env, const char *name, int amin, int amax,
                  emacs_value (*fn)(emacs_env *, ptrdiff_t, emacs_value *,
                                    void *),
                  const char *doc)
{
    emacs_value f = env->make_function(env, amin, amax, fn, doc, NULL);
    emacs_value sym = env->intern(env, name);
    emacs_value fset = env->intern(env, "fset");
    emacs_value a[] = { sym, f };
    env->funcall(env, fset, 2, a);
}

int emacs_module_init(struct emacs_runtime *rt)
{
    /* Ask for the oldest interface this module uses, not the newest the
       header knows about.  The structs are append-only, so a module built
       against a newer emacs-module.h than the Emacs loading it still works
       as long as it asks for no more than it needs.  Everything here is in
       emacs_env_25.  Comparing against sizeof *env instead refuses any
       Emacs older than the header, which on a machine with two Emacs
       installations is a silent "Module initialization failed: 2". */
    if ((size_t)rt->size < sizeof *rt) return 1;
    emacs_env *env = rt->get_environment(rt);
    if ((size_t)env->size < sizeof (struct emacs_env_25)) return 2;

    Qnil = env->make_global_ref(env, env->intern(env, "nil"));
    Qt   = env->make_global_ref(env, env->intern(env, "t"));

    defun(env, "smear-cursor-x11--open", 2, 2, Fopen,
          "Take over X window WINDOW-ID on DISPLAY.  Returns a stage or nil.");
    defun(env, "smear-cursor-x11--trouble", 1, 1, Ftrouble,
          "Why STAGE cannot draw, or nil when it can.");
    defun(env, "smear-cursor-x11--describe", 1, 1, Fdescribe,
          "One line describing STAGE's display.");
    defun(env, "smear-cursor-x11--begin", 1, 1, Fbegin, "Put STAGE's overlay up.");
    defun(env, "smear-cursor-x11--frame-begin", 5, 5, Fframe_begin,
          "Lay STAGE's live text under the rect X Y W H the trail needs.");
    defun(env, "smear-cursor-x11--draw", 2, 2, Fdraw,
          "Draw one LAYER of a trail on STAGE.  See the module source.");
    defun(env, "smear-cursor-x11--frame-end", 1, 1, Fframe_end,
          "Put STAGE's finished frame on the wire.");
    defun(env, "smear-cursor-x11--can-blur-p", 1, 1, Fcan_blur,
          "Non-nil when this display can blur a layer server-side.");
    defun(env, "smear-cursor-x11--end", 1, 1, Fend, "Take STAGE's overlay down.");
    defun(env, "smear-cursor-x11--close", 1, 1, Fclose, "Release STAGE.");
    defun(env, "smear-cursor-x11--renderer", 2, 2, Frenderer,
          "Use the GL renderer on STAGE when GL is non-nil.\n"
          "Returns `gl', `render', or a string saying why GL declined.");
    defun(env, "smear-cursor-x11--sample", 5, 5, Fsample,
          "[MEAN SD CHANGED] for STAGE's overlay over X Y W H.");
    defun(env, "smear-cursor-x11--background-has-detail-p", 1, 1, Fdetail,
          "Non-nil when STAGE's last frame stood on text, not a blank box.");

    defun(env, "smear-cursor-x11--max-layers", 0, 0, Fcapacity,
          "How many layers this module can draw in one flight.");
    defun(env, "smear-cursor-x11--play-start", 1, 1, Fplay_start,
          "Bring STAGE's player thread up.  t, or a string saying why not.");
    defun(env, "smear-cursor-x11--play", 5, 7, Fplay,
          "Hand STAGE's TRACK a whole flight: LAYERS, FRAMES, FPS and,\n"
          "optionally, FROZEN -- non-nil to play over the photograph taken\n"
          "by `smear-cursor-x11--freeze' rather than over the live window --\n"
          "and STILL, non-nil when the shapes do not move and only the alpha\n"
          "does, so the flight is rendered and uploaded once.");
    defun(env, "smear-cursor-x11--play-position", 2, 2, Fplay_position,
          "Which frame of STAGE's TRACK is on screen, or nil.");
    defun(env, "smear-cursor-x11--play-stop-track", 2, 2, Fplay_stop_track,
          "Stop STAGE's TRACK; the overlay stays up for the others.");
    defun(env, "smear-cursor-x11--play-stop", 1, 1, Fplay_stop,
          "Stop STAGE's flight and take the overlay down.");
    defun(env, "smear-cursor-x11--freeze", 5, 5, Ffreeze,
          "Photograph X Y W H of STAGE's window now, for a frozen flight.");
    defun(env, "smear-cursor-x11--play-stats", 1, 1, Fplay_stats,
          "[FRAMES PAINT-TOTAL GAP-MAX GAP-MAX-AT GAP-SUM GAPS DRAIN DRAIN-AGE].");

    emacs_value provide = env->intern(env, "provide");
    emacs_value feat = env->intern(env, "smear-cursor-x11-module");
    env->funcall(env, provide, 1, &feat);
    return 0;
}
