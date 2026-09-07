/* See smear-cursor-x11-play.h. */
#define _POSIX_C_SOURCE 200809L
#include "smear-cursor-x11-play.h"

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* One flight with its own clock.  See SMEAR_MAX_TRACKS. */
typedef struct {
    SmearPaint  layers[SMEAR_MAX_PLAY_LAYERS];
    int         nlayers;
    SmearFrame  frames[SMEAR_MAX_FRAMES];
    int         nframes;
    double      t0;
    double      fps;         /* this flight's own rate, not the player's */
    int         active;
    int         frozen;      /* frames to play over a photograph of the text */
    int         still;       /* shapes do not move; only the alpha does */
    int         stale;       /* a stamp from a previous flight to discard */
} Track;

/* What one frame needs, copied out from under the lock.
 *
 * The player used to paint with the lock held, which made every
 * handoff wait for a frame: measured, 0.04 ms to hand a flight over
 * while nothing was playing and 5.60 ms while something was.  That
 * wait lands on Emacs's one thread, on every cursor movement, and it
 * is invisible to `smear-cursor-report', which measures this thread
 * and is asked after the fact. */
typedef struct {
    int bx, by, bw, bh;               /* the union, for shape and backdrop */
    struct {
        int        due;
        int        nlayers;
        int        frozen;
        int        still;
        int        stale;
        SmearPaint layers[SMEAR_MAX_PLAY_LAYERS];
        SmearFrame frame;
    } t[SMEAR_MAX_TRACKS];
} Snap;

typedef struct {
    pthread_t       tid;
    pthread_mutex_t mu;
    pthread_cond_t  cv;
    SmearStage     *st;
    int             running, quit;
    /* `up' is the overlay mapped; `showing' is something on it.  They
     * used to be one flag, and taking the overlay down between flights
     * cost round trips an effect on every keystroke could not
     * afford. */
    int             showing;

    /* the flights, under the lock */
    Track       track[SMEAR_MAX_TRACKS];
    int         up;          /* the overlay is mapped */
    int         pos;         /* frame last painted, -1 for none yet */

    SmearPlayStats stats, live;
    double         idle_at;               /* when the last flight ended */
    double         drained, drain_last;   /* the drain, sampled */
    double         last_paint;
    int            have_stats;
} Player;

/* How long Emacs's thread will wait for the player's lock.
 *
 * Never indefinitely, whatever the player is doing.  Every call below
 * except the shutdown runs on Emacs's thread, on a keystroke, a
 * cursor movement or a report, and this lock is the one place a fault
 * in the player can reach across and stop the editor.  A dropped
 * animation is a shrug; a frozen Emacs is not, and no flourish is
 * worth the difference.
 *
 * Five milliseconds is far longer than the lock is ever held for its
 * proper purposes: the longest is a frame's memcpy out of the tracks,
 * measured at 0.04 ms. */
#define SMEAR_LOCK_WAIT_MS 5

/* How long the overlay waits after a flight before going away.
 *
 * Long enough to span the gap between two keystrokes, short enough
 * that it is not sitting over the frame while anyone is reading.  See
 * where it is used. */
#define SMEAR_LINGER 0.35

static double mono(void);

/* Take the lock, or give up.  Non-zero if it was taken. */
static int trylock(Player *p)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);       /* timedlock wants the real clock */
    ts.tv_nsec += SMEAR_LOCK_WAIT_MS * 1000L * 1000L;
    if (ts.tv_nsec >= 1000L * 1000L * 1000L) {
        ts.tv_nsec -= 1000L * 1000L * 1000L;
        ts.tv_sec++;
    }
    return pthread_mutex_timedlock(&p->mu, &ts) == 0;
}

static double mono(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + ts.tv_nsec / 1e9;
}

static void deadline_at(struct timespec *ts, double when)
{
    ts->tv_sec  = (time_t)when;
    ts->tv_nsec = (long)((when - (double)ts->tv_sec) * 1e9);
    if (ts->tv_nsec > 999999999L) { ts->tv_nsec -= 1000000000L; ts->tv_sec++; }
}

/* Which frame of TR is due at NOW, or -1 once it has run out.
 *
 * At TR's own rate.  One rate for the whole player, set by whichever
 * flight was handed over last, meant an effect at thirty frames a
 * second re-paces the trail beside it to thirty, so the trail plays
 * at half speed for as long as an effect is on screen. */
static int track_frame(const Track *tr, double now)
{
    if (!tr->active || tr->fps <= 0) return -1;
    int k = (int)((now - tr->t0) * tr->fps);
    if (k < 0) k = 0;
    return k >= tr->nframes ? -1 : k;
}

static void grow_box(int *x0, int *y0, int *x1, int *y1, const SmearFrame *f)
{
    if (f->bw < 1 || f->bh < 1) return;
    if (f->bx < *x0) *x0 = f->bx;
    if (f->by < *y0) *y0 = f->by;
    if (f->bx + f->bw > *x1) *x1 = f->bx + f->bw;
    if (f->by + f->bh > *y1) *y1 = f->by + f->bh;
}

/* Copy out what this frame needs.  Called with the lock held, and kept
 * to a memcpy: this is the whole of what a handoff can wait for. */
static int snapshot(Player *p, double now, Snap *s)
{
    int x0 = 1 << 30, y0 = 1 << 30, x1 = -(1 << 30), y1 = -(1 << 30);
    int any = 0;
    for (int t = 0; t < SMEAR_MAX_TRACKS; t++) {
        int k = track_frame(&p->track[t], now);
        s->t[t].due = k;
        if (k < 0) { p->track[t].active = 0; continue; }
        any = 1;
        s->t[t].frozen  = p->track[t].frozen;
        s->t[t].still   = p->track[t].still;
        s->t[t].stale   = p->track[t].stale;
        p->track[t].stale = 0;
        s->t[t].nlayers = p->track[t].nlayers;
        memcpy(s->t[t].layers, p->track[t].layers,
               (size_t)p->track[t].nlayers * sizeof(SmearPaint));
        s->t[t].frame = p->track[t].frames[k];
        grow_box(&x0, &y0, &x1, &y1, &s->t[t].frame);
    }
    if (!any || x0 >= x1 || y0 >= y1) return 0;
    s->bx = x0; s->by = y0; s->bw = x1 - x0; s->bh = y1 - y0;
    return 1;
}

/* Draw one track's layers.  The backdrop is already down, laid once
 * for the whole frame, because every track draws over the same
 * live text and laying it per track would erase the track before.
 *
 * Nonzero SCALE applies this frame's envelope to each layer.  A still
 * track passes zero: its layers go into a stamp at their own strength,
 * and the envelope is applied to the stamp as a whole when it is
 * composited. */
static void draw_track(SmearStage *st, const Snap *s, int t, int scale)
{
    const SmearFrame *f = &s->t[t].frame;
    if (f->bw < 1 || f->bh < 1) return;
    for (int i = 0; i < s->t[t].nlayers; i++) {
        SmearPaint pt = s->t[t].layers[i];
        memcpy(pt.corners, f->corners[i], sizeof pt.corners);
        pt.head[0] = f->head[i][0];
        pt.head[1] = f->head[i][1];
        /* the layer's shape is fixed for the flight; this frame's
           place in the envelope is all that varies */
        if (scale)
            for (int sIdx = 0; sIdx < pt.nstops; sIdx++)
                pt.stop_alpha[sIdx] *= f->alpha[i];
        smear_stage_draw(st, &pt);
    }
}

/* Paint a snapshot.  Called with the lock NOT held: only this thread
 * touches X, so nothing else can be inside Xlib while it does. */
static void paint_snapshot(SmearStage *st, const Snap *s)
{
    /* The shape and the backdrop cover every track at once: they are
     * what the overlay shows, and it shows all of them.  The
     * compositing does not: each track is flushed over its own box.
     *
     * A full-width line pulse unioned with a cursor trail is most of
     * the window, and compositing that rectangle cost 93 ms a frame on
     * a large one.  As two boxes it is a strip and a smudge. */
    smear_stage_frame_begin(st, s->bx, s->by, s->bw, s->bh);
    for (int t = 0; t < SMEAR_MAX_TRACKS; t++) {
        if (s->t[t].due < 0) continue;
        const SmearFrame *f = &s->t[t].frame;
        smear_stage_region(st, f->bx, f->by, f->bw, f->bh);
        /* Under this track, the text as it was, for the first frames
         * of the flight.  The backdrop laid by frame_begin is the
         * window as it is now, which for a deletion is whatever closed
         * up over what we are marking.
         *
         * For the first frames rather than for all of them: held to
         * the end, the photograph keeps the deleted line on the screen
         * for as long as the effect runs, and what the typist sees is
         * the flash, a wait, and only then the line going.  The edit
         * has to look like it happened when it happened. */
        if (s->t[t].due < s->t[t].frozen) smear_stage_restore_frozen(st);
        /* A previous flight's stamp, discarded here because here is
         * the thread that owns the connection. */
        if (s->t[t].stale) smear_stage_stamp_drop(st, t);
        if (s->t[t].still) {
            /* Rendered once and kept.  Every frame of an effect draws
             * the same pixels, and reading them back off the GPU and
             * pushing them to the display again is, on a display
             * reached over a network, the whole of what it costs. */
            if (!smear_stage_stamp_replay(st, t, s->t[t].frame.alpha[0])) {
                draw_track(st, s, t, 0);
                if (smear_stage_stamp_take(st, t, f->bx, f->by, f->bw, f->bh))
                    smear_stage_stamp_replay(st, t, s->t[t].frame.alpha[0]);
                else
                    /* no stamp to be had (RENDER, or it would not
                       take), so draw it the ordinary way */
                    draw_track(st, s, t, 1);
            }
        } else {
            draw_track(st, s, t, 1);
        }
        smear_stage_frame_end(st);
    }
}

/* Cadence bookkeeping, so `smear-cursor-report' can still say what a
 * flight cost now that Lisp no longer sees the frames go by. */
static void note_frame(Player *p, double t_before, double t_after)
{
    p->live.frames++;
    p->live.paint_total += t_after - t_before;
    if (p->last_paint > 0) {
        double gap = t_before - p->last_paint;
        p->live.gap_sum += gap;
        if (p->live.ngaps < SMEAR_MAX_FRAMES)
            p->live.gaps[p->live.ngaps++] = gap;
        if (gap > p->live.gap_max) {
            p->live.gap_max = gap;
            p->live.gap_max_at = p->live.frames;
        }
    }
    p->last_paint = t_before;
}

static int playing(const Player *p)
{
    for (int t = 0; t < SMEAR_MAX_TRACKS; t++)
        if (p->track[t].active) return 1;
    return 0;
}

/* Take the flight just ended out of the live counters, into OUT.
 * Lock held.  Separate from publishing it because the one number the
 * report cannot get without X, how far behind the display is, can
 * only be measured with the lock released. */
static void close_books(Player *p, SmearPlayStats *out)
{
    *out = p->live;
    memset(&p->live, 0, sizeof p->live);
    p->pos = -1;
    p->last_paint = 0;
}

/* Let `smear-cursor-report' have it, if there was anything to it. */
static void publish(Player *p, const SmearPlayStats *s)
{
    if (s->frames > 0) { p->stats = *s; p->have_stats = 1; }
}

/* Close the books on a flight.  State only: taking the overlay down
 * is X, and X belongs to the player thread, which does it when it sees
 * nothing is playing.  A caller that did it here would be holding the
 * lock inside Xlib, which is the thing this file no longer does. */
static void finish(Player *p)
{
    for (int t = 0; t < SMEAR_MAX_TRACKS; t++) p->track[t].active = 0;
    SmearPlayStats done;
    close_books(p, &done);
    publish(p, &done);
}

static void *run(void *arg)
{
    Player *p = arg;
    Snap snap;
    pthread_mutex_lock(&p->mu);
    while (!p->quit) {
        if (!playing(p)) {
            if (p->showing) {            /* nothing left: show nothing */
                p->showing = 0;
                /* The books close under the lock and are published
                 * after, because the drain is measured in between and
                 * measuring it means letting go.  Publishing before
                 * would leave the report a flight without its one
                 * number that needs the server's answer; calling
                 * `finish' after would clear a flight handed over
                 * while we were outside the lock. */
                SmearPlayStats done;
                close_books(p, &done);
                pthread_mutex_unlock(&p->mu);
                /* The drain is a round trip, and on the display it
                 * exists to diagnose a round trip costs 52 ms.  Asked
                 * once a flight it was a tenth of the typing lag it
                 * was there to explain, so it is sampled instead: the
                 * report says how far behind the display was, not how
                 * far behind it was on this exact flight. */
                double now_d = mono();
                if (now_d - p->drained > 2.0) {
                    p->drain_last = smear_stage_drain(p->st);
                    p->drained = now_d;
                }
                done.drain = p->drain_last;
                done.drain_age = p->drained > 0 ? now_d - p->drained : -1.0;
                /* Down, not merely showing nothing.
                 *
                 * Leaving it mapped is cheaper, since putting it back
                 * up costs a map and on a forwarded display that is
                 * 33.6 ms, but it costs the keyboard.  Watched
                 * continuously, focus was `None' for as long as the
                 * overlay sat on top of the frame and came back the
                 * instant the frame was above it again:
                 *
                 *   focus=None   overlay ABOVE at 2, emacs at 1
                 *   focus=None   overlay ABOVE at 2, emacs at 1
                 *   focus=Emacs  overlay BELOW at 1, emacs at 2
                 *
                 * An editor that stops taking keys is not a cheaper
                 * editor.  Between flights the overlay is nobody's
                 * business, so it goes away, and `smear_stage_begin'
                 * maps it raised again, which is also the only thing
                 * this display honours for getting on top. */
                pthread_mutex_lock(&p->mu);
                publish(p, &done);
                p->idle_at = mono();
                continue;
            }
            if (p->up) {
                /* Up a little longer, in case another flight is
                 * coming.
                 *
                 * Putting it back costs a map, 33.6 ms here and the
                 * cheapest this display honours, and typing
                 * makes one flight per keystroke, so paying it per
                 * flight is paying it per keystroke.  Keeping it
                 * mapped for good is what cost the keyboard: focus
                 * went to `None' for as long as it sat above the
                 * frame.  A moment is neither: a burst of typing pays
                 * one map between them all, and a few tenths after the
                 * last keystroke it is gone again. */
                double left = p->idle_at + SMEAR_LINGER - mono();
                if (left <= 0) {
                    p->up = 0;
                    pthread_mutex_unlock(&p->mu);
                    smear_stage_end(p->st);
                    pthread_mutex_lock(&p->mu);
                    continue;
                }
                struct timespec ts;
                deadline_at(&ts, mono() + left);
                pthread_cond_timedwait(&p->cv, &p->mu, &ts);
                continue;
            }
            pthread_cond_wait(&p->cv, &p->mu);
            continue;
        }
        if (!p->up) {
            pthread_mutex_unlock(&p->mu);
            int ok = smear_stage_begin(p->st);
            pthread_mutex_lock(&p->mu);
            if (!ok) { finish(p); continue; }
            p->up = 1;
            /* Start the clocks now rather than when the flight was
             * handed over.  Putting the overlay up is round trips, and
             * charging them to the flight made it open already several
             * frames in, so the smear appeared half-formed instead of
             * growing out of the cursor.  Only on the first flight
             * now: after that the overlay stays up, showing nothing,
             * and there is nothing to charge. */
            double now = mono();
            for (int t = 0; t < SMEAR_MAX_TRACKS; t++)
                if (p->track[t].active) p->track[t].t0 = now;
        }
        p->showing = 1;
        double now = mono();
        if (!snapshot(p, now, &snap)) {
            /* Nothing worth painting this frame.  If the tracks have
             * run out the loop above takes the overlay down; if they
             * have not (a frame whose box came out empty), wait for
             * the next one rather than going round again.
             *
             * Going round again is a spin holding this lock, and this
             * lock is what `smear_play_flight' takes, on Emacs's
             * thread, on every keystroke.  Measured with a flight of
             * forty empty frames, handing over the next one took
             * 636 ms: not a bad animation, an editor that has
             * stopped. */
            if (playing(p)) {
                struct timespec ts;
                deadline_at(&ts, now + 1.0 / 60.0);
                pthread_cond_timedwait(&p->cv, &p->mu, &ts);
            }
            continue;
        }

        /* Out of the lock for the drawing.  A handoff waits for the
         * memcpy above, not for a frame. */
        pthread_mutex_unlock(&p->mu);
        double t_paint = mono();
        paint_snapshot(p->st, &snap);
        double t_done = mono();
        pthread_mutex_lock(&p->mu);
        note_frame(p, t_paint, t_done);

        /* One clock for every track, not one each.
         *
         * Waking for whichever track's next frame came first meant two
         * tracks at the same rate but out of phase, such as a typing
         * blink over a trail or two yanks in quick succession, woke the
         * loop twice an interval.  Each waking paints a whole frame:
         * the backdrop, and every track that is due.  Measured, two
         * tracks of thirty-six frames painted seventy-three frames
         * 8.3 ms apart and put twice as much on the wire as they had
         * frames for.
         *
         * A track's *frame* still comes from its own clock, so rates
         * stay separate; what is shared is when the loop looks. */
        double fps = 0.0;
        for (int t = 0; t < SMEAR_MAX_TRACKS; t++)
            if (p->track[t].active && p->track[t].fps > fps)
                fps = p->track[t].fps;
        if (fps <= 0.0) fps = 60.0;
        double next = now + 1.0 / fps;
        struct timespec ts;
        deadline_at(&ts, next);
        pthread_cond_timedwait(&p->cv, &p->mu, &ts);
    }
    int was_up = p->up;
    p->up = 0;
    p->showing = 0;
    pthread_mutex_unlock(&p->mu);
    if (was_up) smear_stage_end(p->st);
    return NULL;
}

static Player *player_of(SmearStage *st)
{
    void **slot = smear_stage_play_slot(st);
    return slot ? *slot : NULL;
}

int smear_play_start(SmearStage *st, char *err, size_t errlen)
{
    if (!st) return 1;
    if (smear_stage_trouble(st)) {
        if (err) snprintf(err, errlen, "%s", smear_stage_trouble(st));
        return 1;
    }
    void **slot = smear_stage_play_slot(st);
    if (!slot) return 1;
    if (*slot) return 0;

    Player *p = calloc(1, sizeof *p);
    if (!p) { if (err) snprintf(err, errlen, "out of memory"); return 1; }
    p->st = st;
    p->pos = -1;
    pthread_mutex_init(&p->mu, NULL);

    /* The frame clock is monotonic, so the condition variable must
     * wait on the same one: a wall clock stepping backwards would
     * park the player for as long as the step. */
    pthread_condattr_t attr;
    pthread_condattr_init(&attr);
    pthread_condattr_setclock(&attr, CLOCK_MONOTONIC);
    pthread_cond_init(&p->cv, &attr);
    pthread_condattr_destroy(&attr);

    if (pthread_create(&p->tid, NULL, run, p) != 0) {
        pthread_cond_destroy(&p->cv);
        pthread_mutex_destroy(&p->mu);
        free(p);
        if (err) snprintf(err, errlen, "cannot start the player thread");
        return 1;
    }
    p->running = 1;
    *slot = p;
    return 0;
}

int smear_play_flight(SmearStage *st, int track,
                      const SmearPaint *layers, int nlayers,
                      const SmearFrame *frames, int nf, double fps,
                      int frozen, int still)  /* frozen: frames to hold it */
{
    Player *p = player_of(st);
    if (!p || nlayers < 1 || nf < 1 || fps <= 0) return 0;
    if (track < 0 || track >= SMEAR_MAX_TRACKS) return 0;
    if (nlayers > SMEAR_MAX_PLAY_LAYERS) nlayers = SMEAR_MAX_PLAY_LAYERS;
    if (nf > SMEAR_MAX_FRAMES) nf = SMEAR_MAX_FRAMES;

    if (!trylock(p)) return 0;     /* see SMEAR_LOCK_WAIT_MS */
    Track *tr = &p->track[track];
    memcpy(tr->layers, layers, (size_t)nlayers * sizeof *layers);
    memcpy(tr->frames, frames, (size_t)nf * sizeof *frames);
    tr->nlayers = nlayers;
    tr->nframes = nf;
    tr->fps     = fps;
    tr->frozen  = frozen;
    tr->still   = still;
    /* Whatever the last flight left on this track is not this flight's
     * pixels, but dropping it is X, and this runs on the caller's
     * thread with the lock held.  Both halves of that are forbidden:
     * the lock is what a handoff waits on, and the stage's connection
     * belongs to the player.  So say so, and let the player do it. */
    tr->stale = 1;
    tr->t0 = mono();
    /* A retarget replaces this track's flight without taking the
     * overlay down: putting it up copies the whole window, and doing
     * that again mid-movement is the one cost here worth avoiding. */
    tr->active = 1;
    p->pos = -1;
    pthread_cond_signal(&p->cv);
    pthread_mutex_unlock(&p->mu);
    return 1;
}

int smear_play_position(SmearStage *st, int track)
{
    Player *p = player_of(st);
    if (!p || track < 0 || track >= SMEAR_MAX_TRACKS) return -1;
    if (!trylock(p)) return -1;     /* see SMEAR_LOCK_WAIT_MS */
    int k = -1;
    Track *tr = &p->track[track];
    if (tr->active && tr->fps > 0) {
        k = (int)((mono() - tr->t0) * tr->fps);
        if (k < 0) k = 0;
        if (k >= tr->nframes) k = tr->nframes - 1;
    }
    pthread_mutex_unlock(&p->mu);
    return k;
}

void smear_play_stop_track(SmearStage *st, int track)
{
    Player *p = player_of(st);
    if (!p || track < 0 || track >= SMEAR_MAX_TRACKS) return;
    if (!trylock(p)) return;     /* see SMEAR_LOCK_WAIT_MS */
    p->track[track].active = 0;
    if (!playing(p)) finish(p);
    pthread_cond_signal(&p->cv);
    pthread_mutex_unlock(&p->mu);
}

void smear_play_stop(SmearStage *st)
{
    Player *p = player_of(st);
    if (!p) return;
    if (!trylock(p)) return;     /* see SMEAR_LOCK_WAIT_MS */
    finish(p);
    pthread_cond_signal(&p->cv);
    pthread_mutex_unlock(&p->mu);
    /* The overlay comes down a moment later, on the player's thread.
     * Doing it here would mean holding the lock inside Xlib, and that
     * lock is what a handoff waits on. */
}

int smear_play_stats(SmearStage *st, SmearPlayStats *out)
{
    Player *p = player_of(st);
    if (!p || !out) return 0;
    if (!trylock(p)) return 0;     /* see SMEAR_LOCK_WAIT_MS */
    int ok = p->have_stats;
    if (ok) *out = p->stats;
    pthread_mutex_unlock(&p->mu);
    return ok;
}

void smear_play_wedge_for_test(SmearStage *st, int ms)
{
    Player *p = player_of(st);
    if (!p) return;
    pthread_mutex_lock(&p->mu);
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000L * 1000L };
    nanosleep(&ts, NULL);
    pthread_mutex_unlock(&p->mu);
}

void smear_play_shutdown(SmearStage *st)
{
    if (!st) return;
    void **slot = smear_stage_play_slot(st);
    Player *p = slot ? *slot : NULL;
    if (!p) return;
    *slot = NULL;
    pthread_mutex_lock(&p->mu);
    p->quit = 1;
    for (int t = 0; t < SMEAR_MAX_TRACKS; t++) p->track[t].active = 0;
    pthread_cond_signal(&p->cv);
    pthread_mutex_unlock(&p->mu);
    pthread_join(p->tid, NULL);
    pthread_cond_destroy(&p->cv);
    pthread_mutex_destroy(&p->mu);
    free(p);
}
