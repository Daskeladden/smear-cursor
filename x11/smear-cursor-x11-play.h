/* smear-cursor-x11-play -- play a flight on a thread of our own.
 *
 * Emacs runs Lisp on one thread and a timer only fires between
 * commands, so a smear driven from Lisp stops dead whenever anything
 * else takes a while: jit-lock over a freshly scrolled region, a mode
 * line, a process filter.  Measured on a real session, fourteen frames
 * of a fifteen-frame flight arrived within half a millisecond of their
 * mark and one arrived 235 ms late.  No amount of making the paint
 * cheaper touches that: the paint was 6 ms and never got the chance to
 * run.
 *
 * So the frames stop being Lisp's job.  The spring is deterministic
 * given a start and a target, and `smear-cursor-max-duration' bounds a
 * flight to a few dozen frames, so Lisp integrates the whole flight
 * when the cursor moves and hands the lot over.  This thread paints
 * them on its own clock, and Emacs may block for as long as it likes
 * without the trail noticing.
 *
 * What stays in Lisp is everything worth changing: the spring, the
 * styles, the echoes, the colours.  What comes here is a player.
 *
 * Threading rule: once started, this thread is the only one that
 * touches the stage's Display or the GL context.  Every entry point
 * below takes the player's lock.  The thread copies the frame under
 * that lock and releases it before painting, so a caller waits for the
 * copy and not for a frame.
 */
#ifndef SMEAR_CURSOR_X11_PLAY_H
#define SMEAR_CURSOR_X11_PLAY_H

#include "smear-cursor-x11-core.h"

#define SMEAR_MAX_FRAMES 96     /* max-duration bounds a flight far below */
#define SMEAR_MAX_PLAY_LAYERS SMEAR_MAX_LAYERS

/* Flights play side by side, because the occasions they mark overlap:
 * typing moves the cursor, so a keystroke's flash and the cursor's
 * trail are on screen at the same moment.  Each track is one flight
 * with its own clock; the player unions their boxes and draws them all
 * into the one frame. */
#define SMEAR_MAX_TRACKS 5
#define SMEAR_TRACK_TRAIL 0     /* the cursor's own trail */

/* One frame of a flight: the box to paint and where each layer's quad
 * stands in it.  Everything else about a layer (colour, alpha stops,
 * grow, blur, kind) holds still across a flight and is given once. */
typedef struct {
    int    bx, by, bw, bh;
    double corners[SMEAR_MAX_PLAY_LAYERS][8];
    double head[SMEAR_MAX_PLAY_LAYERS][2];
    /* Scales the layer's alpha for this frame alone.  A trail leaves
     * it at one; an effect is mostly this: a pulse is a rectangle
     * that holds still while its alpha rises and falls, and without a
     * per-frame alpha the layers would have to be resent every
     * frame. */
    double alpha[SMEAR_MAX_PLAY_LAYERS];
} SmearFrame;

/* Cadence of the flight just played, for the report.  PAINT_TOTAL and
 * the gaps are seconds; GAP_MAX_AT counts frames from one. */
typedef struct {
    int    frames;
    double paint_total;
    double gap_max;
    int    gap_max_at;
    double gap_sum;
    /* Every gap, so the report can take a median.  One stall drags a
     * mean far enough to read as chronic slowness, which is exactly
     * the mistake this thread exists to stop people making. */
    double gaps[SMEAR_MAX_FRAMES];
    int    ngaps;
    /* One round trip after the last frame was queued: how far behind
     * the display was when the flight ran out.  See
     * smear_stage_drain.  The frames above are timed as they are
     * handed to the socket, which on a forwarded display is not when
     * they are seen. */
    double drain;
    /* How long ago the drain was measured, in seconds.  It is sampled
     * rather than taken every flight, because the sample is a round
     * trip and on the display it exists to diagnose a round trip is
     * 52 ms.  The reader has to be told whether the figure is this
     * flight's or one from a while back. */
    double drain_age;
} SmearPlayStats;

/* Bring the player thread up for ST.  Cheap to call again.  Zero on
 * success, otherwise ERR says why and the caller should drive frames
 * itself. */
int smear_play_start(SmearStage *st, char *err, size_t errlen);

/* Hand over a flight.  LAYERS describes what to draw and FRAMES says
 * where, NF of them at FPS.  Everything is copied, so the caller's
 * storage is its own again on return.  Replaces a flight in progress
 * without taking the overlay down between them.  Non-zero on success. */
/* FROZEN marks a flight that plays over a photograph taken before the
 * text it marks was removed.  See smear_stage_freeze.  Zero for
 * everything else, which draws over the window as it now is.
 *
 * STILL marks one whose shapes do not move: every frame draws the same
 * pixels and only the alpha differs, so it is rendered and uploaded
 * once and the frames after the first are a composite the server does
 * by itself.  See smear_stage_stamp_take.  Only the caller can know
 * this, so only the caller may say it.  A flight declared still whose
 * corners move plays its first frame over and over. */
int smear_play_flight(SmearStage *st, int track,
                      const SmearPaint *layers, int nlayers,
                      const SmearFrame *frames, int nf, double fps,
                      int frozen, int still);

/* Which frame of TRACK is on screen, or -1 when it is not playing.  A
 * retarget asks this to know where to pick the spring up from. */
int smear_play_position(SmearStage *st, int track);

/* Stop every track, and take the overlay down. */
void smear_play_stop(SmearStage *st);

/* Stop one track.  The overlay stays up while any other is playing. */
void smear_play_stop_track(SmearStage *st, int track);

/* Cadence of the last flight.  Non-zero if there was one. */
int smear_play_stats(SmearStage *st, SmearPlayStats *out);

/* Stop the thread and let it go.  Safe on a stage that never started
 * one. */
void smear_play_shutdown(SmearStage *st);

/* Hold the player's lock for MS milliseconds, as a wedged player
 * would.  For the test that says Emacs's thread comes back anyway;
 * nothing else has any business calling it. */
void smear_play_wedge_for_test(SmearStage *st, int ms);

#endif
