/* smear-cursor-x11-core -- composite a quad over an X window's own
 * contents.
 *
 * Knows nothing about cursors, springs or Emacs.  Given a window and
 * four corners, it lays an antialiased, alpha-blended quad over that
 * window's live pixels and takes it away again.
 *
 * The one idea it is built on: a client can read what another window
 * drew, so the trail does not have to reconstruct the text it covers.
 * Inside Emacs a trail replaces the glyphs it crosses, and that is
 * where every corner case came from.
 */
#ifndef SMEAR_CURSOR_X11_CORE_H
#define SMEAR_CURSOR_X11_CORE_H

#include <stddef.h>

typedef struct SmearStage SmearStage;

/* One layer of a trail.
 *
 * A trail is composed of several of these over one backdrop: a solid
 * core, a grown and blurred halo behind it, a glow at the head.  The
 * caller decides what the layers are; this only knows how to draw one.
 *
 * Corners are in the target window's own pixels, as TL TR BR BL.  HEAD
 * is where the trail is brightest and where the gradient starts.
 */
#define SMEAR_KIND_QUAD   0
#define SMEAR_KIND_RADIAL 1     /* a soft round glow centred on HEAD */
#define SMEAR_MAX_STOPS   8

/* How many layers one frame may be drawn from.
 *
 * The one capacity, because there are three places that have to agree:
 * the array a flight's layers arrive in, the array the GL renderer
 * holds them in until the frame closes, and the uniform arrays the
 * shader declares.  They were three separate numbers, and raising one
 * of them and not the others lost every layer past the smallest
 * without saying so.
 *
 * This is the ceiling, not the answer.  The shader's share of it is
 * settled against the driver when the GL context comes up, since the
 * uniform space a fragment shader is guaranteed runs to sixteen
 * layers and no further, and `smear-cursor-x11--max-layers' reports
 * whichever of the two is smaller.  Sixteen was the whole budget for
 * an effect: a figure of a dozen layers left nothing for the text it
 * was eating. */
#define SMEAR_MAX_LAYERS  32

typedef struct {
    int    kind;
    double corners[8];
    double head[2];
    double r, g, b;         /* 0..1 */
    double grow;            /* pixels to push each corner outward */
    double blur;            /* convolution radius; 0 for a hard edge */
    double radius;          /* SMEAR_KIND_RADIAL only */
    int    nstops;          /* alpha along head -> tail, at least 2 */
    double stop_at[SMEAR_MAX_STOPS];
    double stop_alpha[SMEAR_MAX_STOPS];
} SmearPaint;

/* Open a connection and take over WINDOW.  DISPLAY may be NULL for
 * $DISPLAY.  Returns NULL if the display cannot be opened; use
 * smear_stage_trouble() on a non-NULL stage to learn whether it can
 * actually composite. */
SmearStage *smear_stage_open(const char *display, unsigned long window);

/* NULL when the stage is fit to draw, otherwise why it is not. */
const char *smear_stage_trouble(SmearStage *st);

/* A one-line description of the display, for diagnostics. */
const char *smear_stage_describe(SmearStage *st);

/* Put the overlay up.  Cheap to call again while already up. */
int  smear_stage_begin(SmearStage *st);

void smear_stage_frame_begin(SmearStage *st, int x, int y, int w, int h);

/* Narrow what the next `smear_stage_frame_end' composites over,
 * without touching the shape or relaying the backdrop.
 *
 * This is what lets several trails share a frame without sharing a
 * rectangle.  A full-width line pulse and a cursor trail unioned into
 * one box is most of the window; drawn in their own boxes it is a
 * strip and a smudge.  Called between draws, once per box. */
void smear_stage_region(SmearStage *st, int x, int y, int w, int h);

/* One layer, over whatever is already on this frame. */
void smear_stage_draw(SmearStage *st, const SmearPaint *p);

/* Finish a frame: put it on the wire. */
void smear_stage_frame_end(SmearStage *st);

/* Which renderer draws the layers.
 *
 * `render' composes RENDER primitives inside the X server, so only
 * coordinates cross the wire.  It is fixed-function.
 *
 * `gl' runs a fragment shader where Emacs is and uploads the result,
 * and it can express what a per-pixel program can.  Which of the two
 * is cheaper depends on the style and the connection. */
#define SMEAR_RENDERER_RENDER 0
#define SMEAR_RENDERER_GL     1

/* Ask for a renderer.  Returns the one actually in use: GL falls back
 * to RENDER when the context will not come up, and ERR then says why. */
int smear_stage_set_renderer(SmearStage *st, int which,
                             char *err, size_t errlen);
int smear_stage_renderer(SmearStage *st);

/* Non-zero when this display can blur a layer server-side.  Without it
 * a layer's `blur' is ignored and its edge stays hard. */
int  smear_stage_can_blur(SmearStage *st);

/* Take the overlay down; the window beneath is left as it was. */
void smear_stage_end(SmearStage *st);

/* How far behind the server is, in seconds: the cost of one round
 * trip.  See the definition.  On a forwarded display this is the only
 * number that can tell a frame queued on time from one seen on time. */
double smear_stage_drain(SmearStage *st);

/* Photograph X,Y,W,H of the target now, and keep it.
 *
 * For an effect that marks text which is about to stop existing.  A
 * deletion is drawn over live pixels like everything else here, and by
 * the time the first frame is painted the text is gone and the flash
 * lands on whatever closed up behind it, which marks the wrong thing.
 * Called from inside
 * `before-change-functions', this keeps the pixels; a track playing
 * frozen has them laid back under it every frame.
 *
 * Returns non-zero if it took.  Flushes: see the definition. */
int  smear_stage_freeze(SmearStage *st, int x, int y, int w, int h);

/* Lay the photograph back on the overlay, over the frame's backdrop. */
void smear_stage_restore_frozen(SmearStage *st);

/* How many flights may keep a stamp at once.  One per track, so this
 * follows SMEAR_MAX_TRACKS: a still flight on a track past the last
 * stamp is bounds-checked out of one and drawn afresh every frame,
 * which for the cursor's own glow -- a flight that never ends -- is
 * the one cost worth avoiding. */
#define SMEAR_STAGE_STAMPS 5

/* Render whatever `smear_stage_draw' has queued into stamp SLOT,
 * covering X Y W H, and keep it.  Non-zero if it took.
 *
 * For a flight that does not move.  An effect is a shape held still
 * while its alpha rises and falls, so every frame of it renders the
 * same pixels, reads them back off the GPU and pushes them to the
 * display again.  On a display reached over a network that upload is
 * the whole of what the animation costs.  Rendered once and kept,
 * the frames after the first are a composite the server does by
 * itself.
 *
 * Only the GL renderer uploads anything; under RENDER the layers are
 * drawn by the server already and this does nothing. */
int  smear_stage_stamp_take(SmearStage *st, int slot, int x, int y, int w, int h);

/* Composite stamp SLOT onto the overlay at ALPHA, 0 to 1.  Non-zero if
 * there was a stamp to composite. */
int  smear_stage_stamp_replay(SmearStage *st, int slot, double alpha);

/* Forget stamp SLOT; the next flight on it renders afresh. */
void smear_stage_stamp_drop(SmearStage *st, int slot);

/* How many boxes have been read back off the GPU and pushed to the
 * display since the process started.  What an animation costs on a
 * display reached over a network is this number times the box. */
unsigned long smear_stage_uploads(void);

/* How many times the overlay has been put on screen.  See the
 * definition: this is what a flight costs before it draws anything. */
unsigned long smear_stage_maps(void);

/* The overlay window's XID.  For tests, which have to read back what
 * actually reached it; everything else here goes through the stage.
 * Unsigned long rather than Window, so this header keeps not needing
 * Xlib: the module and the demo include it, and nothing else should
 * have to. */
unsigned long smear_stage_overlay(SmearStage *st);

/* Non-zero when the last painted region held something other than a
 * flat colour, so the trail is standing on real text rather than on a
 * blank rectangle.  Costs a round trip; for tests, not for every
 * frame. */
int  smear_stage_background_has_detail(SmearStage *st);

/* Fingerprint a region of the overlay: mean and standard deviation of
 * the green channel, and how many pixels differ from the window's own
 * text there.  For telling two trails apart by measurement rather than
 * by eye.  Costs a round trip. */
void smear_stage_sample(SmearStage *st, int x, int y, int w, int h,
                        double *mean, double *sd, long *changed);

/* A slot on the stage for the player thread's state, so
 * smear-cursor-x11-play.c needs nothing of this struct's insides.
 * NULL if ST is. */
void **smear_stage_play_slot(SmearStage *st);

void smear_stage_close(SmearStage *st);

#endif
