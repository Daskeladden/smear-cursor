/* smear-cursor-x11-gl -- draw a trail's layers with a fragment shader.
 *
 * The other renderer composes RENDER primitives, which run inside the X
 * server: gradients, a coverage mask, a convolution.  That is why it is
 * usable over a forwarded connection, where only coordinates cross the
 * wire.  That is also its ceiling.  RENDER is fixed-function.
 * There is no per-pixel program in it, so no noise, no distortion, and
 * no way to sample what is underneath in order to disturb it.
 *
 * This renders the whole trail in one shader pass instead, on the
 * machine Emacs is running on, and uploads the result.  That upload is
 * the trade: one trail-sized RGBA image a frame, whose cost depends on
 * the image size and the connection.  A style of several layers still
 * costs one upload, so it can win over a forward where a single flat
 * quad does not.
 *
 * What it buys, beyond effects: the shader works in signed distance, so
 * `grow' is an offset and `blur' is the width of a smoothstep, with no
 * separate mask and convolution pass.
 */
#ifndef SMEAR_CURSOR_X11_GL_H
#define SMEAR_CURSOR_X11_GL_H

#include "smear-cursor-x11-core.h"
#include <X11/Xlib.h>
#include <X11/extensions/Xrender.h>

/* Bring the GL context up.  Zero on success; otherwise ERR says why,
 * and the caller should use the RENDER path.  Cheap to call again. */
int smear_gl_init(char *err, size_t errlen);

/* Non-zero once `smear_gl_init' has succeeded. */
int smear_gl_ready(void);

/* How many layers this driver's shader was built to hold, or zero
 * when there is no GL.  Settled against the driver when the context
 * came up: the uniform space a fragment shader gets is guaranteed
 * only to sixteen layers' worth, and asking for more than there is
 * does not lose the layers that went over, it loses the shader. */
int smear_gl_max_layers(void);

/* Render N layers over the rectangle BX BY BW BH and hand back a
 * 32-bit pixmap of that size holding the result.  ROOT is any drawable
 * on D of DEPTH.  The caller owns the pixmap and must free it; zero on
 * failure.
 *
 * Separate from drawing because an effect does not move: its pixels
 * are the same on every frame of its flight and only its alpha
 * changes, so it is rendered and uploaded once and composited from
 * there.  On a display reached over a network that upload is what an
 * animation costs. */
Pixmap smear_gl_render(Display *d, Drawable root, unsigned depth,
                       const SmearPaint *layers, int n,
                       int bx, int by, int bw, int bh);

/* Render N layers as above and composite the result onto DST at BX BY.
 * Non-zero on success. */
int smear_gl_draw(Display *d, Drawable root, Picture dst, unsigned depth,
                  const SmearPaint *layers, int n,
                  int bx, int by, int bw, int bh, double seconds);

void smear_gl_shutdown(void);

#endif
