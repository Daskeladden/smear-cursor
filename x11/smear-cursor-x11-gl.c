/* See smear-cursor-x11-gl.h. */
#define GL_GLEXT_PROTOTYPES 1
#include "smear-cursor-x11-gl.h"

#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GL/gl.h>
#include <GL/glext.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LAYERS SMEAR_MAX_LAYERS

/* Uniform components one layer of the shader declares.
 *
 * Thirteen rows of it: four for the quad's corners, and one each for
 * the head, the colour, the kind, the grow, the blur, the radius, the
 * stop count, the stop positions and the stop alphas.  A row is a
 * vec4 whatever it holds, which is how a uniform array is packed, so
 * that is fifty-two components.
 *
 * Desktop GL guarantees a thousand and twenty-four of them and no
 * more, which is sixteen layers' worth.  Most drivers offer many
 * times that, and there is no way to know but to ask -- and a shader
 * that asks for more than there is does not fail on the layer that
 * went over, it fails to link at all, and every effect vanishes.  So
 * the count is settled against the driver when the context comes up,
 * and the shader is built around the answer. */
#define SMEAR_LAYER_COMPONENTS 52
#define SMEAR_SPARE_COMPONENTS 32   /* iOrigin, iCount, and room to spare */

static struct {
    EGLDisplay dpy;
    EGLContext ctx;
    GLuint prog, fbo, tex, vao;
    int tw, th;               /* the texture's size */
    unsigned char *pixels;    /* readback buffer */
    int pixcap;
    char err[256];
    int ready;
    int nlayers;              /* what the driver would allow, capped */
} G;

/* The trail, as one pass.
 *
 * Every layer is a signed distance: a quad the spring has deformed, or
 * a disc at the head.  `grow' is then a constant subtracted from that
 * distance and `blur' is the width of the smoothstep across the edge,
 * both of which are free here.  The other renderer needs a wider mask
 * and a convolution over it to say the same thing.
 *
 * The alpha runs along head-to-tail through the layer's stops, so a
 * style's shape survives unchanged from one renderer to the other. */
static const char *FRAG_BODY =
"out vec4 frag;\n"
"uniform vec2  iOrigin;\n"
"uniform int   iCount;\n"
"uniform vec2  iQuad[NL * 4];\n"
"uniform vec2  iHead[NL];\n"
"uniform vec3  iColor[NL];\n"
"uniform int   iKind[NL];\n"
"uniform float iGrow[NL];\n"
"uniform float iBlur[NL];\n"
"uniform float iRadius[NL];\n"
"uniform int   iStops[NL];\n"
"uniform vec4  iStopAt[NL];\n"
"uniform vec4  iStopA[NL];\n"
"\n"
"// signed distance to a quad, negative inside.  The quad is a spring\n"
"// deformation of the cursor rect, so it is not a parallelogram and\n"
"// often not convex: this is the general polygon form.\n"
"float sdQuad(vec2 p, int b) {\n"
"  float d = dot(p - iQuad[b], p - iQuad[b]);\n"
"  float s = 1.0;\n"
"  for (int i = 0, j = 3; i < 4; j = i, i++) {\n"
"    vec2 e = iQuad[b + j] - iQuad[b + i];\n"
"    vec2 w = p - iQuad[b + i];\n"
"    vec2 bb = w - e * clamp(dot(w, e) / dot(e, e), 0.0, 1.0);\n"
"    d = min(d, dot(bb, bb));\n"
"    bvec3 c = bvec3(p.y >= iQuad[b + i].y, p.y < iQuad[b + j].y,\n"
"                    e.x * w.y > e.y * w.x);\n"
"    if (all(c) || all(not(c))) s = -s;\n"
"  }\n"
"  return s * sqrt(d);\n"
"}\n"
"\n"
"float alphaAt(int L, float t) {\n"
"  int n = iStops[L];\n"
"  vec4 at = iStopAt[L], a = iStopA[L];\n"
"  if (t <= at[0]) return a[0];\n"
"  for (int i = 1; i < 4; i++) {\n"
"    if (i >= n) break;\n"
"    if (t <= at[i]) {\n"
"      float u = (t - at[i-1]) / max(1e-5, at[i] - at[i-1]);\n"
"      return mix(a[i-1], a[i], u);\n"
"    }\n"
"  }\n"
"  return a[min(n, 4) - 1];\n"
"}\n"
"\n"
"void main() {\n"
"  vec2 p = iOrigin + gl_FragCoord.xy;\n"
"  vec4 acc = vec4(0.0);\n"
"  for (int L = 0; L < iCount; L++) {\n"
"    int b = L * 4;\n"
"    float d, t;\n"
"    if (iKind[L] == 1) {\n"
"      d = length(p - iHead[L]) - iRadius[L];\n"
"      t = clamp(length(p - iHead[L]) / max(1.0, iRadius[L]), 0.0, 1.0);\n"
"    } else {\n"
"      d = sdQuad(p, b) - iGrow[L];\n"
"      float far = 0.0;\n"
"      for (int i = 0; i < 4; i++)\n"
"        far = max(far, length(iQuad[b + i] - iHead[L]));\n"
"      t = clamp(length(p - iHead[L]) / max(1.0, far), 0.0, 1.0);\n"
"    }\n"
"    float w = max(0.75, iBlur[L]);\n"
"    float cov = 1.0 - smoothstep(-w, w, d);\n"
"    float a = cov * alphaAt(L, t);\n"
"    // premultiplied, over what the earlier layers put down\n"
"    acc.rgb = iColor[L] * a + acc.rgb * (1.0 - a);\n"
"    acc.a   = a + acc.a * (1.0 - a);\n"
"  }\n"
"  frag = acc;\n"
"}\n";

static const char *VERT =
"#version 330 core\n"
"const vec2 v[3] = vec2[3](vec2(-1.0,-1.0), vec2(3.0,-1.0), vec2(-1.0,3.0));\n"
"void main() { gl_Position = vec4(v[gl_VertexID], 0.0, 1.0); }\n";

static GLuint compile(GLenum type, const char *src)
{
    GLuint s = glCreateShader(type);
    glShaderSource(s, 1, &src, NULL);
    glCompileShader(s);
    GLint ok = 0;
    glGetShaderiv(s, GL_COMPILE_STATUS, &ok);
    if (!ok) {
        GLsizei n = 0;
        glGetShaderInfoLog(s, (GLsizei)sizeof G.err - 32, &n, G.err);
        glDeleteShader(s);
        return 0;
    }
    return s;
}

static void release(void);

int smear_gl_ready(void) { return G.ready; }

int smear_gl_max_layers(void) { return G.ready ? G.nlayers : 0; }

int smear_gl_init(char *err, size_t errlen)
{
    if (G.ready) return 0;
    if (G.err[0]) {                 /* asked before and failed */
        if (err) snprintf(err, errlen, "%s", G.err);
        return 1;
    }
    /* Surfaceless: nothing is presented from here, the result is read
     * back and handed to X. */
    PFNEGLGETPLATFORMDISPLAYEXTPROC getdpy =
        (PFNEGLGETPLATFORMDISPLAYEXTPROC)
        eglGetProcAddress("eglGetPlatformDisplayEXT");
    if (getdpy)
        G.dpy = getdpy(0x31DD /* EGL_PLATFORM_SURFACELESS_MESA */,
                       EGL_DEFAULT_DISPLAY, NULL);
    if (G.dpy == EGL_NO_DISPLAY) G.dpy = eglGetDisplay(EGL_DEFAULT_DISPLAY);
    if (G.dpy == EGL_NO_DISPLAY) {
        snprintf(G.err, sizeof G.err, "no EGL display");
        goto fail;
    }
    if (!eglInitialize(G.dpy, NULL, NULL)) {
        snprintf(G.err, sizeof G.err, "eglInitialize failed");
        goto fail;
    }
    if (!eglBindAPI(EGL_OPENGL_API)) {
        snprintf(G.err, sizeof G.err, "no desktop GL through EGL");
        goto fail;
    }
    EGLint cfga[] = { EGL_SURFACE_TYPE, EGL_PBUFFER_BIT,
                      EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT, EGL_NONE };
    EGLConfig cfg; EGLint n = 0;
    if (!eglChooseConfig(G.dpy, cfga, &cfg, 1, &n) || n < 1) {
        snprintf(G.err, sizeof G.err, "no usable EGL config");
        goto fail;
    }
    EGLint ctxa[] = { EGL_CONTEXT_MAJOR_VERSION, 3,
                      EGL_CONTEXT_MINOR_VERSION, 3, EGL_NONE };
    G.ctx = eglCreateContext(G.dpy, cfg, EGL_NO_CONTEXT, ctxa);
    if (G.ctx == EGL_NO_CONTEXT) {
        snprintf(G.err, sizeof G.err, "eglCreateContext failed");
        goto fail;
    }
    if (!eglMakeCurrent(G.dpy, EGL_NO_SURFACE, EGL_NO_SURFACE, G.ctx)) {
        snprintf(G.err, sizeof G.err, "eglMakeCurrent failed");
        goto fail;
    }
    /* What the driver will hold, less what the rest of the shader
     * uses, and never more than the arrays a flight arrives in.  Never
     * fewer than sixteen either: that is the guaranteed minimum, and a
     * driver reporting less than it must is not one to believe. */
    GLint components = 0;
    glGetIntegerv(GL_MAX_FRAGMENT_UNIFORM_COMPONENTS, &components);
    G.nlayers = (components - SMEAR_SPARE_COMPONENTS) / SMEAR_LAYER_COMPONENTS;
    if (G.nlayers > MAX_LAYERS) G.nlayers = MAX_LAYERS;
    if (G.nlayers < 16) G.nlayers = 16;

    char frag[16384];
    snprintf(frag, sizeof frag, "#version 330 core\n#define NL %d\n%s",
             G.nlayers, FRAG_BODY);
    GLuint vs = compile(GL_VERTEX_SHADER, VERT);
    GLuint fs = vs ? compile(GL_FRAGMENT_SHADER, frag) : 0;
    if (!vs || !fs) goto fail;      /* compile() left the log in G.err */
    G.prog = glCreateProgram();
    glAttachShader(G.prog, vs);
    glAttachShader(G.prog, fs);
    glLinkProgram(G.prog);
    GLint ok = 0;
    glGetProgramiv(G.prog, GL_LINK_STATUS, &ok);
    glDeleteShader(vs);
    glDeleteShader(fs);
    if (!ok) {
        GLsizei ln = 0;
        glGetProgramInfoLog(G.prog, (GLsizei)sizeof G.err - 32, &ln, G.err);
        goto fail;
    }
    glGenVertexArrays(1, &G.vao);
    glGenFramebuffers(1, &G.fbo);
    glGenTextures(1, &G.tex);
    G.ready = 1;
    /* Let the context go.  An EGL context is current on at most one
     * thread, and the player thread binds it for every frame.  A
     * context still held here fails its first eglMakeCurrent with
     * EGL_BAD_ACCESS and the trail silently falls back. */
    release();
    return 0;

fail:
    if (err) snprintf(err, errlen, "%s", G.err[0] ? G.err : "GL unavailable");
    return 1;
}

/* Unbind the context so another thread may take it.  See the note in
 * smear_gl_init: this is what lets the player thread draw at all. */
static void release(void)
{
    eglMakeCurrent(G.dpy, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
}

static int ensure_target(int w, int h)
{
    if (w > G.tw || h > G.th) {
        G.tw = w > G.tw ? w : G.tw;
        G.th = h > G.th ? h : G.th;
        glBindTexture(GL_TEXTURE_2D, G.tex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, G.tw, G.th, 0,
                     GL_RGBA, GL_UNSIGNED_BYTE, NULL);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glBindFramebuffer(GL_FRAMEBUFFER, G.fbo);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                               GL_TEXTURE_2D, G.tex, 0);
        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
            snprintf(G.err, sizeof G.err, "framebuffer incomplete");
            return 1;
        }
    }
    int need = w * h * 4;
    if (need > G.pixcap) {
        unsigned char *p = realloc(G.pixels, need);
        if (!p) { snprintf(G.err, sizeof G.err, "out of memory"); return 1; }
        G.pixels = p;
        G.pixcap = need;
    }
    return 0;
}

#define U(name) glGetUniformLocation(G.prog, name)

Pixmap smear_gl_render(Display *d, Drawable root, unsigned depth,
                       const SmearPaint *layers, int n,
                       int bx, int by, int bw, int bh)
{
    if (!G.ready || n < 1 || bw < 1 || bh < 1) return 0;
    if (n > G.nlayers) n = G.nlayers;
    if (!eglMakeCurrent(G.dpy, EGL_NO_SURFACE, EGL_NO_SURFACE, G.ctx)) return 0;
    if (ensure_target(bw, bh)) { release(); return 0; }

    GLfloat quad[MAX_LAYERS * 4 * 2], head[MAX_LAYERS * 2];
    GLfloat colr[MAX_LAYERS * 3], grow[MAX_LAYERS], blur[MAX_LAYERS];
    GLfloat radius[MAX_LAYERS], sat[MAX_LAYERS * 4], sa[MAX_LAYERS * 4];
    GLint kind[MAX_LAYERS], nst[MAX_LAYERS];
    for (int i = 0; i < n; i++) {
        const SmearPaint *p = &layers[i];
        for (int c = 0; c < 4; c++) {
            quad[i * 8 + c * 2]     = (GLfloat)p->corners[c * 2];
            quad[i * 8 + c * 2 + 1] = (GLfloat)p->corners[c * 2 + 1];
        }
        head[i * 2]     = (GLfloat)p->head[0];
        head[i * 2 + 1] = (GLfloat)p->head[1];
        colr[i * 3] = (GLfloat)p->r;
        colr[i * 3 + 1] = (GLfloat)p->g;
        colr[i * 3 + 2] = (GLfloat)p->b;
        kind[i]   = p->kind == SMEAR_KIND_RADIAL ? 1 : 0;
        grow[i]   = (GLfloat)p->grow;
        blur[i]   = (GLfloat)p->blur;
        radius[i] = (GLfloat)p->radius;
        int ns = p->nstops < 2 ? 2 : (p->nstops > 4 ? 4 : p->nstops);
        nst[i] = ns;
        for (int s = 0; s < 4; s++) {
            int k = s < ns ? s : ns - 1;
            sat[i * 4 + s] = (GLfloat)p->stop_at[k];
            sa[i * 4 + s]  = (GLfloat)p->stop_alpha[k];
        }
    }

    glBindFramebuffer(GL_FRAMEBUFFER, G.fbo);
    glViewport(0, 0, bw, bh);
    glDisable(GL_BLEND);
    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);
    glUseProgram(G.prog);
    glUniform2f(U("iOrigin"), (GLfloat)bx, (GLfloat)by);
    glUniform1i(U("iCount"), n);
    glUniform2fv(U("iQuad"), n * 4, quad);
    glUniform2fv(U("iHead"), n, head);
    glUniform3fv(U("iColor"), n, colr);
    glUniform1iv(U("iKind"), n, kind);
    glUniform1fv(U("iGrow"), n, grow);
    glUniform1fv(U("iBlur"), n, blur);
    glUniform1fv(U("iRadius"), n, radius);
    glUniform1iv(U("iStops"), n, nst);
    glUniform4fv(U("iStopAt"), n, sat);
    glUniform4fv(U("iStopA"), n, sa);
    glBindVertexArray(G.vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);

    /* Read back and hand to X.  This is the cost that decides where
     * this renderer belongs: a trail-sized image a frame is nothing
     * across a local socket and far too much across a forwarded link. */
    glPixelStorei(GL_PACK_ALIGNMENT, 4);
    glReadPixels(0, 0, bw, bh, GL_BGRA, GL_UNSIGNED_BYTE, G.pixels);

    /* No vertical flip is needed.  gl_FragCoord counts up from the bottom
     * while X counts down from the top.  glReadPixels returns the bottom
     * row first, and the first XImage row is the top row.  Memory row K
     * has gl_FragCoord.y = K, shaded at p.y = by + K and placed there by
     * XPutImage.  These two conventions cancel; a third flip would mirror
     * the trail vertically inside its box. */
    int stride = bw * 4;

    Pixmap pm = XCreatePixmap(d, root, bw, bh, 32);
    GC gc = XCreateGC(d, pm, 0, NULL);
    XImage *im = XCreateImage(d, NULL, 32, ZPixmap, 0, (char *)G.pixels,
                              bw, bh, 32, stride);
    if (!im) {
        XFreeGC(d, gc); XFreePixmap(d, pm);
        release();
        return 0;
    }
    im->byte_order = LSBFirst;
    XPutImage(d, pm, gc, im, 0, 0, 0, 0, bw, bh);
    im->data = NULL;                /* the buffer is ours, not XImage's */
    XDestroyImage(im);
    XFreeGC(d, gc);
    (void)depth;
    release();
    return pm;
}

int smear_gl_draw(Display *d, Drawable root, Picture dst, unsigned depth,
                  const SmearPaint *layers, int n,
                  int bx, int by, int bw, int bh, double seconds)
{
    (void)seconds;
    Pixmap pm = smear_gl_render(d, root, depth, layers, n, bx, by, bw, bh);
    if (!pm) return 0;
    XRenderPictFormat *argb = XRenderFindStandardFormat(d, PictStandardARGB32);
    Picture src = XRenderCreatePicture(d, pm, argb, 0, NULL);
    XRenderComposite(d, PictOpOver, src, None, dst, 0, 0, 0, 0, bx, by, bw, bh);
    XRenderFreePicture(d, src);
    XFreePixmap(d, pm);
    return 1;
}

void smear_gl_shutdown(void)
{
    if (!G.ready) return;
    eglMakeCurrent(G.dpy, EGL_NO_SURFACE, EGL_NO_SURFACE, G.ctx);
    if (G.tex) glDeleteTextures(1, &G.tex);
    if (G.fbo) glDeleteFramebuffers(1, &G.fbo);
    if (G.vao) glDeleteVertexArrays(1, &G.vao);
    if (G.prog) glDeleteProgram(G.prog);
    eglDestroyContext(G.dpy, G.ctx);
    free(G.pixels);
    memset(&G, 0, sizeof G);
}
