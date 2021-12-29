#define _GNU_SOURCE
#include <getopt.h>
#include <ctype.h>
#include <errno.h>
#include <fcft/fcft.h>
#include <fcntl.h>
#include <limits.h>
#include <pixman.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/select.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <linux/input.h>
#include <libguile.h>
#include "utf8.h"
#include "wlr-layer-shell-unstable-v1-protocol.h"
#include "dscm-unstable-v1-protocol.h"
#include "xdg-shell-protocol.h"

/* macros */
#define BARF(fmt, ...)		do { fprintf(stderr, fmt "\n", ##__VA_ARGS__); exit(EXIT_FAILURE); } while (0)
#define EBARF(fmt, ...)		BARF(fmt ": %s", ##__VA_ARGS__, strerror(errno))
#define MIN(a, b)               ((a) < (b) ? (a) : (b))
#define MAX(a, b)               ((a) > (b) ? (a) : (b))

#define PROGRAM "dtao"
#define VERSION "0.1"
#define COPYRIGHT "copyright 2021 Devin J. Pohly and dtao team"
#define USAGE \
        "usage: dtao [-v] [-ta <l|c|r>] [-sa <l|c|r>] [-h <pixel>]\n" \
        "            [-e <string>] [-fn <font>] [-bg <color>] [-fg <color>]\n" \
        "            [-a] [-z [-z]] [-xs <screen>]"

/* Includes the newline character */
#define MAX_LINE_LEN 8192
#define MAX_BLOCK_LEN 128

/* enums */
enum align { ALIGN_UNSET = 0, ALIGN_L = 1, ALIGN_C = 2, ALIGN_R = 4 };

typedef struct {
        uint32_t fromx;
        uint32_t tox;
        uint32_t fromy;
        uint32_t toy;
        uint32_t button;
} ClickableArea;

typedef struct {
        struct wl_list link;
        struct wl_output *wl_output;
        struct wl_surface *wl_surface;
        struct wl_buffer *wl_buffer;
        struct zwlr_layer_surface_v1 *layer_surface;
        struct dscm_monitor_v1 *dscm;
        pixman_image_t *leftlayer, *centerlayer, *rightlayer;
        uint32_t name, width, stride, bufsize, layout, activetags,
                centerxdraw, rightxdraw;
        char title[MAX_BLOCK_LEN];
} Monitor;

typedef struct {
        enum align align;
        struct wl_list clink;
        scm_t_bits *render;
        scm_t_bits *click;
        char prevtext[MAX_BLOCK_LEN], text[MAX_BLOCK_LEN];
        uint32_t signal, interval, events;
        size_t length;
        ClickableArea ca;
} Block;

/* variables */
static struct fcft_font *font;
static struct wl_display *display;
static struct wl_compositor *compositor;
static struct wl_seat *seat = NULL;
static struct wl_shm *shm;
static struct zwlr_layer_shell_v1 *layer_shell;
static struct dscm_v1 *dscm;
static struct wl_surface *activesurface;
static struct wl_list monitors, cas;

static Monitor *selmon;
static char *namespace = "dtao";
static bool running = true;
static uint32_t savedx = 0, mousex = 0, mousey = 0,
        unhandled = 0, TAGMASK = 0;
static SCM tags, layouts;

/* function declarations */
static int allocate_shm_file(size_t size);
static void createmon(struct wl_output *output, uint32_t name);
static void destroymon(Monitor *m);
static struct wl_buffer *draw_frame(Monitor *m, enum align a);
static void drawbar(Monitor *m, enum align a);
static void drawbars(enum align a);
static void drawbg(Monitor *m, pixman_image_t *dest, pixman_color_t *bg,
        uint32_t x1, uint32_t x2, uint32_t y1, uint32_t y2);
static void drawtext(Monitor *m, enum align align);
static void event_loop(void);
static char *handle_cmd(char *cmd, Monitor *m, pixman_color_t *bg,
        pixman_color_t *fg, uint32_t *xpos, uint32_t *ypos);
static void handle_global(void *data, struct wl_registry *registry,
        uint32_t name, const char *interface, uint32_t version);
static void handle_global_remove(void *data, struct wl_registry *registry,
        uint32_t name);
static void layer_surface_configure(void *data, struct zwlr_layer_surface_v1 *surface,
        uint32_t serial, uint32_t w, uint32_t h);
static void layer_surface_closed(void *data, struct zwlr_layer_surface_v1 *surface);
static int parse_color(const char *str, pixman_color_t *clr);
static int parse_movement(char *str, Monitor *m, uint32_t *xpos,
        uint32_t *ypos, uint32_t xoffset, uint32_t yoffset);
static int parse_movement_arg(const char *str, uint32_t max);
static void pointer_handle_axis(void *data, struct wl_pointer *wl_pointer,
        uint32_t time, uint32_t axis, wl_fixed_t value);
static void pointer_handle_button(void *data, struct wl_pointer *wl_pointer,
        uint32_t serial, uint32_t time, uint32_t button, uint32_t state);
static void pointer_handle_enter(void *data, struct wl_pointer *pointer,
        uint32_t serial, struct wl_surface *surface, wl_fixed_t sx, wl_fixed_t sy);
static void pointer_handle_leave(void *data, struct wl_pointer *pointer,
        uint32_t serial, struct wl_surface *surface);
static void pointer_handle_motion(void *data, struct wl_pointer *pointer,
        uint32_t time, wl_fixed_t sx, wl_fixed_t sy);
static void seat_handle_capabilities(void *data, struct wl_seat *seat,
        enum wl_seat_capability caps);
static void setupmon(Monitor *m);
static int updateblock(Block *b);
static int updateblocks(unsigned int iteration, Block *blocks);
static void updatestatus(unsigned int iteration);
static void wl_buffer_release(void *data, struct wl_buffer *wl_buffer);

/* dscm protocol */
static void dscm_colorscheme(void *data, struct dscm_v1 *d, const char *root,
        const char *border, const char *focus, const char *text);
static void dscm_layout(void *data, struct dscm_v1 *d, const char *name);
static void dscm_tag(void *data, struct dscm_v1 *d, const char *name);
static void dscm_monitor_frame(void *data, struct dscm_monitor_v1 *mon);
static void dscm_monitor_layout(void *data, struct dscm_monitor_v1 *mon,
        uint32_t index);
static void dscm_monitor_tag(void *data, struct dscm_monitor_v1 *mon,
        uint32_t index, enum dscm_monitor_v1_tag_state state,
        uint32_t numclients, uint32_t focusedclient);
static void dscm_monitor_title(void *data, struct dscm_monitor_v1 *mon, char *title);
static void dscm_monitor_selected(void *data, struct dscm_monitor_v1 *mon,
        uint32_t selected);

/* global listeners */
static const struct wl_registry_listener registry_listener = {
        .global = handle_global,
        .global_remove = handle_global_remove,
};
static const struct wl_buffer_listener wl_buffer_listener = {
        .release = wl_buffer_release,
};
static struct zwlr_layer_surface_v1_listener layer_surface_listener = {
        .configure = layer_surface_configure,
        .closed = layer_surface_closed,
};
static const struct wl_seat_listener seat_listener = {
        seat_handle_capabilities,
};
static const struct wl_pointer_listener pointer_listener = {
        pointer_handle_enter,
        pointer_handle_leave,
        pointer_handle_motion,
        pointer_handle_button,
        pointer_handle_axis,
};

/* dscm protocol */
static const struct dscm_v1_listener dscm_listener = {
        .tag = dscm_tag,
        .layout = dscm_layout,
        .colorscheme = dscm_colorscheme,
};
static const struct dscm_monitor_v1_listener dscm_monitor_listener = {
        .tag = dscm_monitor_tag,
        .layout = dscm_monitor_layout,
        .selected = dscm_monitor_selected,
        .title = dscm_monitor_title,
        .frame = dscm_monitor_frame,
};

/* include guile config parameters */
#include "dscm-utils.h"
#include "dscm-bindings.h"
#include "dscm-config.h"

/* function implementations */
int
allocate_shm_file(size_t size)
{
        /* Shared memory support function adapted from [wayland-book] */
        int fd = memfd_create("surface", MFD_CLOEXEC);
        if (fd < 0)
                return -1;
        int ret;
        do {
                ret = ftruncate(fd, size);
        } while (ret < 0 && errno == EINTR);
        if (ret < 0) {
                close(fd);
                return -1;
        }
        return fd;
}

void
createmon(struct wl_output *output, uint32_t name)
{
        Monitor *m;
        m = calloc(1, sizeof(Monitor));
        m->wl_output = output;
        m->name = name;
        if (selmon == NULL)
                selmon = m;
        setupmon(m);
}

void
drawbar(Monitor *m, enum align a)
{
        m->wl_buffer = draw_frame(m, a);
        if (!m->wl_buffer)
                return;
        wl_surface_attach(m->wl_surface, m->wl_buffer, 0, 0);
        wl_surface_damage_buffer(m->wl_surface, 0, 0, m->width, height);
        wl_surface_commit(m->wl_surface);
}

void
drawbars(enum align a)
{
        Monitor *m;
        wl_list_for_each(m, &monitors, link)
                drawbar(m, a);
}

void
destroymon(Monitor *m)
{
        pixman_image_unref(m->leftlayer);
        pixman_image_unref(m->centerlayer);
        pixman_image_unref(m->rightlayer);
        dscm_monitor_v1_destroy(m->dscm);
        zwlr_layer_surface_v1_destroy(m->layer_surface);
        wl_surface_destroy(m->wl_surface);
        wl_list_remove(&m->link);
}

void
drawbg(Monitor *m, pixman_image_t *dest, pixman_color_t *bg,
       uint32_t x1, uint32_t x2, uint32_t y1, uint32_t y2)
{
        if (x1 >= m->width || bg->alpha == 0x0000)
                return;
        pixman_image_fill_boxes(PIXMAN_OP_OVER, dest, bg, 1,
                &(pixman_box32_t){
                        .x1 = x1,
                        .x2 = MIN(x2, m->width),
                        .y1 = y1,
                        .y2 = y2,
                });
}

void
drawtext(Monitor *m, enum align align)
{
        Block *b, *blocks;
        char *p, *start, *end;
        int drawdelim = 0, drawstop = 0;
        pixman_color_t textbgcolor, textfgcolor;
        pixman_image_t *fgfill, *bglayer, *fglayer, *dest;
        uint32_t yoffset, heightoffset, codepoint, ypos, xdraw, space,
                xpos = 0, lastcp = 0, state = UTF8_ACCEPT;

        /* Render the appropriate blocks based on alignment. */
        if (align == ALIGN_L) {
                dest = m->leftlayer;
                blocks = leftblocks;
        } else if (align == ALIGN_C) {
                dest = m->centerlayer;
                blocks = centerblocks;
        } else if (align == ALIGN_R) {
                dest = m->rightlayer;
                blocks = rightblocks;
        }

        /* Can this ever be NULL? */
        if (!blocks)
                return;

        /* Colors (premultiplied!) */
        textbgcolor = bgcolor;
        textfgcolor = fgcolor;

        fgfill = pixman_image_create_solid_fill(&textfgcolor);
        bglayer = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                        m->width, height, NULL, m->width * 4);
        fglayer = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                        m->width, height, NULL, m->width * 4);

        yoffset = isbottom ? borderpx : 0;
        heightoffset = isbottom ? 0 : borderpx;
        ypos = (height + heightoffset + font->ascent - font->descent) / 2;

        for (b = blocks; b->render && !drawstop;) {
                if (drawdelim) {
                        start = delimiter;
                        end = delimiterend;
                } else {
                        b->ca.fromx = xpos;
                        start = b->text;
                        end = start + b->length;
                }
                for (p = start; p != end; p++) {
                        /* Stop drawing when exceeding the monitor width. */
                        if (xpos >= m->width) {
                                drawstop = 1;
                                break;
                        }

                        /* Check for inline ^ commands */
                        if (!drawdelim && state == UTF8_ACCEPT && *p == '^') {
                                p++;
                                if (*p != '^') {
                                        p = handle_cmd(p, m, &textbgcolor, &textfgcolor, &xpos, &ypos);
                                        pixman_image_unref(fgfill);
                                        fgfill = pixman_image_create_solid_fill(&textfgcolor);
                                        continue;
                                }
                        }
                        /* Returns nonzero if more bytes are needed */
                        if (utf8decode(&state, &codepoint, *p))
                                continue;

                        /* Turn off subpixel rendering, which complicates things when
                        * mixed with alpha channels */
                        const struct fcft_glyph *glyph = fcft_glyph_rasterize(font, codepoint,
                                        FCFT_SUBPIXEL_NONE);
                        if (!glyph)
                                continue;

                        /* Adjust x position based on kerning with previous glyph */
                        long x_kern = 0;
                        if (lastcp)
                                fcft_kerning(font, lastcp, codepoint, &x_kern, NULL);
                        xpos += x_kern;
                        lastcp = codepoint;

                        /* Detect and handle pre-rendered glyphs (e.g. emoji) */
                        if (pixman_image_get_format(glyph->pix) == PIXMAN_a8r8g8b8) {
                                /* Only the alpha channel of the mask is used, so we can
                                * use fgfill here to blend prerendered glyphs with the
                                * same opacity */
                                pixman_image_composite32(
                                        PIXMAN_OP_OVER, glyph->pix, fgfill, fglayer, 0, 0, 0, 0,
                                        xpos + glyph->x, ypos - glyph->y, glyph->width, glyph->height);
                        } else {
                                /* Applying the foreground color here would mess up
                                * component alphas for subpixel-rendered text, so we
                                * apply it when blending. */
                                pixman_image_composite32(
                                        PIXMAN_OP_OVER, fgfill, glyph->pix, fglayer, 0, 0, 0, 0,
                                        xpos + glyph->x, ypos - glyph->y, glyph->width, glyph->height);
                        }
                        drawbg(m, bglayer, &textbgcolor, xpos, xpos + glyph->advance.x, yoffset, height - yoffset);
                        xpos += glyph->advance.x;
                        ypos += glyph->advance.y;
                }
                space = 0;
                if (drawdelim) {
                        drawdelim = 0;
                        space += spacing;
                } else {
                        b->ca.tox = xpos;
                        b++;
                        if (b->render != NULL) {
                                space += spacing;
                                if (delimiter)
                                        drawdelim = 1;
                                else
                                        space += spacing;
                        }
                }
                /* Draw spacing between blocks. Since the underlying graphics buffer is being reused,
                 * the spacing must overwrite any old data at the corresponding xpos. */
                if (space > 0) {
                        drawbg(m, bglayer, &textbgcolor, xpos, xpos + space, yoffset, height - yoffset);
                        xpos += space;
                }
        }
        pixman_image_unref(fgfill);

        if (state != UTF8_ACCEPT)
                fprintf(stderr, "malformed UTF-8 sequence\n");

        if (align == ALIGN_L)
                xdraw = 0;
        else if (align == ALIGN_C)
                m->centerxdraw = xdraw = (m->width - xpos) / 2;
        else if (align == ALIGN_R)
                m->rightxdraw = xdraw = m->width - xpos;

        pixman_image_composite32(PIXMAN_OP_OVER, bglayer, NULL, dest, 0, 0, 0, 0,
                        xdraw, 0, xpos, height);
        pixman_image_composite32(PIXMAN_OP_OVER, fglayer, NULL, dest, 0, 0, 0, 0,
                        xdraw, 0, xpos, height);

        pixman_image_unref(bglayer);
        pixman_image_unref(fglayer);
}

struct wl_buffer *
draw_frame(Monitor *m, enum align a)
{
        /* Allocate buffer to be attached to the surface */
        int fd = allocate_shm_file(m->bufsize);
        if (fd == -1 || !m || m->bufsize == 0)
                return NULL;

        uint32_t *data = mmap(NULL, m->bufsize,
                        PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
        if (data == MAP_FAILED) {
                close(fd);
                printf("could not allocate: %d, %d\n", errno, m->bufsize);
                return NULL;
        }

        struct wl_shm_pool *pool = wl_shm_create_pool(shm, fd, m->bufsize);
        struct wl_buffer *buffer = wl_shm_pool_create_buffer(pool, 0,
                        m->width, height, m->stride, WL_SHM_FORMAT_ARGB8888);
        wl_buffer_add_listener(buffer, &wl_buffer_listener, NULL);
        wl_shm_pool_destroy(pool);
        close(fd);

        /* Pixman image corresponding to main buffer */
        pixman_image_t *bar = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                        m->width, height, data, m->stride);

        /* How should this work when there is three different "bars"? */
        if (!adjustwidth)
                pixman_image_fill_boxes(PIXMAN_OP_SRC, bar, &bgcolor, 1,
                        &(pixman_box32_t) {.x1 = 0, .x2 = m->width, .y1 = 0, .y2 = height});
        if (a & ALIGN_L)
                drawtext(m, ALIGN_L);
        if (a & ALIGN_C)
                drawtext(m, ALIGN_C);
        if (a & ALIGN_R) {
                drawtext(m, ALIGN_R);
        }
        /* How should we handle overflows? Configurable render order? */
        pixman_image_composite32(PIXMAN_OP_OVER, m->leftlayer, NULL, bar, 0, 0, 0, 0,
                        0, 0, m->width, height);
        pixman_image_composite32(PIXMAN_OP_OVER, m->rightlayer, NULL, bar, 0, 0, 0, 0,
                        0, 0, m->width, height);
        pixman_image_composite32(PIXMAN_OP_OVER, m->centerlayer, NULL, bar, 0, 0, 0, 0,
                        0, 0, m->width, height);
        pixman_image_unref(bar);
        munmap(data, m->bufsize);
        return buffer;
}

void
event_loop(void)
{
        char dummy[8];
        struct itimerspec spec;
        int ret, tfd, wlfd = wl_display_get_fd(display);

        spec.it_interval = (struct timespec){.tv_sec = updateinterval, .tv_nsec = 0};
        spec.it_value = (struct timespec){.tv_sec = 1, .tv_nsec = 0};
        if ((tfd = timerfd_create(CLOCK_REALTIME,  0)) < 0)
                EBARF("timerfd_create");
        if (timerfd_settime(tfd, 0, &spec, NULL) < 0)
                EBARF("timerfd_settime");

        /* initial draw */
        updatestatus(0);

        for (unsigned int i = 1; running; i++) {
                fd_set rfds;
                FD_ZERO(&rfds);
                FD_SET(wlfd, &rfds);
                FD_SET(tfd, &rfds);

                /* Does this need to be inside the loop? */
                wl_display_flush(display);

                ret = select(MAX(wlfd, tfd) + 1, &rfds, NULL, NULL, NULL);
                if (ret < 0)
                        EBARF("select");

                if (FD_ISSET(tfd, &rfds)) {
                        updatestatus(i);
                        read(tfd, dummy, 8);
                }

                if (FD_ISSET(wlfd, &rfds))
                        if (wl_display_dispatch(display) == -1)
                                break;
        }
}

char *
handle_cmd(char *cmd, Monitor *m, pixman_color_t *bg, pixman_color_t *fg,
        uint32_t *xpos, uint32_t *ypos)
{
        char *arg, *end;

        if (!(arg = strchr(cmd, '(')) || !(end = strchr(arg + 1, ')')))
                return cmd;

        *arg++ = '\0';
        *end = '\0';

        if (!strcmp(cmd, "bg")) {
                if (!*arg)
                        *bg = bgcolor;
                else if (parse_color(arg, bg))
                        fprintf(stderr, "Bad color string \"%s\"\n", arg);
        } else if (!strcmp(cmd, "fg")) {
                if (!*arg)
                        *fg = fgcolor;
                else if (parse_color(arg, fg))
                        fprintf(stderr, "Bad color string \"%s\"\n", arg);
        } else if (!strcmp(cmd, "pa")) {
                if (parse_movement(arg, m, xpos, ypos, 0, 0))
                        fprintf(stderr, "Invalid absolute position argument \"%s\"\n", arg);
        } else if (!strcmp(cmd, "p")) {
                if (parse_movement(arg, m, xpos, ypos, *xpos, *ypos))
                        fprintf(stderr, "Invalid relative position argument \"%s\"\n", arg);
        } else if (!strcmp(cmd, "sx")) {
                savedx = *xpos;
        } else if (!strcmp(cmd, "rx")) {
                *xpos = savedx;
        } else {
                fprintf(stderr, "Unrecognized command \"%s\"\n", cmd);
        }

        /* Restore string for later redraws */
        *--arg = '(';
        *end = ')';
        return end;
}

void
handle_global(void *data, struct wl_registry *registry,
                uint32_t name, const char *interface, uint32_t version)
{
        if (strcmp(interface, wl_compositor_interface.name) == 0) {
                compositor = wl_registry_bind(registry, name,
                                &wl_compositor_interface, 4);
        } else if (strcmp(interface, wl_shm_interface.name) == 0) {
                shm = wl_registry_bind(registry, name, &wl_shm_interface, 1);
        } else if (strcmp(interface, wl_output_interface.name) == 0) {
                struct wl_output *o = wl_registry_bind(registry, name,
                                &wl_output_interface, 1);
                createmon(o, name);
        } else if (strcmp(interface, zwlr_layer_shell_v1_interface.name) == 0) {
                layer_shell = wl_registry_bind(registry, name,
                                &zwlr_layer_shell_v1_interface, 1);
        } else if (strcmp(interface, dscm_v1_interface.name) == 0) {
                dscm = wl_registry_bind(registry, name,
                                &dscm_v1_interface, 1);
                dscm_v1_add_listener(dscm, &dscm_listener, NULL);
        } else if (strcmp(interface, wl_seat_interface.name) == 0) {
                seat = wl_registry_bind(registry, name,
                                &wl_seat_interface, 1);
                wl_seat_add_listener(seat, &seat_listener, NULL);
        }
}

void
handle_global_remove(void *data, struct wl_registry *registry, uint32_t name)
{

        Monitor *m;
        wl_list_for_each(m, &monitors, link)
                if (m->name == name)
                        destroymon(m);
}

void
layer_surface_configure(void *data,
                struct zwlr_layer_surface_v1 *surface,
                uint32_t serial, uint32_t w, uint32_t h)
{
        /* Layer-surface setup adapted from layer-shell example in [wlroots] */
        Monitor *m = data;

        if (!m || !m->wl_surface || !m->layer_surface)
                BARF("failed to configure layer surface, no monitor defined.");

        m->width = w;
        m->stride = m->width * 4;
        m->bufsize = m->stride * height;
        m->leftlayer = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                                m->width, height, NULL, m->stride);
        m->centerlayer = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                                m->width, height, NULL, m->stride);
        m->rightlayer = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                                m->width, height, NULL, m->stride);
        if (exclusive > 0)
                exclusive = height;
        zwlr_layer_surface_v1_set_exclusive_zone(m->layer_surface, exclusive);
        zwlr_layer_surface_v1_ack_configure(surface, serial);
        drawbar(m, ALIGN_L | ALIGN_C | ALIGN_R);
}

void
layer_surface_closed(void *data, struct zwlr_layer_surface_v1 *surface)
{
        zwlr_layer_surface_v1_destroy(surface);
        wl_surface_destroy(((Monitor*)data)->wl_surface);
        /* TODO: Will this cause dtao to quit if disconnecting a monitor? */
        running = false;
}

int
parse_color(const char *str, pixman_color_t *clr)
{
        /* Color parsing logic adapted from [sway] */
        if (*str == '#')
                str++;
        int len = strlen(str);

        // Disallows "0x" prefix that strtoul would ignore
        if ((len != 6 && len != 8) || !isxdigit(str[0]) || !isxdigit(str[1]))
                return 1;

        char *ptr;
        uint32_t parsed = strtoul(str, &ptr, 16);
        if (*ptr)
                return 1;

        if (len == 8) {
                clr->alpha = (parsed & 0xff) * 0x101;
                parsed >>= 8;
        } else {
                clr->alpha = 0xffff;
        }
        clr->red =   ((parsed >> 16) & 0xff) * 0x101;
        clr->green = ((parsed >>  8) & 0xff) * 0x101;
        clr->blue =  ((parsed >>  0) & 0xff) * 0x101;
        return 0;
}

int
parse_movement(char *str, Monitor *m, uint32_t *xpos, uint32_t *ypos, uint32_t xoffset, uint32_t yoffset)
{
        char *xarg = str;
        char *yarg;

        if (!*str) {
                *ypos = (height + font->ascent - font->descent) / 2;
        } else if (!(yarg = strchr(str, ';'))) {
                if (*str == '_') {
                        if (!strcmp(str, "_LEFT")) {
                                *xpos = 0;
                        } else if (!strcmp(str, "_RIGHT")) {
                                *xpos = m->width;
                        } else if (!strcmp(str, "_CENTER")) {
                                *xpos = m->width / 2;
                        } else if (!strcmp(str, "_TOP")) {
                                *ypos = 0;
                        } else if (!strcmp(str, "_BOTTOM")) {
                                *ypos = height;
                        } else {
                                return 1;
                        }
                } else {
                        *xpos = parse_movement_arg (str, m->width) + xoffset;
                }
        } else if (*str == ';') {
                *ypos = parse_movement_arg (++str, height) + yoffset;
        } else {
                *yarg++ = '\0';
                *ypos = parse_movement_arg (yarg, height) + yoffset;
                *xpos = parse_movement_arg (xarg, m->width) + xoffset;
                *--yarg = ';';
        }

        if (*xpos > m->width)
                *xpos = m->width;

        if (*ypos > height)
                *ypos = height;

        return 0;

}

int
parse_movement_arg(const char *str, uint32_t max)
{
        if (!str)
                return 0;

        if (*str == '-')
                return -(parse_movement_arg (++str, max));

        if (*str == 'w')
                return atoi(++str) * max / 100;

        if (*str == 'd')
                return atoi(++str) * font->descent / 100;

        if (*str == 'a')
                return atoi(++str) * font->ascent / 100;

        return atoi(str);
}

void
pointer_handle_axis(void *data, struct wl_pointer *wl_pointer,
                uint32_t time, uint32_t axis, wl_fixed_t value)
{
}

void
pointer_handle_button(void *data, struct wl_pointer *wl_pointer,
                uint32_t serial, uint32_t time, uint32_t button,
                uint32_t state)
{
        if (!activesurface || !state)
                return;

        Block *b;
        uint32_t xoffset;
        Monitor *m, *sel = NULL;

        wl_list_for_each(m, &monitors, link)
                if (m->wl_surface == activesurface)
                        sel = m;
        if (!sel)
                return;
        wl_list_for_each(b, &cas, clink) {
                if (b->align == ALIGN_L)
                        xoffset = 0;
                else if (b->align == ALIGN_C)
                        xoffset = sel->centerxdraw;
                else if (b->align == ALIGN_R)
                        xoffset = sel->rightxdraw;
                if (b->ca.button == button - BTN_MOUSE &&
                            (b->ca.fromx + xoffset) <= mousex &&
                            (b->ca.tox + xoffset) >= mousex &&
                            b->ca.fromy <= mousey && mousey <= b->ca.toy) {
                        dscm_safe_call(b->click, NULL);
                        break;
                }
        }
}

void
pointer_handle_enter(void *data, struct wl_pointer *pointer,
                uint32_t serial, struct wl_surface *surface,
                wl_fixed_t sx, wl_fixed_t sy)
{
        activesurface = surface;
}

void
pointer_handle_leave(void *data, struct wl_pointer *pointer,
                uint32_t serial, struct wl_surface *surface)
{
        activesurface = NULL;
}

void
pointer_handle_motion(void *data, struct wl_pointer *pointer,
                uint32_t time, wl_fixed_t sx, wl_fixed_t sy)
{
        mousex = (uint32_t)wl_fixed_to_int(sx);
        mousey = (uint32_t)wl_fixed_to_int(sy);
}

void
seat_handle_capabilities(void *data, struct wl_seat *seat,
                enum wl_seat_capability caps)
{
        if (caps & WL_SEAT_CAPABILITY_POINTER) {
                struct wl_pointer *pointer = wl_seat_get_pointer(seat);
                wl_pointer_add_listener(pointer, &pointer_listener, NULL);
        }
}

void
setupmon(Monitor *m)
{
        /* Create layer-shell surface */
        m->wl_surface = wl_compositor_create_surface(compositor);
        if (!m->wl_surface)
                BARF("could not create wl_surface");

        m->layer_surface = zwlr_layer_shell_v1_get_layer_surface(layer_shell,
                        m->wl_surface, m->wl_output, layer, namespace);
        m->dscm = dscm_v1_get_monitor(dscm, m->wl_output);

        if (!m->layer_surface)
                BARF("could not create layer_surface");
        if (!m->dscm)
                BARF("could not create dscm monitor");

        wl_list_insert(&monitors, &m->link);
        zwlr_layer_surface_v1_add_listener(m->layer_surface,
                        &layer_surface_listener, m);
        dscm_monitor_v1_add_listener(m->dscm, &dscm_monitor_listener, m);
        zwlr_layer_surface_v1_set_size(m->layer_surface, m->width, height);
        zwlr_layer_surface_v1_set_anchor(m->layer_surface, anchor);
        wl_surface_commit(m->wl_surface);
        wl_display_roundtrip(display);
}

int
updateblock(Block *b)
{
        SCM ret = dscm_safe_call(b->render, selmon);
        if (!scm_is_string(ret))
                return 1;
        memcpy(b->prevtext, b->text, b->length);
        b->length = MIN(MAX_BLOCK_LEN, scm_to_locale_stringbuf(ret, b->text, MAX_BLOCK_LEN));
        return memcmp(b->prevtext, b->text, b->length);
}

int
updateblocks(unsigned int i, Block *blocks)
{
        Block *b;
        int dirty = 0;
        for (b = blocks; b->render; b++)
                if (i == 0 || (b->interval > 0 && i % b->interval == 0)
                           || (unhandled && b->events))
                        if (updateblock(b) != 0)
                                dirty = 1;
        return dirty;
}

/* TDOO: Status must be updated once for each monitor */
void
updatestatus(unsigned int i)
{
        enum align a = ALIGN_UNSET;
        if (updateblocks(i, leftblocks) || i == 0)
                a |= ALIGN_L;
        if (updateblocks(i, centerblocks) || i == 0)
                a |= ALIGN_C;
        if (updateblocks(i, rightblocks) || i == 0)
                a |= ALIGN_R;
        if (a != ALIGN_UNSET)
                drawbars(a);
        /* Mark all events has handled. */
        unhandled = 0;
}

void
wl_buffer_release(void *data, struct wl_buffer *wl_buffer)
{
        /* Sent by the compositor when it's no longer using this buffer */
        wl_buffer_destroy(wl_buffer);
}

/* dscm protocol implementation */
void
dscm_tag(void *data, struct dscm_v1 *d, const char *name)
{
        SCM new = scm_list_1(scm_from_locale_string(name));
        SCM args = scm_list_2(tags, new);
        tags = scm_append(args);
}

void
dscm_layout(void *data, struct dscm_v1 *d, const char *name)
{
        SCM new = scm_list_1(scm_from_locale_string(name));
        SCM args = scm_list_2(layouts, new);
        layouts = scm_append(args);
}

// TODO: Only add listener for colorscheme event if usewmcolorscheme == true
void
dscm_colorscheme(void *data, struct dscm_v1 *d, const char *root,
        const char *border, const char *focus, const char *text)
{
        if (!usewmcolorscheme)
                return;
        parse_color(root, &bgcolor);
        parse_color(text, &fgcolor);
        parse_color(border, &bordercolor);
        drawbars(ALIGN_L | ALIGN_C | ALIGN_R);
}

void
dscm_monitor_tag(void *data, struct dscm_monitor_v1 *mon, uint32_t index,
        enum dscm_monitor_v1_tag_state state, uint32_t numclients, uint32_t focusedclient)
{
        /* TODO: Save state, numclients */
        Monitor *m = data;
        if (numclients > 0)
                m->activetags |= ((1 << index) | TAGMASK);
        else
                m->activetags &= ~((1 << index) | TAGMASK);
}

void
dscm_monitor_layout(void *data, struct dscm_monitor_v1 *mon, uint32_t index)
{
        ((Monitor*)data)->layout = index;
}

void
dscm_monitor_selected(void *data, struct dscm_monitor_v1 *mon, uint32_t selected)
{
        if (selected > 0)
                selmon = (Monitor*)data;
}

void
dscm_monitor_title(void *data, struct dscm_monitor_v1 *mon, char *title)
{
        Monitor *m = data;
        strncpy(m->title, title, MAX_BLOCK_LEN - 1);
        /* Strings that does not fit inside the buffer will
         * cause the buffer to not be null-terminated. */
        m->title[MAX_BLOCK_LEN - 1] = '\0';
}

void
dscm_monitor_frame(void *data, struct dscm_monitor_v1 *mon)
{
        unhandled = 1;
        updatestatus(-1);
}

int
main(int argc, char **argv)
{
        int c;
        Block *b;
        Monitor *m;
        char *configfile = NULL;

        while ((c = getopt(argc, argv, "c:hv")) != -1) {
                if (c == 'c')
                        configfile = optarg;
                else if (c == 'v') {
                        fprintf(stderr, PROGRAM " " VERSION ", " COPYRIGHT "\n");
                        return 0;
                } else
                        goto usage;
        }
        if (optind < argc)
                goto usage;
        if (!configfile)
                BARF("error: config path must be set using '-c'");

        wl_list_init(&cas);
        wl_list_init(&monitors);

        /* Load guile config */
        scm_init_guile();
        dscm_register();
        dscm_config_parse(configfile);

        /* Initialize tags and layout lists */
        tags = scm_list_n(SCM_UNDEFINED);
        layouts = scm_list_n(SCM_UNDEFINED);

        /* Load selected font */
        fcft_set_scaling_filter(FCFT_SCALING_FILTER_LANCZOS3);
        font = fcft_from_name(1, (const char *[]) {fontstr}, NULL);
        if (!font)
                BARF("could not load font");

        /* Set layer size and positioning */
        if (!height)
                height = font->height + font->descent + borderpx;

        /* Register mouse clicks for all vertical space of bar */
        wl_list_for_each(b, &cas, clink)
                b->ca.toy = height;

        /* Set up display and protocols */
        display = wl_display_connect(NULL);
        if (!display)
                BARF("Failed to create display");

        struct wl_registry *registry = wl_display_get_registry(display);
        wl_registry_add_listener(registry, &registry_listener, NULL);
        wl_display_roundtrip(display);

        if (!compositor || !shm || !layer_shell || !dscm)
                BARF("compositor does not support all needed protocols");

        event_loop();

        /* Clean everything up */
        wl_list_for_each(m, &monitors, link)
                destroymon(m);

        dscm_config_cleanup();
        zwlr_layer_shell_v1_destroy(layer_shell);
        fcft_destroy(font);
        wl_shm_destroy(shm);
        wl_compositor_destroy(compositor);
        wl_registry_destroy(registry);
        wl_display_disconnect(display);

        return 0;
usage:
        BARF("Usage: %s [-c path to config.scm]", argv[0]);
}
