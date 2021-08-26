#define _GNU_SOURCE
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
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <linux/input.h>
#include "utf8.h"
#include "wlr-layer-shell-unstable-v1-protocol.h"
#include "xdg-shell-protocol.h"

#define BARF(fmt, ...)		do { fprintf(stderr, fmt "\n", ##__VA_ARGS__); exit(EXIT_FAILURE); } while (0)
#define EBARF(fmt, ...)		BARF(fmt ": %s", ##__VA_ARGS__, strerror(errno))
#define MIN(a, b)               ((a) < (b) ? (a) : (b))
#define MAX(a, b)               ((a) > (b) ? (a) : (b))

#define PROGRAM "dtao"
#define VERSION "0.1"
#define COPYRIGHT "copyright 2021 Devin J. Pohly and dtao team"
#define USAGE \
	"usage: dtao [-v] [-p seconds] [-m <v|h>] [-ta <l|c|r>] [-sa <l|c|r>]\n" \
	"            [-w <pixel>] [-h <pixel>] [-tw <pixel>] [-l <lines>] [-u]\n" \
	"            [-e <string>] [-fn <font>] [-bg <color>] [-fg <color>]\n" \
	"            [-expand <l|c|r>] [-z [-z]] [-xs <screen>]"

/* Includes the newline character */
#define MAX_LINE_LEN 8192
#define MAX_CLICKABLE_AREAS 64
#define MAX_CLICKABLE_AREA_CMD_LEN 128
#define MAX_OUTPUT_MONITORS 8

enum align { ALIGN_C, ALIGN_L, ALIGN_R };

typedef struct {
        uint32_t *data;
        uint32_t width, stride, bufsize;
        struct wl_output *wl_output;
        struct wl_surface *wl_surface;
        struct wl_buffer *wl_buffer;
        struct zwlr_layer_surface_v1 *layer_surface;
        pixman_image_t *bar;
} Monitor;

static struct wl_display *display;
static struct wl_compositor *compositor;
static struct wl_shm *shm;
static struct zwlr_layer_shell_v1 *layer_shell;

struct wl_seat *seat = NULL;

static struct wl_surface *active_wl_surface;
static Monitor monitors[MAX_OUTPUT_MONITORS];

static int32_t output = -1;

static Monitor *m;
static int lines;
static int persist;
static int nummons = 0;
static bool unified;
static int exclusive_zone = -1;
static enum align titlealign, subalign;
static bool expand;
static bool run_display = true;
static uint32_t height, titlewidth;
static uint32_t maxlayerwidth = 0;

static uint32_t savedx = 0;
static uint32_t mousex = 0;
static uint32_t mousey = 0;

static struct ca_entry {
	uint32_t from_x;
	uint32_t to_x;
	uint32_t from_y;
	uint32_t to_y;
	uint32_t mouse_button;
	char cmd[MAX_CLICKABLE_AREA_CMD_LEN];
} ca_entry_creating;

static uint32_t ca_entry_count = 0;
static struct ca_entry ca_entries[MAX_CLICKABLE_AREAS];

static bool ca_waiting = false;

static struct fcft_font *font;
static char line[MAX_LINE_LEN];
static char lastline[MAX_LINE_LEN];
static int linerem;
static bool eat_line = false;
static pixman_color_t
	bgcolor = {
		.red = 0x1111,
		.green = 0x1111,
		.blue = 0x1111,
		.alpha = 0xffff,
	},
	fgcolor = {
		.red = 0xb3b3,
		.green = 0xb3b3,
		.blue = 0xb3b3,
		.alpha = 0xffff,
	};

static void
wl_buffer_release(void *data, struct wl_buffer *wl_buffer)
{
	/* Sent by the compositor when it's no longer using this buffer */
	wl_buffer_destroy(wl_buffer);
}

static const struct wl_buffer_listener wl_buffer_listener = {
	.release = wl_buffer_release,
};

/* Shared memory support function adapted from [wayland-book] */
static int
allocate_shm_file(size_t size)
{
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

/* Color parsing logic adapted from [sway] */
static int
parse_color(const char *str, pixman_color_t *clr)
{
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

static int
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

static int
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

static int
parse_clickable_area(char *str, uint32_t *xpos, uint32_t *ypos)
{
	if (ca_waiting) {
		if(*str) {
			fprintf(stderr, "Second clickable area command must have 0 arguments\n");
			return 1;
		}

		ca_entry_creating.to_x = *xpos;
		ca_entry_creating.to_y = *ypos;

		if(ca_entry_creating.to_x < ca_entry_creating.from_x) {
			uint32_t temp = ca_entry_creating.to_x;
			ca_entry_creating.to_x = ca_entry_creating.from_x;
			ca_entry_creating.from_x = temp;
		}
		if(ca_entry_creating.to_y < ca_entry_creating.from_y) {
			uint32_t temp = ca_entry_creating.to_y;
			ca_entry_creating.to_y = ca_entry_creating.from_y;
			ca_entry_creating.from_y = temp;
		}

		ca_entry_creating.from_y -= font->ascent;

		memcpy(&ca_entries[ca_entry_count++], &ca_entry_creating, sizeof(struct ca_entry));

		ca_waiting = false;
	} else {
		if (!*str) {
			fprintf(stderr, "Second clickable area command must have 2 arguments (0 provided)\n");
			return 1;
		}
		int mouse_button = *str - '0';
		if(mouse_button < 0 || mouse_button > 9) {
			fprintf(stderr, "Invalid mouse button: %d", mouse_button);
			return 1;
		}
		str++;
		if (*str != ',') {
			fprintf(stderr, "First clickable area command must have 2 arguments (1 provided)\n");
			return 1;
		}
	    	char *cmd = ++str;
		if (strlen(cmd) > MAX_CLICKABLE_AREA_CMD_LEN) {
			fprintf(stderr, "Clickable area command exceeds maximum length (%d)\n", MAX_CLICKABLE_AREA_CMD_LEN);
			return 1;
		}

		ca_entry_creating.from_x = *xpos;
		ca_entry_creating.from_y = *ypos;
		ca_entry_creating.to_x = 0;
		ca_entry_creating.to_y = 0;

		strcpy(ca_entry_creating.cmd, cmd);
		ca_waiting = true;
	}

	return 0;
}

static char *
handle_cmd(char *cmd, Monitor *m, pixman_color_t *bg, pixman_color_t *fg,
        uint32_t *xpos, uint32_t *ypos, uint32_t *istw)
{
	char *arg, *end;

	if (!(arg = strchr(cmd, '(')) || !(end = strchr(arg + 1, ')')))
		return cmd;

	*arg++ = '\0';
	*end = '\0';

	if (!strcmp(cmd, "bg")) {
		if (!*arg) {
			*bg = bgcolor;
		} else if (parse_color(arg, bg)) {
			fprintf(stderr, "Bad color string \"%s\"\n", arg);
		}
	} else if (!strcmp(cmd, "fg")) {
		if (!*arg) {
			*fg = fgcolor;
		} else if (parse_color(arg, fg)) {
			fprintf(stderr, "Bad color string \"%s\"\n", arg);
		}
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
	} else if (!strcmp(cmd, "ca")) {
		if (parse_clickable_area(arg, xpos, ypos))
			fprintf(stderr, "Invalid clickable area command argument \"%s\"\n", arg);
        } else if (!strcmp(cmd, "tw")) {
                *istw = 1;
        } else if (!strcmp(cmd, "sw")) {
                *istw = 0;
	} else {
		fprintf(stderr, "Unrecognized command \"%s\"\n", cmd);
	}

	/* Restore string for later redraws */
	*--arg = '(';
	*end = ')';
	return end;
}

static void
draw_frame(char *text)
{
        /* Reset monitor buffers */
        for (int i = 0; i < nummons; i++)
                monitors[i].wl_buffer = NULL;

        /* Colors (premultiplied!) */
        pixman_color_t textbgcolor = bgcolor;
        pixman_color_t textfgcolor = fgcolor;

        for (int i = 0; i < nummons; i++) {
                m = &monitors[i];

                /* Allocate buffer to be attached to the surface */
                int fd = allocate_shm_file(m->bufsize);
                if (fd == -1)
		        return;
                m->data = mmap(NULL, m->bufsize,
                                PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
                if (m->data == MAP_FAILED) {
                        close(fd);
                        printf("could not allocate: %d, %d\n", errno, m->bufsize);
                        return;
                }

                struct wl_shm_pool *pool = wl_shm_create_pool(shm, fd, m->bufsize);
                m->wl_buffer = wl_shm_pool_create_buffer(pool, 0,
                        m->width, height, m->stride, WL_SHM_FORMAT_ARGB8888);
                wl_buffer_add_listener(m->wl_buffer, &wl_buffer_listener, NULL);
                wl_shm_pool_destroy(pool);
                close(fd);

                /* Pixman image corresponding to main buffer */
                m->bar = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                        m->width, height, m->data, m->width * 4);
                /* Fill bar with background color if bar should extend beyond text */
                if (!expand)
                        pixman_image_fill_boxes(PIXMAN_OP_SRC, m->bar, &bgcolor, 1,
                                        &(pixman_box32_t) {.x1 = 0, .x2 = m->width, .y1 = 0, .y2 = height});
        }

        /* Sub-window layers */
        pixman_image_t *swbg = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                        maxlayerwidth, height, NULL, maxlayerwidth * 4);
        pixman_image_t *swfg = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                        maxlayerwidth, height, NULL, maxlayerwidth * 4);

        /* Title window layers */
        pixman_image_t *twbg = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                        maxlayerwidth, height, NULL, maxlayerwidth * 4);
        pixman_image_t *twfg = pixman_image_create_bits(PIXMAN_a8r8g8b8,
                        maxlayerwidth, height, NULL, maxlayerwidth * 4);

	pixman_image_t *fgfill = pixman_image_create_solid_fill(&textfgcolor);

        uint32_t twxpos = 0, swxpos = 0, twypos, swypos;
	/* start drawing at center-left (ypos sets the text baseline) */
        twypos = swypos = (height + font->ascent - font->descent) / 2;

        /* Draw in sub-window layer by default */
	uint32_t *xpos = &swxpos;
	uint32_t *ypos = &swypos;

	ca_entry_count = 0;

        pixman_image_t *fglayer, *bglayer;
	uint32_t codepoint, lastcp = 0, state = UTF8_ACCEPT, istw = 0;
	for (char *p = text; *p; p++) {
		/* Check for inline ^ commands */
		if (state == UTF8_ACCEPT && *p == '^') {
			p++;
			if (*p != '^') {
				p = handle_cmd(p, m, &textbgcolor, &textfgcolor, xpos, ypos, &istw);
				pixman_image_unref(fgfill);
				fgfill = pixman_image_create_solid_fill(&textfgcolor);
				continue;
			}
		}

                if (istw) {
                        xpos = &twxpos;
                        ypos = &twypos;
                        bglayer = twbg;
                        fglayer = twfg;
                } else {
                        xpos = &swxpos;
                        ypos = &swypos;
                        bglayer = swbg;
                        fglayer = swbg;
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
		*xpos += x_kern;
		lastcp = codepoint;

		/* Detect and handle pre-rendered glyphs (e.g. emoji) */
		if (pixman_image_get_format(glyph->pix) == PIXMAN_a8r8g8b8) {
			/* Only the alpha channel of the mask is used, so we can
			 * use fgfill here to blend prerendered glyphs with the
			 * same opacity */
			pixman_image_composite32(
				PIXMAN_OP_OVER, glyph->pix, fgfill, fglayer, 0, 0, 0, 0,
				*xpos + glyph->x, *ypos - glyph->y, glyph->width, glyph->height);
		} else {
			/* Applying the foreground color here would mess up
			 * component alphas for subpixel-rendered text, so we
			 * apply it when blending. */
			pixman_image_composite32(
				PIXMAN_OP_OVER, fgfill, glyph->pix, fglayer, 0, 0, 0, 0,
				*xpos + glyph->x, *ypos - glyph->y, glyph->width, glyph->height);
		}

                pixman_image_fill_boxes(PIXMAN_OP_OVER, bglayer,
                                &textbgcolor, 1, &(pixman_box32_t){
                                        .x1 = *xpos,
                                        .x2 = MIN(*xpos + glyph->advance.x, maxlayerwidth),
                                        .y1 = 0,
                                        .y2 = height,
                                });

		/* increment pen position */
		*xpos += glyph->advance.x;
		*ypos += glyph->advance.y;
	}
	pixman_image_unref(fgfill);

	if (state != UTF8_ACCEPT)
		fprintf(stderr, "malformed UTF-8 sequence\n");

        for (int i = 0; i < nummons; i++) {
                m = &monitors[i];
                pixman_image_composite32(PIXMAN_OP_OVER, swbg, NULL, m->bar, 0, 0, 0, 0,
                                m->width - swxpos, 0, swxpos, height);
                pixman_image_composite32(PIXMAN_OP_OVER, swfg, NULL, m->bar, 0, 0, 0, 0,
                                m->width - swxpos, 0, swxpos, height);
                pixman_image_composite32(PIXMAN_OP_OVER, twbg, NULL, m->bar, 0, 0, 0, 0,
                                0, 0, maxlayerwidth, height);
                pixman_image_composite32(PIXMAN_OP_OVER, twfg, NULL, m->bar, 0, 0, 0, 0,
                                0, 0, maxlayerwidth, height);
	        pixman_image_unref(m->bar);
	        munmap(m->data, m->bufsize);
        }

	pixman_image_unref(swbg);
	pixman_image_unref(swfg);
	pixman_image_unref(twbg);
	pixman_image_unref(twfg);
}

/* Layer-surface setup adapted from layer-shell example in [wlroots] */
static void
layer_surface_configure(void *data,
		struct zwlr_layer_surface_v1 *surface,
		uint32_t serial, uint32_t w, uint32_t h)
{
	height = h;
        m = data;
	m->width = w;
	m->stride = m->width * 4;
	m->bufsize = m->stride * height;

        if (!m || !m->wl_surface || !m->layer_surface)
                BARF("failed to configure layer surface, no monitor defined.");

	if (exclusive_zone > 0)
		exclusive_zone = height;
	zwlr_layer_surface_v1_set_exclusive_zone(m->layer_surface, exclusive_zone);
	zwlr_layer_surface_v1_ack_configure(surface, serial);
}

static void
layer_surface_closed(void *data, struct zwlr_layer_surface_v1 *surface)
{
	zwlr_layer_surface_v1_destroy(surface);
	wl_surface_destroy(((Monitor*)data)->wl_surface);
	run_display = false;
}

static struct zwlr_layer_surface_v1_listener layer_surface_listener = {
	.configure = layer_surface_configure,
	.closed = layer_surface_closed,
};

static void
pointer_handle_enter(void *data, struct wl_pointer *pointer,
		uint32_t serial, struct wl_surface *surface,
		wl_fixed_t sx, wl_fixed_t sy)
{
	active_wl_surface = surface;
}

static void
pointer_handle_leave(void *data, struct wl_pointer *pointer,
		uint32_t serial, struct wl_surface *surface)
{
	active_wl_surface = NULL;
}

static void
pointer_handle_motion(void *data, struct wl_pointer *pointer,
		uint32_t time, wl_fixed_t sx, wl_fixed_t sy)
{
	mousex = (uint32_t)wl_fixed_to_int(sx);
	mousey = (uint32_t)wl_fixed_to_int(sy);
}

static void
pointer_handle_button(void *data, struct wl_pointer *wl_pointer,
		uint32_t serial, uint32_t time, uint32_t button,
		uint32_t state)
{
	if(active_wl_surface == ((Monitor*)data)->wl_surface && state) {
		for(uint32_t i=0; i < ca_entry_count; i++) {
			struct ca_entry entry = ca_entries[i];
			if(entry.mouse_button == button - BTN_LEFT &&
					entry.from_x <= mousex && mousex <= entry.to_x &&
					entry.from_y <= mousey && mousey <= entry.to_y) {
				FILE *script;
				script = popen(entry.cmd, "r");
				if (script != NULL) {
					while (1) {
						char *line;
						char buf[MAX_LINE_LEN];
						line = fgets(buf, sizeof(buf), script);
						if (line == NULL) break;
					}
				}
				pclose(script);
			}
		}
	}
}

static void
pointer_handle_axis(void *data, struct wl_pointer *wl_pointer,
		uint32_t time, uint32_t axis, wl_fixed_t value)
{
}

static const struct wl_pointer_listener pointer_listener = {
	pointer_handle_enter,
	pointer_handle_leave,
	pointer_handle_motion,
	pointer_handle_button,
	pointer_handle_axis,
};

static void
seat_handle_capabilities(void *data, struct wl_seat *seat,
		enum wl_seat_capability caps)
{
	if (caps & WL_SEAT_CAPABILITY_POINTER) {
		struct wl_pointer *pointer = wl_seat_get_pointer(seat);
		wl_pointer_add_listener(pointer, &pointer_listener, &monitors[0]);
	}
}

static const struct wl_seat_listener seat_listener = {
	seat_handle_capabilities,
};

static void
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
                if (nummons < MAX_OUTPUT_MONITORS) {
                        monitors[nummons] = (Monitor){.wl_output = o};
                        nummons++;
                }
	} else if (strcmp(interface, zwlr_layer_shell_v1_interface.name) == 0) {
		layer_shell = wl_registry_bind(registry, name,
				&zwlr_layer_shell_v1_interface, 1);
	} else if (strcmp(interface, wl_seat_interface.name) == 0) {
    		seat = wl_registry_bind(registry, name,
					&wl_seat_interface, 1);
		wl_seat_add_listener(seat, &seat_listener, NULL);
	}
}

static const struct wl_registry_listener registry_listener = {.global = handle_global,};

static void
drawbar()
{
        draw_frame("asdasd asdl asld");
        for (int i = 0; i < nummons; i++) {
                m = &monitors[i];
                if (!m->wl_buffer)
                        continue;
                printf("asdasd\n");
                wl_surface_attach(m->wl_surface, m->wl_buffer, 0, 0);
                wl_surface_damage_buffer(m->wl_surface, 0, 0, m->width, height);
                wl_surface_commit(m->wl_surface);
        }
}

static void
read_stdin(void)
{

	/* Read as much data as we can into line buffer */
	ssize_t b = read(STDIN_FILENO, line + linerem, MAX_LINE_LEN - linerem);
	if (b < 0)
		EBARF("read");
	if (b == 0) {
		run_display = 0;
		return;
	}
	linerem += b;

	/* Handle each line in the buffer in turn */
	char *curline, *end;
	for (curline = line; (end = memchr(curline, '\n', linerem)); curline = end) {
		*end++ = '\0';
		linerem -= end - curline;

		if (eat_line) {
			eat_line = false;
			continue;
		}

		/* Keep last line for redrawing purposes */
		memcpy(lastline, curline, end - curline);
	}

	if (linerem == MAX_LINE_LEN || eat_line) {
		/* Buffer is full, so discard current line */
		linerem = 0;
		eat_line = true;
	} else if (linerem && curline != line) {
		/* Shift any remaining data over */
		memmove(line, curline, linerem);
	}

        drawbar();
}

static void
event_loop(void)
{
	int ret;
	int wlfd = wl_display_get_fd(display);

	while (run_display) {
		fd_set rfds;
		FD_ZERO(&rfds);
		FD_SET(STDIN_FILENO, &rfds);
		FD_SET(wlfd, &rfds);

		/* Does this need to be inside the loop? */
		wl_display_flush(display);

		ret = select(wlfd + 1, &rfds, NULL, NULL, NULL);
		if (ret < 0)
			EBARF("select");

		if (FD_ISSET(STDIN_FILENO, &rfds))
			read_stdin();

		if (FD_ISSET(wlfd, &rfds))
			if (wl_display_dispatch(display) == -1)
				break;
	}
}

int
main(int argc, char **argv)
{
	char *namespace = "dtao";
	char *fontstr = "";
	char *actionstr = "";
	uint32_t layer = ZWLR_LAYER_SHELL_V1_LAYER_TOP;
	uint32_t anchor = ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP |
			ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT |
			ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT;

	/* Parse options */
	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-b")) {
			anchor ^= ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP |
				ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM;
		} else if (!strcmp(argv[i], "-bg")) {
			if (++i >= argc)
				BARF("option -bg requires an argument");
			if (parse_color(argv[i], &bgcolor))
				BARF("malformed color string for -bg");
		} else if (!strcmp(argv[i], "-e")) {
			if (++i >= argc)
				BARF("option -e requires an argument");
			actionstr = argv[i];
		} else if (!strcmp(argv[i], "-expand")) {
			if (++i >= argc)
				BARF("option -expand requires an argument");
			expand = true;
			if (argv[i][0] == 'l')
				titlealign = ALIGN_R;
			else if (argv[i][0] == 'r')
				titlealign = ALIGN_L;
			else if (argv[i][0] == 'c')
				titlealign = ALIGN_C;
		} else if (!strcmp(argv[i], "-fg")) {
			if (++i >= argc)
				BARF("option -fg requires an argument");
			if (parse_color(argv[i], &fgcolor))
				BARF("malformed color string for -fg");
		} else if (!strcmp(argv[i], "-fn")) {
			if (++i >= argc)
				BARF("option -fn requires an argument");
			fontstr = argv[i];
		} else if (!strcmp(argv[i], "-h")) {
			if (++i >= argc)
				BARF("option -h requires an argument");
			height = atoi(argv[i]);
		} else if (!strcmp(argv[i], "-l")) {
			if (++i >= argc)
				BARF("option -l requires an argument");
			lines = atoi(argv[i]);
		} else if (!strcmp(argv[i], "-L")) {
			if (++i >= argc)
				BARF("option -L requires an argument");
			if (argv[i][0] == 'o')
				layer = ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY;
			else if (argv[i][0] == 'b')
				layer = ZWLR_LAYER_SHELL_V1_LAYER_BOTTOM;
			else if (argv[i][0] == 'u')
				layer = ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND;
			else
				layer = ZWLR_LAYER_SHELL_V1_LAYER_TOP;
		} else if (!strcmp(argv[i], "-p")) {
			if (++i >= argc)
				BARF("option -p requires an argument");
			persist = atoi(argv[i]);
		} else if (!strcmp(argv[i], "-sa")) {
			if (++i >= argc)
				BARF("option -sa requires an argument");
			if (argv[i][0] == 'l')
				subalign = ALIGN_L;
			else if (argv[i][0] == 'r')
				subalign = ALIGN_R;
			else
				subalign = ALIGN_C;
		} else if (!strcmp(argv[i], "-ta")) {
			if (++i >= argc)
				BARF("option -ta requires an argument");
			/* Expand overrides alignment */
			if (!expand) {
				if (argv[i][0] == 'l')
					titlealign = ALIGN_L;
				else if (argv[i][0] == 'r')
					titlealign = ALIGN_R;
				else
					titlealign = ALIGN_C;
			}
		} else if (!strcmp(argv[i], "-tw")) {
			if (++i >= argc)
				BARF("option -tw requires an argument");
			titlewidth = atoi(argv[i]);
		} else if (!strcmp(argv[i], "-u")) {
			unified = 1;
		} else if (!strcmp(argv[i], "-v")) {
			fprintf(stderr, PROGRAM " " VERSION ", " COPYRIGHT "\n");
			return 0;
		} else if (!strcmp(argv[i], "-xs")) {
			if (++i >= argc)
				BARF("option -xs requires an argument");
			/* One-based to match dzen2 */
			output = atoi(argv[i]) - 1;
		} else if (!strcmp(argv[i], "-z")) {
			exclusive_zone++;
		} else {
			BARF("option '%s' not recognized\n%s", argv[i], USAGE);
		}
	}

	/* Set up display and protocols */
	display = wl_display_connect(NULL);
	if (!display)
		BARF("Failed to create display");

	struct wl_registry *registry = wl_display_get_registry(display);
	wl_registry_add_listener(registry, &registry_listener, NULL);
	wl_display_roundtrip(display);

	if (!compositor || !shm || !layer_shell)
		BARF("compositor does not support all needed protocols");

	/* Load selected font */
	fcft_set_scaling_filter(FCFT_SCALING_FILTER_LANCZOS3);
	font = fcft_from_name(1, (const char *[]) {fontstr}, NULL);
	if (!font)
		BARF("could not load font");

        for (int i = 0; i < nummons; i++) {
                m = &monitors[i];
                /* Create layer-shell surface */
                m->wl_surface = wl_compositor_create_surface(compositor);
                if (!m->wl_surface)
		        BARF("could not create wl_surface");
                m->layer_surface = zwlr_layer_shell_v1_get_layer_surface(layer_shell,
                                m->wl_surface, m->wl_output, layer, namespace);
                if (!m->layer_surface)
                        BARF("could not create layer_surface");
                zwlr_layer_surface_v1_add_listener(m->layer_surface,
                                &layer_surface_listener, m);

                /* Set layer size and positioning */
                if (!height)
                        height = font->ascent + font->descent;

                zwlr_layer_surface_v1_set_size(m->layer_surface, m->width, height);
                zwlr_layer_surface_v1_set_anchor(m->layer_surface, anchor);
	        wl_surface_commit(m->wl_surface);
	        wl_display_roundtrip(display);

                if (maxlayerwidth == 0 || maxlayerwidth > m->width)
                        maxlayerwidth = m->width;
        }

        drawbar();
	event_loop();

	/* Clean everything up */
        for (int i = 0; i < nummons; i++) {
                m = &monitors[i];
	        zwlr_layer_surface_v1_destroy(m->layer_surface);
	        wl_surface_destroy(m->wl_surface);
        }
	zwlr_layer_shell_v1_destroy(layer_shell);
	fcft_destroy(font);
	wl_shm_destroy(shm);
	wl_compositor_destroy(compositor);
	wl_registry_destroy(registry);
	wl_display_disconnect(display);

	return 0;
}
