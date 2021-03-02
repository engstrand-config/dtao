#define _POSIX_C_SOURCE 200112L
#include <linux/input-event-codes.h>
#include <assert.h>
#include <GLES2/gl2.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <wayland-cursor.h>
#include <wayland-egl.h>
#include <wlr/render/egl.h>
#include <wlr/util/log.h>
#include "wlr-layer-shell-unstable-v1-protocol.h"
#include "xdg-shell-protocol.h"

#include <ft2build.h>
#include FT_FREETYPE_H

#define FONTFILE "/usr/share/fonts/OTF/FantasqueSansMono-Regular.otf"
#define PTSIZE 50
#define TEXT "hello world"
#define TEXT_HEIGHT 24

static struct wl_display *display;
static struct wl_compositor *compositor;
static struct wl_seat *seat;
static struct wl_shm *shm;
static struct wl_pointer *pointer;
static struct wl_keyboard *keyboard;
static struct xdg_wm_base *xdg_wm_base;
static struct zwlr_layer_shell_v1 *layer_shell;

struct zwlr_layer_surface_v1 *layer_surface;
static struct wl_output *wl_output;

struct wl_surface *wl_surface;
struct wlr_egl egl;
struct wl_egl_window *egl_window;
struct wlr_egl_surface *egl_surface;
struct wl_callback *frame_callback;

static uint32_t output = UINT32_MAX;

static uint32_t layer = ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND;
static uint32_t anchor = 0;
static uint32_t width = 0, height = 0;
static bool run_display = true;
static bool animate = false;
static double frame = 0;
static int cur_x = -1, cur_y = -1;
static int buttons = 0;

struct wl_cursor_image *cursor_image;
struct wl_surface *cursor_surface, *input_surface;

static struct {
	struct timespec last_frame;
	float color[3];
	int dec;
} demo;

static void draw(void);

static void
surface_frame_callback(void *data, struct wl_callback *cb, uint32_t time)
{
	wl_callback_destroy(cb);
	frame_callback = NULL;
	draw();
}

static struct wl_callback_listener frame_listener = {
	.done = surface_frame_callback
};

static void
draw_bitmap(FT_Bitmap * bitmap, FT_Int x, FT_Int y)
{
	FT_Int i, j, p, q;
	FT_Int x_max = x + bitmap->width;
	FT_Int y_max = y + bitmap->rows;


	/* for simplicity, we assume that `bitmap->pixel_mode' */
	/* is `FT_PIXEL_MODE_GRAY' (i.e., not a bitmap font)   */

	for (i = x, p = 0; i < x_max; i++, p++) {
		for (j = y, q = 0; j < y_max; j++, q++) {
			if (i < 0 || j < 0 || i >= width || j >= height)
				continue;

			image[j][i] |= bitmap->buffer[q * bitmap->width + p];
		}
	}
}

static void
draw(void)
{
	eglMakeCurrent(egl.display, egl_surface, egl_surface, egl.context);
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);

	long ms = (ts.tv_sec - demo.last_frame.tv_sec) * 1000 +
			(ts.tv_nsec - demo.last_frame.tv_nsec) / 1000000;
	int inc = (demo.dec + 1) % 3;

	if (!buttons) {
		demo.color[inc] += ms / 2000.0f;
		demo.color[demo.dec] -= ms / 2000.0f;

		if (demo.color[demo.dec] < 0.0f) {
			demo.color[inc] = 1.0f;
			demo.color[demo.dec] = 0.0f;
			demo.dec = inc;
		}
	}

	if (animate) {
		frame += ms / 50.0;
	}

	glViewport(0, 0, width, height);
	if (buttons) {
		glClearColor(1, 1, 1, 1);
	} else {
		glClearColor(demo.color[0], demo.color[1], demo.color[2], 1);
	}
	glClear(GL_COLOR_BUFFER_BIT);


	// font stuff
	FT_Library library;
	FT_Face face;

	FT_GlyphSlot slot;
	FT_Vector pen;		/* untransformed origin  */
	FT_Error error;

	char *filename;
	char *text;

	int points;
	int target_height;
	int n, num_chars;


	filename = FONTFILE;
	text = TEXT;
	points = PTSIZE;
	num_chars = strlen(text);
	target_height = HEIGHT;

	error = FT_Init_FreeType(&library);	/* initialize library */
	/* error handling omitted */

	error = FT_New_Face(library, filename, 0, &face);	/* create face object */
	/* error handling omitted */

	/* use 50pt at 100dpi */
	error = FT_Set_Char_Size(face, points * 64, 0, 100, 0);	/* set character size */
	/* error handling omitted */

	/* cmap selection omitted;                                        */
	/* for simplicity we assume that the font contains a Unicode cmap */

	slot = face->glyph;

	/* the pen position in 26.6 cartesian space coordinates; */
	/* start at (300,200) relative to the upper left corner  */
	pen.x = 300 * 64;
	pen.y = (target_height - 200) * 64;

	for (n = 0; n < num_chars; n++) {
		/* set transformation */
		FT_Set_Transform(face, NULL, &pen);

		/* load glyph image into the slot (erase previous one) */
		error = FT_Load_Char(face, text[n], FT_LOAD_RENDER);
		if (error)
			continue;	/* ignore errors */

		/* now, draw to our target surface (convert position) */
		draw_bitmap(&slot->bitmap,
				slot->bitmap_left,
				target_height - slot->bitmap_top);

		/* increment pen position */
		pen.x += slot->advance.x;
		pen.y += slot->advance.y;
	}


	if (cur_x != -1 && cur_y != -1) {
		glEnable(GL_SCISSOR_TEST);
		glScissor(cur_x, height - cur_y, 5, 5);
		glClearColor(0, 0, 0, 1);
		glClear(GL_COLOR_BUFFER_BIT);
		glDisable(GL_SCISSOR_TEST);
	}

	frame_callback = wl_surface_frame(wl_surface);
	wl_callback_add_listener(frame_callback, &frame_listener, NULL);

	eglSwapBuffers(egl.display, egl_surface);

	demo.last_frame = ts;

	FT_Done_Face(face);
	FT_Done_FreeType(library);
}

static void
xdg_surface_handle_configure(void *data,
		struct xdg_surface *xdg_surface, uint32_t serial)
{
	xdg_surface_ack_configure(xdg_surface, serial);
}

static const struct xdg_surface_listener xdg_surface_listener = {
	.configure = xdg_surface_handle_configure,
};

static void
layer_surface_configure(void *data,
		struct zwlr_layer_surface_v1 *surface,
		uint32_t serial, uint32_t w, uint32_t h)
{
	width = w;
	height = h;
	wlr_log(WLR_DEBUG, "configure %ux%u", w, h);
	if (egl_window) {
		wl_egl_window_resize(egl_window, width, height, 0, 0);
	}
	zwlr_layer_surface_v1_ack_configure(surface, serial);
}

static void
layer_surface_closed(void *data, struct zwlr_layer_surface_v1 *surface)
{
	wlr_egl_destroy_surface(&egl, egl_surface);
	wl_egl_window_destroy(egl_window);
	zwlr_layer_surface_v1_destroy(surface);
	wl_surface_destroy(wl_surface);
	run_display = false;
}

struct zwlr_layer_surface_v1_listener layer_surface_listener = {
	.configure = layer_surface_configure,
	.closed = layer_surface_closed,
};

static void
wl_pointer_enter(void *data, struct wl_pointer *wl_pointer,
		uint32_t serial, struct wl_surface *surface,
		wl_fixed_t surface_x, wl_fixed_t surface_y)
{
	struct wl_cursor_image *image;
	image = cursor_image;
	wl_surface_attach(cursor_surface,
			wl_cursor_image_get_buffer(image), 0, 0);
	wl_surface_damage(cursor_surface, 1, 0, image->width, image->height);
	wl_surface_commit(cursor_surface);
	wl_pointer_set_cursor(wl_pointer, serial, cursor_surface,
			image->hotspot_x, image->hotspot_y);
	input_surface = surface;
}

static void
wl_pointer_leave(void *data, struct wl_pointer *wl_pointer,
		uint32_t serial, struct wl_surface *surface)
{
	cur_x = cur_y = -1;
	buttons = 0;
}

static void
wl_pointer_motion(void *data, struct wl_pointer *wl_pointer,
		uint32_t time, wl_fixed_t surface_x, wl_fixed_t surface_y)
{
	cur_x = wl_fixed_to_int(surface_x);
	cur_y = wl_fixed_to_int(surface_y);
}

static void
wl_pointer_button(void *data, struct wl_pointer *wl_pointer,
		uint32_t serial, uint32_t time, uint32_t button, uint32_t state)
{
	if (input_surface == wl_surface) {
		if (state == WL_POINTER_BUTTON_STATE_PRESSED) {
			buttons++;
		} else {
			buttons--;
		}
	} else {
		assert(false && "Unknown surface");
	}
}

static void
wl_pointer_axis(void *data, struct wl_pointer *wl_pointer,
		uint32_t time, uint32_t axis, wl_fixed_t value)
{
	// Who cares
}

static void
wl_pointer_frame(void *data, struct wl_pointer *wl_pointer)
{
	// Who cares
}

static void
wl_pointer_axis_source(void *data, struct wl_pointer *wl_pointer,
		uint32_t axis_source)
{
	// Who cares
}

static void
wl_pointer_axis_stop(void *data, struct wl_pointer *wl_pointer,
		uint32_t time, uint32_t axis)
{
	// Who cares
}

static void
wl_pointer_axis_discrete(void *data, struct wl_pointer *wl_pointer,
		uint32_t axis, int32_t discrete)
{
	// Who cares
}

struct wl_pointer_listener pointer_listener = {
	.enter = wl_pointer_enter,
	.leave = wl_pointer_leave,
	.motion = wl_pointer_motion,
	.button = wl_pointer_button,
	.axis = wl_pointer_axis,
	.frame = wl_pointer_frame,
	.axis_source = wl_pointer_axis_source,
	.axis_stop = wl_pointer_axis_stop,
	.axis_discrete = wl_pointer_axis_discrete,
};

static void
wl_keyboard_keymap(void *data, struct wl_keyboard *wl_keyboard,
		uint32_t format, int32_t fd, uint32_t size)
{
	// Who cares
}

static void
wl_keyboard_enter(void *data, struct wl_keyboard *wl_keyboard,
		uint32_t serial, struct wl_surface *surface,
		struct wl_array *keys)
{
	wlr_log(WLR_DEBUG, "Keyboard enter");
}

static void
wl_keyboard_leave(void *data, struct wl_keyboard *wl_keyboard,
		uint32_t serial, struct wl_surface *surface)
{
	wlr_log(WLR_DEBUG, "Keyboard leave");
}

static void
wl_keyboard_key(void *data, struct wl_keyboard *wl_keyboard,
		uint32_t serial, uint32_t time, uint32_t key, uint32_t state)
{
	wlr_log(WLR_DEBUG, "Key event: %d %d", key, state);
}

static void
wl_keyboard_modifiers(void *data, struct wl_keyboard *wl_keyboard,
		uint32_t serial, uint32_t mods_depressed, uint32_t mods_latched,
		uint32_t mods_locked, uint32_t group)
{
	// Who cares
}

static void
wl_keyboard_repeat_info(void *data, struct wl_keyboard *wl_keyboard,
		int32_t rate, int32_t delay)
{
	// Who cares
}

static struct wl_keyboard_listener keyboard_listener = {
	.keymap = wl_keyboard_keymap,
	.enter = wl_keyboard_enter,
	.leave = wl_keyboard_leave,
	.key = wl_keyboard_key,
	.modifiers = wl_keyboard_modifiers,
	.repeat_info = wl_keyboard_repeat_info,
};

static void
seat_handle_capabilities(void *data, struct wl_seat *wl_seat,
		enum wl_seat_capability caps)
{
	if ((caps & WL_SEAT_CAPABILITY_POINTER)) {
		pointer = wl_seat_get_pointer(wl_seat);
		wl_pointer_add_listener(pointer, &pointer_listener, NULL);
	}
	if ((caps & WL_SEAT_CAPABILITY_KEYBOARD)) {
		keyboard = wl_seat_get_keyboard(wl_seat);
		wl_keyboard_add_listener(keyboard, &keyboard_listener, NULL);
	}
}

static void
seat_handle_name(void *data, struct wl_seat *wl_seat, const char *name)
{
	// Who cares
}

const struct wl_seat_listener seat_listener = {
	.capabilities = seat_handle_capabilities,
	.name = seat_handle_name,
};

static void
handle_global(void *data, struct wl_registry *registry,
		uint32_t name, const char *interface, uint32_t version)
{
	if (strcmp(interface, wl_compositor_interface.name) == 0) {
		compositor = wl_registry_bind(registry, name,
				&wl_compositor_interface, 1);
	} else if (strcmp(interface, wl_shm_interface.name) == 0) {
		shm = wl_registry_bind(registry, name, &wl_shm_interface, 1);
	} else if (strcmp(interface, "wl_output") == 0) {
		if (output != UINT32_MAX) {
			if (!wl_output) {
				wl_output = wl_registry_bind(registry, name,
						&wl_output_interface, 1);
			} else {
				output--;
			}
		}
	} else if (strcmp(interface, wl_seat_interface.name) == 0) {
		seat = wl_registry_bind(registry, name, &wl_seat_interface, 1);
		wl_seat_add_listener(seat, &seat_listener, NULL);
	} else if (strcmp(interface, zwlr_layer_shell_v1_interface.name) == 0) {
		layer_shell = wl_registry_bind(registry, name,
				&zwlr_layer_shell_v1_interface, 1);
	} else if (strcmp(interface, xdg_wm_base_interface.name) == 0) {
		xdg_wm_base = wl_registry_bind(registry, name,
				&xdg_wm_base_interface, 1);
	}
}

static void
handle_global_remove(void *data, struct wl_registry *registry, uint32_t name)
{
	// who cares
}

static const struct wl_registry_listener registry_listener = {
.global = handle_global,.global_remove = handle_global_remove,};

int
main(int argc, char **argv)
{
	wlr_log_init(WLR_DEBUG, NULL);
	char *namespace = "wlroots";
	int exclusive_zone = -1;
	bool found;
	int c;
	layer = ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY;
	anchor = ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP |
			ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT |
			ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT;
	while ((c = getopt(argc, argv, "nxw:h:o:")) != -1) {
		switch (c) {
			case 'o':
				output = atoi(optarg);
				break;
			case 'w':
				width = atoi(optarg);
				break;
			case 'h':
				height = atoi(optarg);
				break;
			case 'x':
				exclusive_zone++;
				break;
			case 'n':
				animate = true;
				break;
			default:
				break;
		}
	}

	if (!height) {
		height = TEXT_HEIGHT;
	}

	display = wl_display_connect(NULL);
	if (display == NULL) {
		fprintf(stderr, "Failed to create display\n");
		return 1;
	}

	struct wl_registry *registry = wl_display_get_registry(display);
	wl_registry_add_listener(registry, &registry_listener, NULL);
	wl_display_roundtrip(display);

	if (compositor == NULL) {
		fprintf(stderr, "wl_compositor not available\n");
		return 1;
	}
	if (shm == NULL) {
		fprintf(stderr, "wl_shm not available\n");
		return 1;
	}
	if (layer_shell == NULL) {
		fprintf(stderr, "layer_shell not available\n");
		return 1;
	}

	struct wl_cursor_theme *cursor_theme =
			wl_cursor_theme_load(NULL, 16, shm);
	assert(cursor_theme);
	struct wl_cursor *cursor =
			wl_cursor_theme_get_cursor(cursor_theme, "crosshair");
	if (cursor == NULL) {
		cursor = wl_cursor_theme_get_cursor(cursor_theme, "left_ptr");
	}
	assert(cursor);
	cursor_image = cursor->images[0];

	cursor = wl_cursor_theme_get_cursor(cursor_theme, "tcross");
	if (cursor == NULL) {
		cursor = wl_cursor_theme_get_cursor(cursor_theme, "left_ptr");
	}
	assert(cursor);

	cursor_surface = wl_compositor_create_surface(compositor);
	assert(cursor_surface);

	EGLint attribs[] = { EGL_ALPHA_SIZE, 8, EGL_NONE };
	wlr_egl_init(&egl, EGL_PLATFORM_WAYLAND_EXT, display,
			attribs, WL_SHM_FORMAT_ARGB8888);

	wl_surface = wl_compositor_create_surface(compositor);
	assert(wl_surface);

	layer_surface = zwlr_layer_shell_v1_get_layer_surface(layer_shell,
			wl_surface, wl_output, layer, namespace);
	assert(layer_surface);
	zwlr_layer_surface_v1_set_size(layer_surface, width, height);
	zwlr_layer_surface_v1_set_anchor(layer_surface, anchor);
	zwlr_layer_surface_v1_set_exclusive_zone(layer_surface,
			exclusive_zone <= 0 ? exclusive_zone : height);
	zwlr_layer_surface_v1_add_listener(layer_surface,
			&layer_surface_listener, layer_surface);
	wl_surface_commit(wl_surface);
	wl_display_roundtrip(display);

	egl_window = wl_egl_window_create(wl_surface, width, height);
	assert(egl_window);
	egl_surface = wlr_egl_create_surface(&egl, egl_window);
	assert(egl_surface);

	wl_display_roundtrip(display);
	draw();

	while (wl_display_dispatch(display) != -1 && run_display) {
		// This space intentionally left blank
	}

	wl_cursor_theme_destroy(cursor_theme);
	return 0;
}
