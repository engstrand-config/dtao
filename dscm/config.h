#pragma once

#ifdef DEVELOP
#define REPL_SOCKET_PATH "/tmp/dtao-guile-devel.socket"
#else
#define REPL_SOCKET_PATH "/tmp/dtao-guile.socket"
#endif

#define DSCM_DEFINE_P(CVAR, KEY, SETTER, RELOADER)			\
	{								\
		SCM m1 = scm_from_pointer(&(CVAR), NULL);		\
		SCM m2 = scm_from_pointer(SETTER, NULL);		\
		SCM m3 = SCM_BOOL_F;					\
		scm_gc_protect_object(m1);				\
		scm_gc_protect_object(m2);				\
		if (RELOADER != NULL) {					\
			m3 = scm_from_pointer(RELOADER, NULL);		\
			scm_gc_protect_object(m3);			\
		}							\
		scm_hash_set_x(						\
			metadata,					\
			scm_string_to_symbol(scm_from_locale_string(KEY)), \
			scm_list_3(m1, m2, m3));			\
	}

#define DSCM_DEFINE(CVAR, KEY, DEFVAL, SETTER, RELOADER)	\
	{							\
		(CVAR) = DEFVAL;				\
		DSCM_DEFINE_P(CVAR, KEY, SETTER, RELOADER);	\
	}

static SCM metadata;

/* Set to 0 after the initial config load */
static unsigned int firstload = 1;

/* Config variable definitions. */
/* These will be automatically set from the guile config. */
static char *fontstr           = "";
static uint32_t height         = 0;
static uint32_t updateinterval = 1;
static uint32_t padtop         = 0;
static uint32_t padbottom      = 0;
static uint32_t padleft        = 0;
static uint32_t padright       = 0;
static scm_t_bits *renderer    = NULL;
static struct wl_list blocks;

/* positioning */
static int layer = ZWLR_LAYER_SHELL_V1_LAYER_TOP;
static int anchor = ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP
	| ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT
	| ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT;


/* Default colors */
static pixman_color_t bgcolor = {
	.red = 0x1111,
	.green = 0x1111,
	.blue = 0x1111,
	.alpha = 0x0000,
};

static pixman_color_t fgcolor = {
	.red = 0xb3b3,
	.green = 0xb3b3,
	.blue = 0xb3b3,
	.alpha = 0xffff,
};

static inline void
setter_int(void *cvar, SCM value)
{
	DSCM_ASSERT_TYPE(scm_is_integer(value),
			 value, "set", DSCM_ARG2, "int");
	(*((int*)cvar)) = scm_to_int(value);
}


static inline void
setter_uint32(void *cvar, SCM value)
{
	DSCM_ASSERT_TYPE(scm_is_integer(value),
			 value, "set", DSCM_ARG2, "uint32");
	(*((uint32_t*)cvar)) = scm_to_uint32(value);
}

static inline void
setter_string(void *cvar, SCM value)
{
	DSCM_ASSERT_TYPE(scm_is_string(value),
			 value, "set", DSCM_ARG2, "string");
	(*((char**)cvar)) = scm_to_locale_string(value);
}

static inline void
setter_color(void *cvar, SCM value)
{
	DSCM_ASSERT_TYPE(scm_is_string(value),
			 value, "set", DSCM_ARG2, "string");
	pixman_color_t **dest = cvar;
	char *colorstr = scm_to_locale_string(value);
	parse_color(colorstr, *dest);
	free(colorstr);
}

static inline void
setter_renderer(void *cvar, SCM value)
{
	DSCM_ASSERT_TYPE((scm_is_true(scm_procedure_p(value))),
			 value, "set", DSCM_ARG2, "procedure");
	(*((scm_t_bits**)cvar)) = dscm_get_pointer(value);
}

static inline void
setter_block(void *cvar, SCM value)
{
	DSCM_ASSERT_TYPE((scm_is_true(scm_list_p(value))),
			 value, "set", DSCM_ARG2, "alist");

	Block *b;
	int found = 0;
	struct wl_list *lst = cvar;

	char *id = dscm_assoc_ref_string(value, "id");
	DSCM_ASSERT((id != NULL), "Missing id of block: ~s", value);

	wl_list_for_each(b, lst, link) {
		if (!strcmp(b->id, id)) {
			found = 1;
			free(id);
		}
	}

	SCM interval = dscm_assoc_ref(value, "interval");
	SCM signal = dscm_assoc_ref(value, "signal");
	SCM events = dscm_assoc_ref(value, "events?");
	SCM render = dscm_assoc_ref(value, "render");
	SCM click = dscm_assoc_ref(value, "click");

	scm_dynwind_begin(0);

	if (!found) {
		b = calloc(1, sizeof(Block));
		b->id = id;
		scm_dynwind_unwind_handler(free, b, 0);
		scm_dynwind_unwind_handler(free, id, 0);
	}

	if (!scm_is_false(interval)) {
		DSCM_ASSERT_TYPE(scm_is_integer(interval),
				 value, "define-block", "interval", "int");
		b->interval = scm_to_int(interval);
	}
	if (!scm_is_false(signal)) {
		DSCM_ASSERT_TYPE(scm_is_integer(interval),
				 value, "define-block", "signal", "int");
		b->signal = scm_to_int(signal);
	}
	if (!scm_is_false(events)) {
		DSCM_ASSERT_TYPE(scm_is_bool(events),
				 value, "define-block", "events", "bool");
		b->events = scm_to_int(events);
	}
	if (!scm_is_false(click)) {
		DSCM_ASSERT_TYPE(scm_is_true(scm_procedure_p(click)),
				 value, "define-block", "click", "procedure");
		b->click = dscm_get_pointer(click);
	}
	if (!scm_is_false(render)) {
		DSCM_ASSERT_TYPE(scm_is_true(scm_procedure_p(render)),
				 value, "define-block", "render", "procedure");
		b->render = dscm_get_pointer(render);
	}

	if (!found)
		wl_list_insert(lst, &b->link);

	scm_dynwind_end();
}

static inline void
dscm_config_load(char *configfile)
{
	if (firstload) {
		scm_c_define("dtao:%repl-socket-path",
			     scm_from_locale_string(REPL_SOCKET_PATH));
		scm_c_primitive_load(PREFIX "/share/dtao-guile/init.scm");
		firstload = 0;
	}
	scm_c_primitive_load(configfile);
}

static inline void
dscm_config_initialize()
{
	wl_list_init(&blocks);

	scm_permanent_object(metadata);
	metadata = scm_make_hash_table(scm_from_int(20));

	DSCM_DEFINE(height, "height", 30, &setter_uint32, NULL);
	DSCM_DEFINE(updateinterval, "update-interval", 1, &setter_uint32, NULL);
	DSCM_DEFINE(padtop, "padding-top", 5, &setter_uint32, NULL);
	DSCM_DEFINE(padbottom, "padding-bottom", 5, &setter_uint32, NULL);
	DSCM_DEFINE(padleft, "padding-left", 5, &setter_uint32, NULL);
	DSCM_DEFINE(padright, "padding-right", 5, &setter_uint32, NULL);
	DSCM_DEFINE(padright, "padding-right", 5, &setter_uint32, NULL);

	DSCM_DEFINE_P(layer, "layer", &setter_int, NULL);
	DSCM_DEFINE_P(anchor, "anchor", &setter_int, NULL);
	DSCM_DEFINE_P(fontstr, "font", &setter_string, NULL);
	DSCM_DEFINE_P(bgcolor, "text-color", &setter_color, NULL);
	DSCM_DEFINE_P(fgcolor, "background-color", &setter_color, NULL);

	DSCM_DEFINE_P(blocks, "blocks", &setter_block, NULL);
	DSCM_DEFINE_P(renderer, "renderer", &setter_renderer, NULL);
}

static inline void
dscm_config_cleanup()
{
	free(fontstr);
}
