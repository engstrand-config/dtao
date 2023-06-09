#pragma once

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

/* Config variable definitions. */
/* These will be automatically set from the guile config. */
static char *fontstr           = "";
static uint32_t height         = 0;
static uint32_t updateinterval = 1;
static uint32_t spacing        = 10;
static uint32_t padtop         = 0;
static uint32_t padbottom      = 0;
static uint32_t padleft        = 0;
static uint32_t padright       = 0;
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
	DSCM_ASSERT_TYPE(scm_is_integer(value), value, "set", DSCM_ARG2, "int");
	(*((int*)cvar)) = scm_to_integer(value);
}


static inline void
setter_uint32(void *cvar, SCM value)
{
	DSCM_ASSERT_TYPE(scm_is_integer(value), value, "set", DSCM_ARG2, "uint32");
	(*((uint32_t*)cvar)) = scm_to_uint32(value);
}

static inline void
setter_string(void *cvar, SCM value)
{
	DSCM_ASSERT_TYPE(scm_is_string(value), value, "set", DSCM_ARG2, "string");
	((char*)cvar)) = scm_to_locale_string(value);
}

static inline void
setter_color(void *cvar, SCM value)
{
	DSCM_ASSERT_TYPE(scm_is_string(value), value, "set", DSCM_ARG2, "string");
	pixman_color_t *dest = cvar;
	char *colorstr = scm_to_locale_string(value);
	parse_color(dest, colorstr);
	free(colorstr);
}

static inline void
dscm_config_load(char *configfile)
{
	scm_c_primitive_load(PREFIX "/share/dtao-guile/init.scm");
	scm_c_primitive_load(configfile);
}

static inline void
dscm_config_initialize()
{
	wl_list_init(blocks);

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
}

static inline void
dscm_config_cleanup()
{
	free(fontstr);
}
