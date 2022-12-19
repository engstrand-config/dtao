#pragma once

SCM config;

/* Config variable definitions. */
/* These will be automatically set from the guile config. */
static uint32_t height                  = 0;
static int exclusive                    = -1;
static int adjustwidth                  = 0;
static int isbottom                     = 0;
static int usewmcolorscheme             = 0;
static int borderpx                     = 0;
static char *fontstr                    = "";
static int updateinterval               = 1;
static char *delimiter                  = NULL;
static char *delimiterend               = NULL;
static uint32_t spacing                 = 10;

/* Bar padding */
static int padtop                       = 0;
static int padbottom                    = 0;
static int padleft                      = 0;
static int padright                     = 0;

/* positioning */
static int layer = ZWLR_LAYER_SHELL_V1_LAYER_TOP;;
static int anchor = ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP
	| ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT
	| ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT;

/* blocks */
static Block *leftblocks                = NULL;
static Block *centerblocks              = NULL;
static Block *rightblocks               = NULL;

/* default colors */
static pixman_color_t bordercolor = {
	.red = 0xffff,
	.green = 0x0000,
	.blue = 0x0000,
	.alpha = 0xffff,
};

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
dscm_parse_block(unsigned int index, SCM block, void *data, enum align a)
{
	Block *blocks = data;
	scm_t_bits *click = dscm_alist_get_proc_pointer(block, "click");

	blocks[index] = (Block){
		.align = a,
		.signal = dscm_alist_get_int(block, "signal"),
		.interval = dscm_alist_get_int(block, "interval"),
		.render = dscm_alist_get_proc_pointer(block, "render"),
		.events = dscm_alist_get_int(block, "events"),
		.click = click,
	};

	/* TODO: Send the mouse button of the click event to the click handler */
	if (click)
		wl_list_insert(&cas, &blocks[index].clink);
}

static inline void
dscm_config_parse(char *configfile)
{
	SCM eval, barlayer;

	scm_c_primitive_load(configfile);
	config = dscm_get_variable("config");
	if (scm_is_null(config))
		BARF("invalid config");

	fontstr = dscm_alist_get_string(config, "font");
	height = dscm_alist_get_int(config, "height");
	borderpx = dscm_alist_get_int(config, "border-px");
	exclusive = dscm_alist_get_int(config, "exclusive");
	isbottom = dscm_alist_get_int(config, "bottom");
	adjustwidth = dscm_alist_get_int(config, "adjust-width");
	usewmcolorscheme = dscm_alist_get_int(config, "use-dwl-guile-colorscheme");
	delimiter = dscm_alist_get_string(config, "delimiter");
	spacing = dscm_alist_get_int(config, "block-spacing");
	padtop = dscm_alist_get_int(config, "padding-top");
	padbottom = dscm_alist_get_int(config, "padding-bottom");
	padleft = dscm_alist_get_int(config, "padding-left");
	padright = dscm_alist_get_int(config, "padding-right");

	if (delimiter)
		delimiterend = delimiter + strlen(delimiter);
	if (isbottom)
		anchor ^= ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP |
			ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM;

	if (!usewmcolorscheme) {
		char *bgstr, *fgstr, *borderstr;
		bgstr = dscm_alist_get_string(config, "background-color");
		fgstr = dscm_alist_get_string(config, "foreground-color");
		borderstr = dscm_alist_get_string(config, "border-color");
		parse_color(bgstr, &bgcolor);
		parse_color(fgstr, &fgcolor);
		parse_color(borderstr, &bordercolor);
		free(bgstr);
		free(fgstr);
		free(borderstr);
	}

	barlayer = dscm_alist_get(config, "layer");
	eval = scm_primitive_eval(barlayer);
	layer = scm_to_int(eval);

	leftblocks = dscm_iterate_list(dscm_alist_get(config, "left-blocks"),
				       sizeof(Block), &dscm_parse_block, ALIGN_L);
	centerblocks = dscm_iterate_list(dscm_alist_get(config, "center-blocks"),
					 sizeof(Block), &dscm_parse_block, ALIGN_C);
	rightblocks = dscm_iterate_list(dscm_alist_get(config, "right-blocks"),
					sizeof(Block), &dscm_parse_block, ALIGN_R);
}

static inline void
dscm_config_cleanup()
{
	free(leftblocks);
	free(centerblocks);
	free(rightblocks);
	free(fontstr);
}
