#pragma once

static inline SCM
dscm_enabled_in_tagset(SCM tag, uint32_t tagset)
{
	int index = scm_to_int(tag);
	if (tagset & (1 << index))
		return SCM_BOOL_T;
	return SCM_BOOL_F;

}

static inline Monitor *
dscm_get_exposed_monitor()
{
	SCM monitor, ptr;
	monitor = scm_c_lookup("dtao:active-monitor!");
	ptr = scm_variable_ref(monitor);
	return scm_to_pointer(ptr);
}

static inline SCM
dscm_binding_selected_monitor()
{
	Monitor *m = dscm_get_exposed_monitor();
	if (m && m == selmon)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static inline SCM
dscm_binding_active_tag(SCM tag)
{
	Monitor *m = dscm_get_exposed_monitor();
	if (!m)
		return SCM_BOOL_F;
	return dscm_enabled_in_tagset(tag, m->activetags);
}

static inline SCM
dscm_binding_urgent_tag(SCM tag)
{
	Monitor *m = dscm_get_exposed_monitor();
	if (!m)
		return SCM_BOOL_F;
	return dscm_enabled_in_tagset(tag, m->urgenttags);
}

static inline SCM
dscm_binding_selected_tag(SCM tag)
{
	Monitor *m = dscm_get_exposed_monitor();
	if (m && scm_to_int(tag) == m->seltag)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static inline SCM
dscm_binding_title()
{
	Monitor *m = dscm_get_exposed_monitor();
	if (m)
		return scm_from_locale_string(m->title);
	return scm_string(SCM_EOL);
}

static inline SCM
dscm_binding_layout()
{
	Monitor *m = dscm_get_exposed_monitor();
	if (!m || scm_is_null(layouts))
		return scm_string(SCM_EOL);
	SCM sel = scm_list_ref(layouts, scm_from_int(m->layout));
	if (scm_is_null(sel))
		return scm_string(SCM_EOL);
	return sel;
}

static inline SCM
dscm_binding_view(SCM tag)
{
	Monitor *m = dscm_get_exposed_monitor();
	int new = scm_to_int(tag);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_tags(m->dscm, ((1 << new) | TAGMASK), 1);
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_toggleview(SCM tag)
{
	Monitor *m = dscm_get_exposed_monitor();
	int toggle = scm_to_int(tag);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_tags(m->dscm, m->activetags ^ ((1 << toggle) | TAGMASK), 0);
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_tag(SCM tag)
{
	Monitor *m = dscm_get_exposed_monitor();
	int new = scm_to_int(tag);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_client_tags(m->dscm, 0, ((1 << new) | TAGMASK));
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_toggletag(SCM tag)
{
	Monitor *m = dscm_get_exposed_monitor();
	int toggle = scm_to_int(tag);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_client_tags(m->dscm, ~0, ((1 << toggle) | TAGMASK));
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_tags()
{
	return tags;
}

static inline SCM
dscm_binding_setlayout(SCM layout)
{
	Monitor *m = dscm_get_exposed_monitor();
	int index = scm_to_int(layout);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_layout(m->dscm, index);
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_nextlayout()
{
	Monitor *m = dscm_get_exposed_monitor();
	if (!m)
		return SCM_BOOL_F;
	/* The selected layout will be updated once the compositor
	 * has acknowledged the updated layout (dscm_monitor_layout event). */
	if (m->layout >= numlayouts - 1)
		dscm_monitor_v1_set_layout(m->dscm, 0);
	else
		dscm_monitor_v1_set_layout(m->dscm, m->layout + 1);
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_getlayout()
{
	Monitor *m = dscm_get_exposed_monitor();
	if (!m)
		return SCM_BOOL_F;
	SCM name = scm_list_ref(layouts, scm_from_int(m->layout));
	if (!name)
		return SCM_BOOL_F;
	return name;
}

static inline SCM
dscm_binding_layouts()
{
	return layouts;
}

static inline void
dscm_register()
{
	/* Layering */
	scm_c_define("LAYER-OVERLAY",
		     scm_from_int(ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY));
	scm_c_define("LAYER-BACKGROUND",
		     scm_from_int(ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND));
	scm_c_define("LAYER-BOTTOM",
		     scm_from_int(ZWLR_LAYER_SHELL_V1_LAYER_BOTTOM));
	scm_c_define("LAYER-TOP",
		     scm_from_int(ZWLR_LAYER_SHELL_V1_LAYER_TOP));

	scm_c_define_gsubr("dtao:selected-monitor?", 0, 0, 0,
			   &dscm_binding_selected_monitor);
	scm_c_define_gsubr("dtao:active-tag?", 1, 0, 0,
			   &dscm_binding_active_tag);
	scm_c_define_gsubr("dtao:urgent-tag?", 1, 0, 0,
			   &dscm_binding_urgent_tag);
	scm_c_define_gsubr("dtao:selected-tag?", 1, 0, 0,
			   &dscm_binding_selected_tag);
	scm_c_define_gsubr("dtao:title", 0, 0, 0,
			   &dscm_binding_title);
	scm_c_define_gsubr("dtao:layout", 0, 0, 0,
			   &dscm_binding_layout);
	scm_c_define_gsubr("dtao:view", 1, 0, 0,
			   &dscm_binding_view);
	scm_c_define_gsubr("dtao:toggle-view", 1, 0, 0,
			   &dscm_binding_toggleview);
	scm_c_define_gsubr("dtao:tag", 1, 0, 0,
			   &dscm_binding_tag);
	scm_c_define_gsubr("dtao:toggle-tag", 1, 0, 0,
			   &dscm_binding_toggletag);
	scm_c_define_gsubr("dtao:set-layout", 1, 0, 0,
			   &dscm_binding_setlayout);
	scm_c_define_gsubr("dtao:next-layout", 0, 0, 0,
			   &dscm_binding_nextlayout);
	scm_c_define_gsubr("dtao:get-layout", 0, 0, 0,
			   &dscm_binding_getlayout);
	scm_c_define_gsubr("dtao:tags", 0, 0, 0,
			   &dscm_binding_tags);
	scm_c_define_gsubr("dtao:layouts", 0, 0, 0,
			   &dscm_binding_layouts);
}
