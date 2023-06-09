#pragma once

static inline SCM
dscm_enabled_in_tagset(SCM monitor, SCM tag, uint32_t tagset)
{
	int index = scm_to_int(tag);
	if (tagset & (1 << index))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static inline SCM
dscm_binding_active_tag(SCM monitor, SCM tag)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
	if (!m)
		return SCM_BOOL_F;
	return dscm_enabled_in_tagset(monitor, tag, m->activetags);
}

static inline SCM
dscm_binding_urgent_tag(SCM monitor, SCM tag)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
	if (!m)
		return SCM_BOOL_F;
	return dscm_enabled_in_tagset(monitor, tag, m->urgenttags);
}

static inline SCM
dscm_binding_selected_tag(SCM monitor, SCM tag)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
	if (m && scm_to_uint32(tag) == m->seltag)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static inline SCM
dscm_binding_view(SCM monitor, SCM tag)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
	int new = scm_to_int(tag);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_tags(m->dscm, ((1 << new) | TAGMASK), 1);
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_toggleview(SCM monitor, SCM tag)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
	int toggle = scm_to_int(tag);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_tags(m->dscm, m->activetags ^ ((1 << toggle) | TAGMASK), 0);
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_tag(SCM monitor, SCM tag)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
	int new = scm_to_int(tag);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_client_tags(m->dscm, 0, ((1 << new) | TAGMASK));
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_toggletag(SCM monitor, SCM tag)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
	int toggle = scm_to_int(tag);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_client_tags(m->dscm, ~0, ((1 << toggle) | TAGMASK));
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_setlayout(SCM monitor, SCM layout)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
	int index = scm_to_int(layout);
	if (!m)
		return SCM_BOOL_F;
	dscm_monitor_v1_set_layout(m->dscm, index);
	return SCM_BOOL_T;
}

static inline SCM
dscm_binding_nextlayout(SCM monitor)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
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
dscm_binding_getlayout(SCM monitor)
{
	Monitor *m = dscm_get_monitor_by_name(monitor);
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

static inline SCM
dscm_binding_tags()
{
	return tags;
}

static inline SCM
dscm_binding_set(SCM rest)
{
	DSCM_SET_REST("set", rest, 2) {
		SCM key = dscm_list_ref(rest, i);
		SCM meta = scm_hash_ref(metadata, key, SCM_UNDEFINED);
		DSCM_ASSERT(!scm_is_false(meta), "Invalid key in setter: ~a", key);

		dscm_reloader_t func;
		void *cvar = scm_to_pointer(scm_car(meta));
		dscm_setter_t setter = (dscm_setter_t)scm_to_pointer(scm_cadr(meta));
		(*setter)(cvar, dscm_list_ref(rest, i + 1));

		/* When parsing the config for the first time, there is no need to
		   manually reload the updated configuration parameter, since this
		   will be done automatically right after.*/
		if (!firstload) {
			SCM reloader = scm_caddr(meta);
			if (!scm_is_false(reloader)) {
				func = (dscm_reloader_t)scm_to_pointer(reloader);
				(*func)();
			}
		}
	}
	return SCM_BOOL_T;
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

	scm_c_define_gsubr("dtao:active-tag?", 2, 0, 0,
			   &dscm_binding_active_tag);
	scm_c_define_gsubr("dtao:urgent-tag?", 2, 0, 0,
			   &dscm_binding_urgent_tag);
	scm_c_define_gsubr("dtao:selected-tag?", 2, 0, 0,
			   &dscm_binding_selected_tag);
	scm_c_define_gsubr("dtao:view", 2, 0, 0,
			   &dscm_binding_view);
	scm_c_define_gsubr("dtao:toggle-view", 2, 0, 0,
			   &dscm_binding_toggleview);
	scm_c_define_gsubr("dtao:tag", 2, 0, 0,
			   &dscm_binding_tag);
	scm_c_define_gsubr("dtao:toggle-tag", 2, 0, 0,
			   &dscm_binding_toggletag);
	scm_c_define_gsubr("dtao:set-layout", 2, 0, 0,
			   &dscm_binding_setlayout);
	scm_c_define_gsubr("dtao:next-layout", 1, 0, 0,
			   &dscm_binding_nextlayout);
	scm_c_define_gsubr("dtao:get-layout", 1, 0, 0,
			   &dscm_binding_getlayout);
	scm_c_define_gsubr("dtao:tags", 0, 0, 0,
			   &dscm_binding_tags);
	scm_c_define_gsubr("dtao:layouts", 0, 0, 0,
			   &dscm_binding_layouts);

	/* dtao-guile specific bindings */
	scm_c_define_gsubr("set", 0, 0, 1, &dscm_binding_set);
}
