#pragma once

/* TODO: Can the monitor be bound to the execution context
 * of the blocks, such that it must not be specified as
 * an argument? */
static inline SCM
dscm_binding_selected_monitor(SCM monitor)
{
        Monitor *m = (Monitor*)scm_to_pointer(monitor);
        if (m && m == selmon)
                return SCM_BOOL_T;
        return SCM_BOOL_F;
}

static inline SCM
dscm_binding_selected_tag(SCM monitor, SCM tag)
{
        Monitor *m = (Monitor*)scm_to_pointer(monitor);
        int index = scm_to_int(tag);
        if (m && (m->activetags & (1 << (index - 1))))
                return SCM_BOOL_T;
        return SCM_BOOL_F;
}

static inline SCM
dscm_binding_title(SCM monitor)
{
        Monitor *m = (Monitor*)scm_to_pointer(monitor);
        if (m)
                return scm_from_locale_string(m->title);
        return scm_string(SCM_EOL);
}

static inline SCM
dscm_binding_layout(SCM monitor)
{
        Monitor *m = (Monitor*)scm_to_pointer(monitor);
        if (!m || scm_is_null(layouts))
                return scm_string(SCM_EOL);
        SCM sel = scm_list_ref(layouts, scm_from_int(m->layout));
        if (scm_is_null(sel))
                return scm_string(SCM_EOL);
        return sel;
}

static inline SCM
dscm_binding_view(SCM monitor, SCM tag)
{
        Monitor *m = (Monitor*)scm_to_pointer(monitor);
        int new = scm_to_int(tag);
        if (!m || new >= scm_to_int(scm_length(tags)))
                return SCM_BOOL_F;
        dscm_monitor_v1_set_tags(m->dscm, new, 1);
        return SCM_BOOL_T;
}

static inline SCM
dscm_binding_tags()
{
        return tags;
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

        scm_c_define_gsubr("dtao:selected-monitor?", 1, 0, 0,
                           &dscm_binding_selected_monitor);
        scm_c_define_gsubr("dtao:selected-tag?", 2, 0, 0,
                           &dscm_binding_selected_tag);
        scm_c_define_gsubr("dtao:title", 1, 0, 0,
                           &dscm_binding_title);
        scm_c_define_gsubr("dtao:layout", 1, 0, 0,
                           &dscm_binding_layout);
        scm_c_define_gsubr("dtao:view", 2, 0, 0,
                           &dscm_binding_view);
        scm_c_define_gsubr("dtao:tags", 0, 0, 0,
                           &dscm_binding_tags);
        scm_c_define_gsubr("dtao:layouts", 0, 0, 0,
                           &dscm_binding_layouts);
}
