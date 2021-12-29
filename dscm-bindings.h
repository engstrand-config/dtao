#pragma once

static inline void
dscm_register()
{
        /* TODO: Add bindings for clicking tags, etc. */
        /* Layering */
        scm_c_define("LAYER-OVERLAY",
                scm_from_int(ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY));
        scm_c_define("LAYER-BACKGROUND",
                scm_from_int(ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND));
        scm_c_define("LAYER-BOTTOM",
                scm_from_int(ZWLR_LAYER_SHELL_V1_LAYER_BOTTOM));
        scm_c_define("LAYER-TOP",
                scm_from_int(ZWLR_LAYER_SHELL_V1_LAYER_TOP));
}
