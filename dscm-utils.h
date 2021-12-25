#pragma once

static inline SCM
dscm_get_variable(const char *name)
{
        return scm_variable_ref(scm_c_lookup(name));
}

static inline SCM
dscm_alist_get(SCM alist, const char* key)
{
        return scm_assoc_ref(alist, scm_from_utf8_string(key));
}

static inline char*
dscm_alist_get_string(SCM alist, const char* key)
{
        SCM value = dscm_alist_get(alist, key);
        if (scm_is_string(value))
                return scm_to_locale_string(value);
        return NULL;
}

static inline int
dscm_alist_get_int(SCM alist, const char* key)
{
        SCM value = dscm_alist_get(alist, key);
        if (scm_is_bool(value))
                return scm_is_true(value) ? 1 : 0;
        return scm_to_int(value);
}
