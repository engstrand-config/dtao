#pragma once

#define DSCM_ARG1 "1"
#define DSCM_ARG2 "1"
#define DSCM_ARG3 "1"
#define DSCM_ARG4 "1"

#define DSCM_ASSERT(PRED, MSG, ...)					\
	if (!PRED) scm_misc_error(					\
		"", MSG, scm_list_n(__VA_ARGS__, SCM_UNDEFINED))

#define DSCM_ASSERT_TYPE(PRED, VALUE, SUBR, ARG, TYPE)			\
	DSCM_ASSERT(PRED, SUBR ": Wrong type argument in position " ARG \
		    " (expected " TYPE "): ~a", VALUE)

#define DSCM_SET_REST(SUBR, REST, NUM)					\
	int length = scm_to_int(scm_length(REST));			\
	DSCM_ASSERT((length % NUM == 0),				\
		    SUBR ": Invalid number of (rest) arguments: ~a", REST); \
	for (int i = 0; i < length; i += NUM)

typedef struct {
	SCM proc;
	uint32_t button;
} dscm_call_data_t;

typedef void(*dscm_reloader_t)();
typedef void(*dscm_setter_t)(void*, SCM);

static inline SCM
dscm_list_ref(SCM list, int index)
{
	return scm_list_ref(list, scm_from_int(index));
}

static inline scm_t_bits *
dscm_get_pointer(SCM proc)
{
	scm_gc_protect_object(proc);
	return SCM_UNPACK_POINTER(proc);
}

static inline SCM
dscm_assoc_ref(SCM alist, const char* symbol)
{
	return scm_assoc_ref(alist, scm_string_to_symbol(scm_from_locale_string(symbol)));
}

static inline char*
dscm_assoc_ref_string(SCM alist, const char *symbol)
{
	SCM value = dscm_assoc_ref(alist, symbol);
	if (scm_is_string(value))
		return scm_to_locale_string(value);
	return NULL;
}

static inline unsigned int
dscm_get_list_length(SCM list)
{
	return scm_to_unsigned_integer(scm_length(list), 0, -1);
}

static inline SCM
dscm_get_list_item(SCM list, unsigned int index)
{
	return scm_list_ref(list, scm_from_unsigned_integer(index));
}

static inline void *
dscm_call_render_callback(void *data)
{
	dscm_call_data_t *wrapper = (dscm_call_data_t*)data;
	return scm_call_0(wrapper->proc);
}

static inline void *
dscm_call_click_callback(void *data)
{
	dscm_call_data_t *wrapper = (dscm_call_data_t*)data;
	SCM button = scm_from_int(wrapper->button);
	return scm_call_1(wrapper->proc, button);
}

static inline SCM
dscm_safe_call_render(scm_t_bits *ptr, Monitor *m)
{
	if (ptr == NULL)
		BARF("dscm: could not call proc that is NULL");
	dscm_call_data_t wrapper = {.proc = SCM_PACK_POINTER(ptr)};
	return scm_c_with_continuation_barrier(&dscm_call_render_callback, &wrapper);
}

static inline SCM
dscm_safe_call_click(scm_t_bits *ptr, Monitor *m, uint32_t button)
{
	if (ptr == NULL)
		BARF("dscm: could not call proc that is NULL");
	dscm_call_data_t wrapper = {.proc = SCM_PACK_POINTER(ptr), .button = button};
	return scm_c_with_continuation_barrier(&dscm_call_click_callback, &wrapper);
}

static inline Monitor*
dscm_get_monitor_by_name(SCM monitor)
{
	Monitor *m = NULL;
	uint32_t name = scm_to_uint32(monitor);
	wl_list_for_each(m, &monitors, link)
		if (m->name == name)
			break;
	return m;
}
