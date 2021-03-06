#include <glib.h>
#include <glib-object.h>
#include <gobject/gvaluecollector.h>
#include <gtk/gtk.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/version.h>
#include <caml/callback.h>

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

#define unbox_g_value(v) ((GValue *) Data_custom_val(v))


void gcaml_g_value_release(value object_wrapper) {
    GValue *v = unbox_g_value(object_wrapper);
    g_value_unset(v);
}

static struct custom_operations gcaml_g_value_ops = {
    "cheap.hella.gcaml.value",
    gcaml_g_value_release,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

value alloc_g_value(GType type) {
    value res = caml_alloc_custom(&gcaml_g_value_ops, sizeof(GValue), 0, 1);
    GValue *v = Data_custom_val(res);
    if (!v) caml_failwith("out of memory?");
    memset(v, 0, sizeof(GValue));
    g_value_init(v, type);
    return res;
}

value copy_g_value(GValue *v) {
    value res = alloc_g_value(G_VALUE_TYPE(v));
    GValue *rv = unbox_g_value(res);
    g_value_copy(v, rv);
    return res;
}

typedef struct gcaml_gobject_box {
    GObject *obj;
} gcaml_gobject_box;

#define unbox_g_object(v) G_OBJECT(((gcaml_gobject_box *) Data_custom_val(v))->obj)

void gcaml_gobject_release(value object_wrapper) {
    gcaml_gobject_box *box = (gcaml_gobject_box *) Data_custom_val(object_wrapper);
    if (box->obj) {
        g_object_unref((gpointer) box->obj);
        box->obj = 0;
    }
}

static struct custom_operations gcaml_gobject_ops = {
    "cheap.hella.gcaml.object",
    gcaml_gobject_release,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

value gcaml_box_gobject(GObject *v) {
    value res = caml_alloc_custom(&gcaml_gobject_ops, sizeof(gcaml_gobject_box), 0, 1);
    gcaml_gobject_box *box = Data_custom_val(res);
    box->obj = v;
    return res;
}

value box_g_type(GType typ) {
    value res = caml_alloc(sizeof(GType), 0);
    GType *typ_v = Data_custom_val(res);
    *typ_v = typ;
    return res;
}

GType unbox_g_type(value v_typ) {
    return *((GType *) Data_custom_val(v_typ));
}


CAMLprim value gcaml_object_new(value v_type) {

    CAMLparam1(v_type);
    CAMLlocal1(v_res);

    GType type = unbox_g_type(v_type);
    GObject *res = g_object_new_with_properties(type, 0, NULL, NULL);

    g_object_ref_sink(res);

    v_res = gcaml_box_gobject(res);
    CAMLreturn(v_res);
}

CAMLprim value gcaml_object_unref(value object) {

    CAMLparam1(object);

    gcaml_gobject_release(object); 

    CAMLreturn(Val_unit);
}

/* types and values */


CAMLprim value gcaml_type_name(value v_type) {
    CAMLparam1(v_type);
    CAMLlocal1(res);
    GType type = unbox_g_type(v_type);
    const char *name = g_type_name(type);
    if (!name) caml_failwith("GType not found");
    res = caml_copy_string(name);
    CAMLreturn(res);
}

CAMLprim value gcaml_type_of_name(value v_name) {
    CAMLparam1(v_name);
    const char *name = String_val(v_name);
    GType type = g_type_from_name(name);
    if (!type) caml_failwith("GType not found");
    CAMLreturn(box_g_type(type));
}

CAMLprim value gcaml_type_of_value(value v_v) {
    CAMLparam1(v_v);
    CAMLlocal1(res);
    GValue *gv = unbox_g_value(v_v);
    GType typ = G_VALUE_TYPE(gv);
    res = box_g_type(typ);
    CAMLreturn(res);
}

CAMLprim value gcaml_g_value_of_boolean(value v_boolean) {
    CAMLparam1(v_boolean);
    CAMLlocal1(res);
    gboolean v = Bool_val(v_boolean);
    res = alloc_g_value(G_TYPE_BOOLEAN);
    g_value_set_boolean(unbox_g_value(res), v);
    CAMLreturn(res);
}

CAMLprim value gcaml_boolean_of_g_value(value v_value) {
    CAMLparam1(v_value);
    GValue *gv = unbox_g_value(v_value);
    gboolean v = g_value_get_boolean(gv);
    CAMLreturn(v ? Val_true : Val_false);
}

#define VALUE_OF_CHAR(sym, typ, setter) CAMLprim value sym(value v_char) { \
    CAMLparam1(v_char);\
    CAMLlocal1(res);\
    gint8 v = Int_val(v_char);\
    res = alloc_g_value(typ);\
    setter(unbox_g_value(res), v);\
    CAMLreturn(res);\
}

CAMLprim value gcaml_char_of_g_value(value v_value) {
    CAMLparam1(v_value);
    GValue *gv = unbox_g_value(v_value);
    CAMLreturn(Val_int(g_value_get_schar(gv)));
}

CAMLprim value gcaml_uchar_of_g_value(value v_value) {
    CAMLparam1(v_value);
    GValue *gv = unbox_g_value(v_value);
    CAMLreturn(Val_int(g_value_get_uchar(gv)));
}


VALUE_OF_CHAR(gcaml_g_value_of_char, G_TYPE_CHAR, g_value_set_schar)
VALUE_OF_CHAR(gcaml_g_value_of_uchar, G_TYPE_UCHAR, g_value_set_uchar)

#define VALUE_OF_INT(sym, typ, setter)\
CAMLprim value sym(value v_int) {\
    CAMLparam1(v_int);\
    CAMLlocal1(res);\
    res = alloc_g_value(typ);\
    setter(unbox_g_value(res), Int_val(v_int));\
    CAMLreturn(res);\
}

#define VALUE_OF_INT64(sym, typ, setter)\
CAMLprim value sym(value v_int) {\
    CAMLparam1(v_int);\
    CAMLlocal1(res);\
    int64_t i = Int64_val(v_int);\
    res = alloc_g_value(typ);\
    setter(unbox_g_value(res), i);\
    CAMLreturn(res);\
}


VALUE_OF_INT(gcaml_g_value_of_int, G_TYPE_INT, g_value_set_int)
VALUE_OF_INT(gcaml_g_value_of_uint, G_TYPE_UINT, g_value_set_uint)
VALUE_OF_INT(gcaml_g_value_of_enum, G_TYPE_ENUM, g_value_set_enum)
VALUE_OF_INT(gcaml_g_value_of_flags, G_TYPE_FLAGS, g_value_set_flags)
VALUE_OF_INT64(gcaml_g_value_of_long, G_TYPE_LONG, g_value_set_long)
VALUE_OF_INT64(gcaml_g_value_of_ulong, G_TYPE_ULONG, g_value_set_ulong)
VALUE_OF_INT64(gcaml_g_value_of_int64, G_TYPE_INT64, g_value_set_int64)
VALUE_OF_INT64(gcaml_g_value_of_uint64, G_TYPE_UINT64, g_value_set_uint64)

#define INT_OF_VALUE(sym, getter)\
CAMLprim value sym(value inp) {\
    CAMLparam1(inp);\
    CAMLreturn(Val_int(getter(unbox_g_value(inp))));\
}

#define INT64_OF_VALUE(sym, getter)\
CAMLprim value sym(value inp) {\
    CAMLparam1(inp);\
    CAMLreturn(caml_copy_int64(getter(unbox_g_value(inp))));\
}

INT_OF_VALUE(gcaml_int_of_g_value, g_value_get_int)
INT_OF_VALUE(gcaml_uint_of_g_value, g_value_get_uint)
INT_OF_VALUE(gcaml_enum_of_g_value, g_value_get_enum)
INT_OF_VALUE(gcaml_flags_of_g_value, g_value_get_flags)
INT64_OF_VALUE(gcaml_long_of_g_value, g_value_get_long)
INT64_OF_VALUE(gcaml_ulong_of_g_value, g_value_get_ulong)
INT64_OF_VALUE(gcaml_int64_of_g_value, g_value_get_int64)
INT64_OF_VALUE(gcaml_uint64_of_g_value, g_value_get_uint64)

CAMLprim value gcaml_g_value_of_float(value v_float) {
    CAMLparam1(v_float);
    CAMLlocal1(res);
    float v = (float) Double_val(v_float);
    res = alloc_g_value(G_TYPE_FLOAT);
    g_value_set_float(unbox_g_value(res), v);
    CAMLreturn(res);
}

CAMLprim value gcaml_float_of_g_value(value inp) {
    CAMLparam1(inp);
    GValue *gv = unbox_g_value(inp);
    CAMLreturn(caml_copy_double((double) g_value_get_float(gv)));
}

CAMLprim value gcaml_double_of_g_value(value inp) {
    CAMLparam1(inp);
    GValue *gv = unbox_g_value(inp);
    CAMLreturn(caml_copy_double(g_value_get_double(gv)));
}

CAMLprim value gcaml_g_value_of_double(value v_float) {
    CAMLparam1(v_float);
    CAMLlocal1(res);
    double v = Double_val(v_float);
    res = alloc_g_value(G_TYPE_DOUBLE);
    g_value_set_double(unbox_g_value(res), v);
    CAMLreturn(res);
}

CAMLprim value gcaml_g_value_of_string(value v_string) {
    CAMLparam1(v_string);
    CAMLlocal1(res);
    const char *s = String_val(v_string);
    res = alloc_g_value(G_TYPE_STRING);
    g_value_set_string(unbox_g_value(res), s);
    CAMLreturn(res);
}

CAMLprim value gcaml_string_of_g_value(value gv_v) {
    CAMLparam1(gv_v);
    CAMLlocal1(res);
    GValue *gv = unbox_g_value(gv_v);
    if (!G_VALUE_HOLDS_STRING(gv)) caml_failwith("type error!");
    const char *str = g_value_get_string(gv);

    /* DRAGONS: do we really have to rely on the null terminator here? */
    res = caml_copy_string(str);

    CAMLreturn(res);
}

CAMLprim value gcaml_g_value_of_g_object(value v_object) {
    CAMLparam1(v_object);
    CAMLlocal1(res);
    gcaml_gobject_box *box = Data_custom_val(v_object);
    if (!(box->obj)) caml_failwith("Trying to cast null pointer GObject * to GValue *!");
    GObject *obj = box->obj;
    res = alloc_g_value(G_TYPE_OBJECT);
    GValue *res_gv = unbox_g_value(res);
    g_value_set_object(res_gv, obj);
    CAMLreturn(res);
}

CAMLprim value gcaml_g_object_of_g_value(value v_value) {
    CAMLparam1(v_value);
    CAMLlocal1(res);
    GValue *gv = unbox_g_value(v_value);
    GObject *obj = g_value_get_object(gv);
    res = gcaml_box_gobject(obj);
    CAMLreturn(res);
}

CAMLprim value gcaml_value_of_boxed(value v_object) {
    CAMLparam1(v_object);
    CAMLlocal1(res);
    void *ptr = (void *) v_object;
    res = alloc_g_value(G_TYPE_BOXED);
    g_value_set_boxed(unbox_g_value(res), ptr);
    CAMLreturn(res);
}

CAMLprim value gcaml_boxed_of_g_value(value v_val) {
    CAMLparam1(v_val);
    GValue *gv = unbox_g_value(v_val);
    void *payload = g_value_get_boxed(gv);
    CAMLreturn((value) payload);
}

CAMLprim value gcaml_value_of_pointer(value v_object) {
    CAMLparam1(v_object);
    CAMLlocal1(res);
    void *ptr = (void *) v_object;
    res = alloc_g_value(G_TYPE_POINTER);
    g_value_set_pointer(unbox_g_value(res), ptr);
    CAMLreturn(res);
}


CAMLprim value gcaml_object_set_property(value v_obj, value v_name, value v_val) {
    CAMLparam3(v_obj, v_name, v_val);
    GObject *obj = unbox_g_object(v_obj);
    const char *name = String_val(v_name);
    GValue *val = unbox_g_value(v_val);

    g_object_set_property(obj, name, val);

    CAMLreturn(Val_unit);
}

CAMLprim value gcaml_object_get_property(value v_obj, value v_name, value v_typ) {
    CAMLparam3(v_obj, v_name, v_typ);
    CAMLlocal1(res);
    GObject *obj = unbox_g_object(v_obj);
    const char *name = String_val(v_name);
    GType typ = unbox_g_type(v_typ);
    res = alloc_g_value(v_typ);
    
    g_object_get_property(obj, name, unbox_g_value(res));

    CAMLreturn(res);
}

CAMLprim value gcaml_object_get_property_type(value v_obj, value v_name) {
    CAMLparam2(v_obj, v_name);
    CAMLlocal1(res);
    GObject *obj = unbox_g_object(v_obj);
    const char *name = String_val(v_name);

    GObjectClass *cls = G_OBJECT_CLASS(obj);
    GParamSpec *prop = g_object_class_find_property(cls, name);

    if (!prop) caml_failwith("Property not found");

    GType typ = prop->value_type;

    g_param_spec_unref(prop);

    res = box_g_type(typ);
    CAMLreturn(res);
}

CAMLprim value gcaml_container_append_child(value v_parent, value v_child) {
    CAMLparam2(v_parent, v_child);

    GtkContainer *parent = (GtkContainer *) unbox_g_object(v_parent);
    GtkWidget *child = (GtkWidget *) unbox_g_object(v_child);

    gtk_container_add(parent, child);

    CAMLreturn(Val_unit);
}

CAMLprim value gcaml_container_remove_child(value v_parent, value v_child) {
    CAMLparam2(v_parent, v_child);

    GtkContainer *parent = (GtkContainer *) unbox_g_object(v_parent);
    GtkWidget *child = (GtkWidget *) unbox_g_object(v_child);

    gtk_container_remove(parent, child);

    CAMLreturn(Val_unit);
}

CAMLprim value gcaml_widget_set_name(value v_widg, value v_name) {
    CAMLparam2(v_widg, v_name);

    GtkWidget *widg = (GtkWidget *) unbox_g_object(v_widg);
    const char *name = String_val(v_name);

    gtk_widget_set_name(widg, name);

    CAMLreturn(Val_unit);
}

CAMLprim value gcaml_widget_get_name(value v_widg) {
    CAMLparam1(v_widg);
    CAMLlocal1(res);

    GtkWidget *widg = (GtkWidget *) unbox_g_object(v_widg);
    const char *name = gtk_widget_get_name(widg);

    res = caml_copy_string(name);

    CAMLreturn(res);
}

CAMLprim value gcaml_widget_show(value v_widg) {
    CAMLparam1(v_widg);

    GtkWidget *widg = (GtkWidget *) unbox_g_object(v_widg);

    gtk_widget_show(widg);

    CAMLreturn(Val_unit);

}

CAMLprim value gcaml_object_cast_to_widget(value v_widg) {
    CAMLparam1(v_widg);
    
    GObject *obj = unbox_g_object(v_widg);

    if (!G_TYPE_CHECK_INSTANCE_TYPE(obj, GTK_TYPE_WIDGET))
        caml_failwith("Type error: illegal cast to GtkWidget");

    CAMLreturn(v_widg);
}

CAMLprim value gcaml_object_cast_to_container(value v_widg) {
    CAMLparam1(v_widg);
    
    GObject *obj = unbox_g_object(v_widg);

    if (!G_TYPE_CHECK_INSTANCE_TYPE(obj, GTK_TYPE_CONTAINER))
        caml_failwith("Type error: illegal cast to GtkContainer");

    CAMLreturn(v_widg);
}

CAMLprim value gcaml_init(value v_argc, value v_argv) {
    CAMLparam2(v_argc, v_argv);

    int argc = Int_val(v_argc);
    char **argv = malloc(sizeof(char *) * argc);
    for (size_t i=0; i < argc; i++) {
        const char *instring = String_val(Field(v_argv, i));
        size_t size = strlen(instring); /* DRAGONS: I know, I know.*/
        char *newstring = malloc(size);
        memcpy(newstring, instring, size);
        argv[i] = newstring;
    }

    gtk_init(&argc, &argv);
    /* we're deliberately not freeing argv because gtk_init expects us not to.
     * this function should only ever be called once so it's not a memory leak
     * */

    /* TODO: get rid of this and register the types we use with girepository */
    gtk_test_register_all_types();


    CAMLreturn(Val_unit);
}

/* signals */

typedef struct gcaml_closure_internal {
    GClosure closure;
    value caml_callback;
} gcaml_closure_internal;

typedef gcaml_closure_internal *gcaml_closure;

void gcaml_closure_init(gcaml_closure res, value inp) {

    res->caml_callback = inp;
    caml_register_global_root(&res->caml_callback);


}

void gcaml_closure_destroy(gpointer notify_data, GClosure *closure) {

    gcaml_closure inp = (gcaml_closure) closure;
    caml_remove_global_root(&(inp->caml_callback));

}

void gcaml_closure_marshaller(
        GClosure *closure,
        GValue *return_value,
        guint n_param_values,
        const GValue *param_values,
        gpointer invocation_hint,
        gpointer marshal_data) {

    CAMLparam0();
    CAMLlocal2(arg_array, retval);
    arg_array = caml_alloc(n_param_values * sizeof(value), 0);

    for (size_t i=0; i < n_param_values; i++) {
        Store_field(arg_array, i, copy_g_value((GValue *) &param_values[i]));
    }

    retval = caml_callback(((gcaml_closure) closure)->caml_callback, arg_array);
    if (retval != Val_unit) {
        GValue *g_retval = unbox_g_value(retval);

        g_value_copy(g_retval, return_value);
    }

    CAMLreturn0;
}

GClosure *gcaml_closure_to_gclosure(value oc) {

    gcaml_closure occ;
    GClosure *res = g_closure_new_simple(sizeof(gcaml_closure_internal), 0);
    occ = (gcaml_closure) res;
    gcaml_closure_init(occ, oc);

    g_closure_add_finalize_notifier(res, 0, gcaml_closure_destroy);
    g_closure_set_marshal(res, gcaml_closure_marshaller);

    return res;
}

CAMLprim value gcaml_signal_connect(
        value v_target,
        value v_signal_name,
        value callback,
        value v_is_after) {

    CAMLparam4(v_target, v_signal_name, callback, v_is_after);
    CAMLlocal1(res);

    const char *signal_name = String_val(v_signal_name);
    GObject *target = unbox_g_object(v_target);
    gboolean after = Bool_val(v_is_after);

    GClosure *closure = gcaml_closure_to_gclosure(callback);
    
    gulong connection_id = g_signal_connect_closure(
            target, signal_name, closure, after);

    if (connection_id == 0) {
        caml_failwith("Failed to attach signal");
    }

    res = caml_alloc(sizeof(gulong), 0);
    *((gulong *) Data_custom_val(res)) = connection_id;

    CAMLreturn(res);
}

CAMLprim value gcaml_signal_disconnect(
        value v_target,
        value v_id) {
    CAMLparam2(v_target, v_id);

    gulong id = *((gulong *) Data_custom_val(v_id));
    GObject *target = unbox_g_object(v_target);

    g_signal_handler_disconnect(target, id);

    CAMLreturn(Val_unit);
}

/* styles */

CAMLprim value gcaml_widget_bind_css(value v_target, value v_css_path, value v_priority) {
    CAMLparam3(v_target, v_css_path, v_priority);

    const char *path = String_val(v_css_path);
    int priority = Int_val(v_priority);
    GObject *target = unbox_g_object(v_target);
    GtkWidget *widg = (GtkWidget *) target;

    GtkCssProvider *provider = gtk_css_provider_new();
   
    GError *error;
    gtk_css_provider_load_from_path(provider, path, &error);

    if (error) caml_failwith(error->message);

    GtkStyleContext *sctx = gtk_widget_get_style_context(widg);

    gtk_style_context_add_provider(sctx, provider, priority);

    CAMLreturn(Val_unit);
}
