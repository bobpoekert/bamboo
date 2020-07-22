type g_value
type g_type
type g_pointer
type g_object
type g_widget
type g_container
type g_application

external g_type_name : g_type -> string = "gcaml_type_name"
external g_type_of_name : string -> g_type = "gcaml_type_of_name"
external g_type_of_value : g_value -> g_type = "gcaml_type_of_value"

external g_value_of_boolean : bool -> g_value = "gcaml_g_value_of_boolean"
external g_value_of_char : int -> g_value = "gcaml_g_value_of_char"
external g_value_of_uchar : int -> g_value = "gcaml_g_value_of_uchar"
external g_value_of_int : int -> g_value = "gcaml_g_value_of_int"
external g_value_of_uint : int -> g_value = "gcaml_g_value_of_uint"
external g_value_of_long : int64 -> g_value = "gcaml_g_value_of_long"
external g_value_of_ulong : int64 -> g_value = "gcaml_g_value_of_ulong"
external g_value_of_int64 : int64 -> g_value = "gcaml_g_value_of_int64"
external g_value_of_uint64 : int64 -> g_value = "gcaml_g_value_of_uint64"
external g_value_of_float : float -> g_value = "gcaml_g_value_of_float"
external g_value_of_double : float -> g_value = "gcaml_g_value_of_double"
external inner_g_value_of_string : string -> g_value = "gcaml_g_value_of_string"

let g_value_of_string s = 
    String.escaped s
    |> inner_g_value_of_string

external g_value_of_g_object : g_object -> g_value = "gcaml_g_value_of_g_object"
external g_value_of_enum : int -> g_value = "gcaml_g_value_of_enum"
external g_value_of_flags : int -> g_value = "gcaml_g_value_of_flags"

external boolean_of_g_value : g_value -> bool = "gcaml_boolean_of_g_value"
external char_of_g_value : g_value -> int = "gcaml_char_of_g_value"
external uchar_of_g_value : g_value -> int = "gcaml_uchar_of_g_value"
external int_of_g_value : g_value -> int = "gcaml_int_of_g_value"
external uint_of_g_value : g_value -> int = "gcaml_uint_of_g_value"
external long_of_g_value : g_value -> int64 = "gcaml_long_of_g_value"
external ulong_of_g_value : g_value -> int64 = "gcaml_ulong_of_g_value"
external int64_of_g_value : g_value -> int64 = "gcaml_int64_of_g_value"
external uint64_of_g_value : g_value -> int64 = "gcaml_uint64_of_g_value"
external float_of_g_value : g_value -> float = "gcaml_float_of_g_value"
external double_of_g_value : g_value -> float = "gcaml_double_of_g_value"
external inner_string_of_g_value : g_value -> string = "gcaml_string_of_g_value"

let string_of_g_value v = 
    inner_string_of_g_value v
    |> Scanf.unescaped

external g_object_of_g_value : g_value -> g_object = "gcaml_g_object_of_g_value"
external enum_of_g_value : g_value -> int = "gcaml_enum_of_g_value"
external flags_of_g_value : g_value -> int = "gcaml_flags_of_g_value"

external g_type_of_g_value : g_value -> g_type = "gcaml_g_type_of_g_value"

external object_new : g_type -> g_object = "gcaml_object_new"

let object_new_by_name name = 
    g_type_of_name name
    |> object_new

external object_unref : g_object -> unit = "gcaml_object_unref"

external object_get_property_type : g_object -> string -> g_type = "gcaml_object_get_property_type"
external object_set_property : g_object -> string -> g_value = "gcaml_object_set_property"
external object_get_property_of_type : g_object -> string -> g_type -> g_value = "gcaml_object_get_property"

external object_cast_to_widget : g_object -> g_widget = "gcaml_object_cast_to_widget"
external widget_cast_to_object : g_widget -> g_object = "%identity"

external inner_widget_set_name : g_widget -> string -> unit = "gcaml_widget_set_name"
external inner_widget_get_name : g_widget -> string = "gcaml_widget_get_name"

let widget_set_name w n = 
    inner_widget_set_name w (String.escaped n)

let widget_get_name w = Scanf.unescaped (inner_widget_get_name w)

external object_cast_to_container : g_object -> g_container = "gcaml_object_cast_to_container"
external container_cast_to_object : g_container -> g_object = "%identity"
external container_add_child : g_container -> g_container -> string -> unit = "gcaml_container_append_child"
external container_remove_child : g_container -> g_container -> unit = "gcaml_container_remove_child"

(* this is like this for a reason. don't call init yourself! *)
let _ = 
    let module Init = struct 
        external init : int -> string array -> unit = "gcaml_init"
    end in 
    Init.init (Array.length Sys.argv) Sys.argv
