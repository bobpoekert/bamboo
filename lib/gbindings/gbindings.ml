type g_value
type g_type
type g_pointer
type g_object

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
external object_unref : g_object -> unit = "gcaml_object_unref"
