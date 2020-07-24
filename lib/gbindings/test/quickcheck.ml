open OUnit
open Gbindings

let test_roundtrip typ typ_name encoder decoder = 
    QCheck.Test.make ~count:10000
        ~name:(Printf.sprintf "%s_roundtrip" typ_name)
        typ (fun v -> (decoder (encoder v)) == v)

let test_roundtrip_float typ typ_name encoder decoder = 
    QCheck.Test.make ~count:10000
        ~name:(Printf.sprintf "%s_roundtrip" typ_name)
        typ (fun v -> (abs_float (decoder (encoder v)) -. v) < 0.5)

let test_widget_set_name = 
    let open QCheck in
    let typ = g_type_of_name "GtkLabel" in 
    let tester n = 
        let obj = object_new typ in 
        let widg = object_cast_to_widget obj in 
        widget_set_name widg n;
        String.equal n (widget_get_name widg) in 
    QCheck.Test.make ~count:10000 ~name:"widget_name" string tester

let roundtrip_tests = 
    let open QCheck in 
    let ui64 = set_gen Gen.ui64 int64 in 
    [
        test_widget_set_name;
        test_roundtrip bool "boolean" g_value_of_boolean boolean_of_g_value;
        test_roundtrip (-127 -- 127) "char" g_value_of_char char_of_g_value;
        test_roundtrip (int_bound 255) "uchar" g_value_of_uchar uchar_of_g_value;
        test_roundtrip_float float "float" g_value_of_float float_of_g_value;
        test_roundtrip_float float "double" g_value_of_double double_of_g_value;
        QCheck.Test.make ~count:10000
            ~name:"string_roundtrip"
            string (fun v -> String.equal (string_of_g_value (g_value_of_string v)) v);
        test_roundtrip int "int" g_value_of_int int_of_g_value;
        test_roundtrip int "uint" g_value_of_uint uint_of_g_value;
        test_roundtrip int64 "long" g_value_of_long long_of_g_value;
        test_roundtrip ui64 "ulong" g_value_of_ulong ulong_of_g_value;
        test_roundtrip int64 "int64" g_value_of_int64 int64_of_g_value;
        test_roundtrip ui64 "uint64" g_value_of_uint64 uint64_of_g_value;
    ]

let test_type_name _test_ctx = 
    let name = "GtkWidget" in
    let t = g_type_of_name name in 
    let n = g_type_name t in 
    assert_equal name n

let test_object_construct _text_ctx = 
    let typ = g_type_of_name "GObject" in 
    let _obj = object_new typ in ()

let () = 
    let _ = run_test_tt_main (
        "object" >::: [
            "type name" >:: test_type_name;
            "construct" >:: test_object_construct;
        ]
    ) in
    List.iter QCheck.Test.check_exn roundtrip_tests



