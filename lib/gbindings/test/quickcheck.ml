
let test_roundtrip typ typ_name encoder decoder = 
    QCheck.Test.make ~count:10000
        ~name:(Printf.sprintf "%s_roundtrip" typ_name)
        typ (fun v -> (decoder (encoder v)) == v)

let test_roundtrip_float typ typ_name encoder decoder = 
    QCheck.Test.make ~count:10000
        ~name:(Printf.sprintf "%s_roundtrip" typ_name)
        typ (fun v -> (abs_float (decoder (encoder v)) -. v) < 0.5)

let roundtrip_tests = 
    let open QCheck in 
    let open Gbindings in
    let ui64 = set_gen Gen.ui64 int64 in 
    [
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

(*
let _ = 
    let open OUnit in 
    run_test_tt_main (
        "roundtrips" >::: (List.map QCheck_ounit.to_ounit_test roundtrip_tests))
        *)

let () = 
    List.iter QCheck.Test.check_exn roundtrip_tests




