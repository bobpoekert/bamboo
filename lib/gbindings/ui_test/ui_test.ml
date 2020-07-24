open Gbindings

let label_type = g_type_of_name "GtkLabel"
let window_type = g_type_of_name "GtkWindow"
let grid_type = g_type_of_name "GtkGrid"

let make_label text =
    let lobj = object_new label_type in 
    let widg = object_cast_to_widget lobj in 
    let text_val = g_value_of_string text in 
    object_set_property lobj "label" text_val;
    widget_show widg;
    widg

let into_container cont_type init children = 
    let obj = object_new cont_type in
    let cont = object_cast_to_container obj in 
    let widg = object_cast_to_widget obj in
    init obj;
    List.iter (container_add_child cont) children;
    widget_show widg;
    widg
    

let into_grid = into_container grid_type (fun v -> 
    object_set_property v "column_spacing" (g_value_of_int 10);
    object_set_property v "row_spacing" (g_value_of_int 10);
    ()
)

let into_window = into_container window_type ignore

let () = Lwt_main.run (

    Lwt_glib.install ();

    let grid = 
        List.init 10 (fun v -> make_label (Printf.sprintf "%d" v))
        |> into_grid in
    let waiter, _waker = Lwt.wait () in
    ignore (into_window [grid]);
    waiter

    
)
