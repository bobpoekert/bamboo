
exception Break
exception Continue

let break_ () = raise Break
let continue_ () = raise Continue

let rec for_ res mapper data = 
    match data with 
    | [] -> List.rev res
    | h :: t -> (try (for_ ((mapper h) :: res) mapper data) with
        | Break -> List.rev res
        | Continue -> (for_ res mapper t)
    )

let rec list_contains v l = 
    match l with
    | [] -> false
    | h :: t -> 
            if v == h then true 
            else (list_contains v t)

let list_not_contains v l = not (list_contains v l)

let slice slicee lower upper step = 
    let rec iter ctr res v =
        if v == [] then res 
        else if (match lower with
                | None -> false
                | Some c -> c < ctr) then (iter (ctr + 1) res (List.tl v))
        else if (match upper with 
                | None -> false
                | Some c -> c >= ctr) then res 
        else if (match step with 
                | None -> false
                | Some c -> ctr mod c != 0) then (iter (ctr + 1) res (List.tl v))
        else match v with
        | [] -> res
        | h :: t -> (iter (ctr + 1) (h :: res) t) in
    List.rev (iter 0 [] slicee)

(* this is just a stub, 
 * there should be a compiler pass that rewrites Widget's to other things so this never gets called *)
let make_widget guid f = {
    args = ();
    constructor_guid = guid;
    result = None;
    construct = f;
}
