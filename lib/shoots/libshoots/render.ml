module type UIToolkit = sig 
    type node_t

    (* unitype for property values.
     * for GTK this is Gbindings.g_value (a wrapper around GValue)
     *)
    type unival_t

    type callback_t

    val create_node : (string * unival_t) list -> node_t
    val append_child : node_t -> node_t -> unit
    val replace_child : node_t (* parent *) -> node_t (* child *) -> unit

    (* return value indicates whether callback was there to remove in the first place *)
    val remove_callback : node_t -> callback_t -> bool


end

type 'a for_result = Stop | Skip | Result of 'a

let map_for mapper lst =
    let rec iter res lst = 
        match lst with 
        | [] -> res
        | h :: t -> match (mapper h) with
            | Stop -> res
            | Skip -> iter res t
            | Result v -> iter (v :: res) t in
    List.rev (iter [] lst)


module Make(T : UIToolkit) = struct
    type 'a tree_result = {
        node : T.node_t;
        children : ('a tree_node) list;
    } and type 'a tree_node = {
        args : 'a;
        constructor_guid : int;
        result : ('a tree_result) option ref;
        construct : 'a -> 'a tree_result;
    }

    type param = (string * T.unival_t)

    let create_node constructor args = 
        {
            args = args;
            result = ref None;
            construct = constructor;
        }

    let node_children node = 
        match !node.result with 
        | None -> []
        | Some v -> v.children

    let node_node node =
        match !node.result with
        | None -> raise (Failure "node should be inflated already!")
        | Some v -> v

    let inflate_node node = 
        node.result <- (node.construct node.args)

    let nodes_equal a b = 
        (a.args == b.args) && (a.constructor_guid == b.constructor_guid)

    let rec drop_until_equal prevs cur = 
        match prevs with 
        | [] -> res
        | h :: t -> if nodes_equal h cur 
            then prevs
            else drop_until_equal t prevs

    let rec map_into f res v = 
        match v with 
        | [] -> res
        | h :: t -> map_into f ((f h) :: res) t

    let rec eval_node parent prev next = 
        if nodes_equal next prev then
            prev 
        else begin
            inflate_node next;
            match parent with
            | None -> ()
            | Some parent -> T.replace_child [] parent prev next;
            let new_children = merge_children [] (Some next) (node_children prev) (node_children next) |> List.reverse in
            next.result <- {node = new_node; children = new_children};
            next
        end
    and merge_children res parent prev next =
        match (prev, next) with 
        | [], [] -> res
        | a, [] -> res
        | [], b -> map_into (eval_node parent) res b
        | ha :: ta, hb :: tb -> 
                if nodes_equal ha hb then
                    merge_children (ha :: res) ta tb
                else 
                    match drop_until_equal tb ha with
                    | [] -> map_into (eval_node parent) res b
                    | n :: t -> merge_children (n :: res) t ta

                    
    let rec inflate_tree node = 
        inflate_node node;
        List.iter (fun child ->
            inflate_tree child;
            T.append_child (node_node node) (node_node child);
        ) (node_children node)
        node
    
    let eval_tree prev next = 
        match prev with 
        | None -> inflate_tree next
        | Some prev -> eval_node None prev next



end

