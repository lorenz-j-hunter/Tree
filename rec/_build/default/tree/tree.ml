open Stack

type 'a node = {value:'a; index:int; bf:int}

type 'a tree =
  | Leaf
  | Node of 'a node * 'a tree list

let rec fill f l bf =
  let arg = f :: l in
    match l with
    | [] -> fill f arg bf 
    | hd :: tl ->
      if List.length l = bf then l
      else fill f arg bf

let new_node ?bf:(bf=3) ?index:(index= -1) init =
  Node ({value=init; index=index; bf=bf}, (fill Leaf [] bf)) 

type 'a action =
  | Insert of 'a
  | Remove of int
  | Test
 
let rec preorder = fun (mode : 'a action) (t: 'a tree) (results: ('a tree * int) stack) ->
  match t with
  | Leaf -> begin
    match mode with
    | Insert (item : 'a) ->
      let index = results#get_size () in (* gather results *)
        let foo = ref t in foo := new_node ~index:index item; (* modify tree *)
        results#push ((new_node ~index:index item), index)
    | Remove (index : int) ->
      let foo = ref t in foo := Leaf; (* Replace node with Leaf *)
    | Test -> results#push (Leaf, -1);
  end
  | Node (n, subtrees) -> 
    results#push (Node (n, subtrees), n.index);
    for i = 0 to n.bf - 1 do
      preorder mode (List.nth subtrees i) results;
    done;
