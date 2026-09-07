open Stack

type 'a node = {value:'a; index:int; bf:int}

type 'a tree_node = 
  | Leaf
  | Node of 'a node * 'a tree_node list 

type 'a tree = {bf: int; the_tree : 'a tree_node}

let rec fill f l bf =
  let arg = f :: l in
    match l with
    | [] -> fill f arg bf 
    | hd :: tl ->
      if List.length l = bf then l
      else fill f arg bf

let new_tree ?bf:(bf=3) ?index:(index= -1) init =
  {
    bf=bf;
    the_tree=Node ({value=init; index=index; bf=bf}, (fill Leaf [] bf))
  }

let new_node ?bf:(bf=3) ?index:(index= -1) init =
  Node ({value=init; index=index; bf=bf}, (fill Leaf [] bf))


type 'a action =
  | Insert of 'a
  | Remove of int
  | Test

(*A function which performs preorder insertion or removal from a tree.

mode : 'a action -> Do you want to insert or remove?

t : 'a tree -> A tree to act on.

results : ('a tree * int) stack ref -> A collection of every node which was traversed.
In other words, results#get_size () = size of the tree.
*)
let rec preorder = fun ~(mode : 'a action) ~(t : 'a tree) ~(results : ('a tree_node * int) stack) ->
  match t.the_tree with
  | Leaf -> begin
    match mode with
    | Insert (item : 'a) -> begin
      let index = results#get_size () in
        let foo = ref t.the_tree in foo := new_node ~index:index ~bf:t.bf item; (* modify tree *)
    end
    | Remove (index : int) ->
      let foo = ref t.the_tree in foo := Leaf; (* Replace node with Leaf *)
    | Test -> results#push (Leaf, -1)
  end
  | Node (n, subtrees) ->
    let args = List.map (fun x -> {bf=t.bf; the_tree=x}) subtrees in
    List.iter (fun subtree -> preorder ~mode ~t:subtree ~results) args;