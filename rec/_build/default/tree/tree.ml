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

(*A function which performs preorder insertion or removal from a tree.

mode : 'a action -> Do you want to insert or remove?

t : 'a tree -> A tree to act on.

results : ('a tree * int) stack ref -> A collection of every node which was traversed.
In other words, !results#get_size () = size of the tree.
*)
let rec preorder_unwrapped = fun (mode : 'a action) (t: 'a tree) (results: ('a tree * int) stack ref) (quit : bool ref) ->
  match t with
  | Leaf -> begin
    match mode with
    | Insert (item : 'a) -> begin
      let index = !results#get_size () in
        let foo = ref t in begin
          foo := new_node ~index:index item; (* modify tree *)
          !results#push ((new_node ~index:index item), index); (*Include current node in !results.*)
          List.iter (!results#push) (fill (Leaf, -1) [] 3); (*Include leaves in !results.*)
        end
    end
    | Remove (index : int) ->
      let foo = ref t in foo := Leaf; (* Replace node with Leaf *)
    | Test -> !results#push (Leaf, -1)
  end
  | Node (n, subtrees) ->
    let data = (!results#get_size (), ref 0) in begin 
      while not !quit do begin
        incr (snd data);
        if fst data < !results#get_size () then
          quit := true (*if we have inserted, then quit*)
        else if !(snd data) > n.bf - 1 then
          quit := true (*if we are at the end of a subtrees, quit.*)
        else begin
          preorder_unwrapped mode (List.nth subtrees !(snd data)) results quit; (*Recursive call*)
          let count_this_node = Node (n, subtrees) in !results#push (count_this_node, n.index);
        end;
      end done
    end

let preorder = fun (mode : 'a action) (t : 'a tree) (results : ('a tree * int) stack) ->
  preorder_unwrapped mode t (ref results) (ref false);