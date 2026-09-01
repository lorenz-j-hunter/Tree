open Stack

type 'a node = {value:'a; index:int; subtrees:'a node list}

type 'a tree =
  | Leaf
  | Node of 'a node * 'a tree list * int

let rec fill f l bf =
  let arg = f :: l in
    match l with
    | [] -> fill f arg bf 
    | hd :: tl ->
      if List.length l = bf then l
      else fill f arg bf

let root_ ?bf:(bf=3) init = Node ({value=init; index=0; subtrees=[]}, (fill Leaf [] bf), bf)

type 'a action =
  | Insert of 'a
  | Remove of int
  | Search of int
  | Test 

let rec preorder = fun (mode : 'a action) (t: 'a tree) (results: ('a tree * int) stack ref)  ->
  match t with
  | Leaf -> begin
    match mode with
    | Insert (item : 'a) -> let foo = ref t in foo := root_ item; !results#push ((root_ item), -1) 
    | Remove (index : int) -> !results#push (Leaf, index) (* placeholder *)
    | Search (index : int) -> !results#push (Leaf, index) (* placeholder *)
    | Test -> !results#push (Leaf, -1) (* placeholder *)
  end
  | Node (n, sts, bf) ->
    for i = 0 to bf - 1 do preorder mode (List.nth sts i) results done;
  match mode with
  | Search _ -> ()
  | _ -> ()
