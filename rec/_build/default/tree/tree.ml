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

let rec preorder = fun (mode : 'a action) (t: 'a tree) (results: 'a tree stack ref)  ->
  match t with
  | Leaf -> begin
    match mode with
    | Insert (item : 'a) -> !results#push (root_ item)
    | Remove (index : int) -> !results#push Leaf (* placeholder *)
    | Search (index : int) -> !results#push Leaf (* placeholder *) 
    | Test -> !results#push Leaf (* placeholder *)
  end
  | Node (n, sts, bf) ->
    for i = 0 to bf - 1 do preorder mode (List.nth sts i) results done;
