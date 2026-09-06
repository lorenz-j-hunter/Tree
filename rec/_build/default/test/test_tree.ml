open Tree
open Stack

let test_works () =
  Alcotest.(check bool) "Preorder is free from runtime errors." true (try
    let s = new stack in
    let r = new_node ~index:0 1 in preorder Test r s; true
    with _ -> false)

let test_get_results () =
  Alcotest.(check bool) "Preorder is logically and semantically correct." true (try
    let s = new stack in
    let r = new_node ~index:0 1 in preorder Test r s;
      if s#get_size () > 0 then true else false
    with _ -> false)

let test_insert () =
  Alcotest.(check bool) "Insert is free from runtime errors." true (try
    let s = new stack in
    let r = new_node ~index:0 1 in preorder (Insert 2) r s;
      if s#get_size () > 0 then true else false
    with _ -> false)

let test_insert_1 () =
  Alcotest.(check (list int)) "No logic errors" [13; 40; 121] (try
    let rec fill = fun ret ->
      match ret with
      | [] ->
        let r = new_node ~index:0 1 in
        let results = ref (new stack) in
          for i = 0 to 2 do
            let s = new stack in
              preorder (Insert 0) r s;
              if i = 2 then results := s;
          done; (* Fill results with subtrees *)
          fill (ret @ [!results#get_size ()])
      | hd :: [] ->
        let r = new_node ~index:0 1 in
        let results = ref (new stack) in
          for i = 0 to 11 do
            let s = new stack in
              preorder (Insert 0) r s;
              if i = 11 then results := s; (* Call recursion *)
          done; (* Fill results with subtrees *)
          fill (ret @ [!results#get_size ()]); (* Call recursion *)
      | fst :: snd :: [] ->
        let r = new_node ~index:0 1 in
        let results = ref (new stack) in
          for i = 0 to 39 do
            let s = new stack in
              preorder (Insert 0) r s;
              if i = 39 then results := s;
          done; (* Fill results with subtrees *)
          fill (ret @ [!results#get_size ()]); (* Call recursion *)
      | _ -> ret
      in fill []
    with _ -> [-1; -1])

let test_remove () =
  Alcotest.(check bool) "Removal is free from runtime errors" true (try
    let s = new stack in
    let r = new_node ~index:0 1 in
      for _ = 0 to 4 do preorder (Insert 0) r s; done;
    true
    with _ -> false)

let test_remove_1 () =
  Alcotest.(check int) "Removal is logically and semantically correct" 4 (try
    let s = new stack in
    let r = new_node ~index:0 1 in
      for _ = 0 to 4 do preorder (Insert 0) r s; done;
    preorder (Remove 4) r s;
    s#get_size ()
    with _ -> -1)

let () =
  let open Alcotest in
  run "Tree" [
    "preorder", [
      test_case "Test" `Quick test_works;
      test_case "Test" `Quick test_get_results;
      test_case "Insert" `Quick test_insert;
      test_case "Insert" `Quick test_insert_1;
      test_case "Remove" `Quick test_remove;
      test_case "Remove" `Quick test_remove_1;
    ];
  ]