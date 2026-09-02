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
  Alcotest.(check (list int)) "Insert is logically and semantically correct." [13; 40] (try
    let rec fill = fun ret ->
      match ret with
      | [] ->
        let s = new stack in
        let r = new_node ~index:0 1 in
          s#push (r, 0); (* Include Root Node *)
          for _ = 0 to 2 do preorder (Insert 0) r s; done; (* Fill results with subtrees *)
          fill (ret @ [s#get_size ()]); (* Call recursion *)
      | hd :: [] ->
        let s = new stack in
        let r = new_node ~index:0 1 in
          s#push (r, 0); (* Include root *)
          for _ = 0 to 11 do preorder (Insert 0) r s; done; (*Fill first level, then second (bf=3)*)
          ret @ [s#get_size ()]; (*Return Result*)
      | _ -> [-1; -1]
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
      test_case "doesn't crash" `Quick test_works;
      test_case "gets results" `Quick test_get_results;
      test_case "insert doesn't crash" `Quick test_insert;
      test_case "insert functions correctly" `Quick test_insert_1;
      test_case "remove doesn't crash" `Quick test_remove;
      test_case "remove functions correctly" `Quick test_remove_1;
    ];
  ]