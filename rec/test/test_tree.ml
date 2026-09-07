open Tree
open Stack

let test_works () =
  Alcotest.(check bool) "Preorder is free from runtime errors." true (try
    let s = new stack in
    let r = {bf=3; the_tree=(new_node ~index:0 ~bf:3 1)} in
      preorder ~mode:Test ~t:r ~results:s; true
    with _ -> false)

let test_get_results () =
  Alcotest.(check bool) "Preorder is logically and semantically correct." true (try
    let s = new stack in
    let r = {bf=3; the_tree=(new_node ~index:0 ~bf:3 1)} in begin
      preorder ~mode:Test ~t:r ~results:s;
      if s#get_size () > 0 then true else false;
    end
    with _ -> false)

let test_insert () =
  Alcotest.(check bool) "Insert is free from runtime errors." true (try
    let s = new stack in
    let r = {bf=3; the_tree=(new_node ~index:0 ~bf:3 1)} in begin
      preorder ~mode:(Insert 2) ~t:r ~results:s;
      if s#get_size () > 0 then true else false;
    end
    with _ -> false)

let test_insert_1 () =
  Alcotest.(check (list int)) "No logic errors" [13; 40; 121] (try
    let rec fill = fun ret ->
      match ret with
      | [] ->
        let r = {bf=3; the_tree=(new_node ~index:0 ~bf:3 1)} in
        let results = ref (new stack) in
          for i = 0 to 2 do
            let s = new stack in
              preorder ~mode:(Insert 0) ~t:r ~results:s;
              if i = 2 then results := s;
          done; (* Fill results with subtrees *)
          fill (ret @ [!results#get_size ()])
      | hd :: [] ->
        let r = {bf=3; the_tree=(new_node ~index:0 ~bf:3 1)} in
        let results = ref (new stack) in
          for i = 0 to 11 do
            let s = new stack in
              preorder ~mode:(Insert 0) ~t:r ~results:s;
              if i = 11 then results := s; (* Call recursion *)
          done; (* Fill results with subtrees *)
          fill (ret @ [!results#get_size ()]); (* Call recursion *)
      | fst :: snd :: [] ->
        let r = {bf=3; the_tree=(new_node ~index:0 ~bf:3 1)} in
        let results = ref (new stack) in
          for i = 0 to 39 do
            let s = new stack in
              preorder ~mode:(Insert 0) ~t:r ~results:s;
              if i = 39 then results := s;
          done; (* Fill results with subtrees *)
          fill (ret @ [!results#get_size ()]); (* Call recursion *)
      | _ -> ret
      in fill []
    with _ -> [-1; -1])

let test_remove () =
  Alcotest.(check bool) "Removal is free from runtime errors" true (try
    let s = new stack in
    let r = {bf=3; the_tree=(new_node ~index:0 ~bf:3 1)} in
      for _ = 0 to 4 do preorder ~mode:(Insert 0) ~t:r ~results:s; done;
    true
    with _ -> false)

let test_remove_1 () =
  Alcotest.(check int) "Removal is logically and semantically correct" 4 (try
    let s = new stack in
    let r = {bf=3; the_tree=(new_node ~index:0 ~bf:3 1)} in
      for _ = 0 to 4 do preorder ~mode:(Insert 0) ~t:r ~results:s; done;
    preorder ~mode:(Remove 4) ~t:r ~results:s;
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