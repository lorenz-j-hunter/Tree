open Tree
open Stack

let test_works () =
  Alcotest.(check bool) "Does preorder crash?" true (try
    let s = new stack in
    let r = new_node ~index:0 1 in preorder Test r s; true
    with _ -> false)

let test_get_results () =
  Alcotest.(check bool) "Does it get real results" true (try
    let s = new stack in
    let r = new_node ~index:0 1 in preorder Test r s;
      if s#get_size () > 0 then true else false
    with _ -> false)

let test_insert () =
  Alcotest.(check bool) "Can we insert to the tree" true (try
    let s = new stack in
    let r = new_node ~index:0 1 in preorder (Insert 2) r s;
      if s#get_size () > 0 then true else false
    with _ -> false)

let test_remove () =
  Alcotest.(check bool) "Can we remove from the tree" true (try
    let s = new stack in
    let r = new_node ~index:0 1 in
      for _ = 0 to 4 do preorder (Insert 0) r s; done;
    true
    with _ -> false)

let test_remove_1 () =
  Alcotest.(check bool) "Can we remove from the tree" true (try
    let s = new stack in
    let r = new_node ~index:0 1 in
      for _ = 0 to 4 do preorder (Insert 0) r s; done;
    preorder (Remove 4) r s;
    if s#get_size () = 4 then true else false
    with _ -> false)

let () =
  let open Alcotest in
  run "Tree" [
    "preorder", [
      test_case "doesn't crash" `Quick test_works;
      test_case "gets results" `Quick test_get_results;
      test_case "insert doesn't crash" `Quick test_insert;
      test_case "remove doesn't crash" `Quick test_remove;
      test_case "remove functions correctly" `Quick test_remove_1;
    ];
  ]