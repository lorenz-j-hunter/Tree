open Tree
open Stack

let test_works () =
  Alcotest.(check bool) "Does preorder crash?" true (try
    let s = ref (new stack) in
    let r = root_ 1 in preorder Test r s; true
    with _ -> false)

let test_get_results () =
  Alcotest.(check bool) "Does it get real results" true (try
    let s = ref (new stack) in
    let r = root_ 1 in preorder Test r s;
      if !s#get_size () > 0 then true else false
    with _ -> false)

let test_insert () =
  Alcotest.(check bool) "Can we insert to the tree" true (try
    let s = ref (new stack) in
    let r = root_ 1 in preorder (Insert 2) r s;
      if !s#get_size () > 0 then true else false
    with _ -> false)

let () =
  let open Alcotest in
  run "Tree" [
    "preorder", [
      test_case "doesn't crash" `Quick test_works;
      test_case "gets results" `Quick test_get_results;
    ];
  ]