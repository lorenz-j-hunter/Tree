open Treelib.Treedef
open Printf
let () =
  let tree_test_code () =
    let t0 = new Treelib.Treedef.tree in
      Format.open_box 1;
      let start = int_of_float (Sys.time ()) in begin
        for i = 5 to 12 do
          t0#append (float_of_int i);
        done;
        let end_ = int_of_float (Sys.time ()) in
          printf "This should read '5 6 7 8 9 10 11 12':";
          t0#print ();
          printf "(duration=%d)" (end_-start);
      end;
      Format.close_box ();
    
  in tree_test_code ()
