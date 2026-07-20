open Treelib.Treedef
open Treelib.Funcs
open Format
let () =
  let tree_test_code () =
    open_vbox 1;
    let t0 = new Treelib.Treedef.tree in
      (*Test fill*)
      Format.printf "Test fill\n";
      let start = int_of_float (time_ms ()) in begin
        for i = 5 to 12 do
          t0#fill (float_of_int i);
        done;
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9 10 11 12':\t";
          print_break 3 1;
          t0#print ();
          print_break 3 1; printf "(duration, us=%d)" duration;
      end;
      print_newline (); print_newline ();
      (*Test pop*)
       Format.printf "Test pop\n";
      let start = int_of_float (time_ms ()) in begin
        t0#pop (); t0#pop (); t0#pop ();
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          t0#print ();
          print_break 3 1; printf "(duration, us=%d)" duration;
      end;
      print_newline (); print_newline ();
      (*Test convert*)
       Format.printf "Test convert\n";
      let start = int_of_float (time_ms ()) in begin
        let array = List.map unpack_pair_op (t0#convert ()) in
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          for i = 0 to (List.length array) - 1 do
            Format.printf "%f " (fst (List.nth array i));
          done;
          print_break 3 1; printf "(duration, us=%d)" duration;
      end;
      print_newline (); print_newline ();
    let t1 = ref (new Treelib.Treedef.tree) in
      (*Test operator=*)
       Format.printf "Test operator=\n";
      let start = int_of_float (time_ms ()) in
        t1 := t0;
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          (!t1)#print();
          print_break 3 1; printf "(duration, us=%d)" duration;
      print_newline (); print_newline ();   
      (*Test convert*)
       Format.printf "Test convert\n";
      let start = int_of_float (time_ms ()) in begin
        let array = List.map unpack_pair_op ((!t1)#convert ()) in
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          for i = 0 to (List.length array) - 1 do
            Format.printf "%f " (fst (List.nth array i));
          done;
          print_break 3 1; printf "(duration, us=%d)" duration;
      end;
      print_newline (); print_newline ();
      (*Test insert*)
       Format.printf "Test insert\n";
      let start = int_of_float (time_ms ()) in
        (!t1)#insert 15. 2 2;
        let duration = int_of_float (time_ms ()) - start in
          printf "unq_ should read '7': %d\t.alc_unq_ should read '7': %d\n" ((!t1)#get_unq()) ((!t1)#get_alc_unq());
          printf "This should read '5 6 7 8 9 15':\t"; print_break 3 1; (!t1)#print();
          print_break 3 1; printf "(duration, us=%d)" duration;
      print_newline (); print_newline ();
      let start = int_of_float (time_ms ()) in
        (!t1)#insert 20. 2 7;
        let duration = int_of_float (time_ms ()) - start in
          printf "unq_ should read '12': %d\t.alc_unq_ should read '13': %d\n" ((!t1)#get_unq()) ((!t1)#get_alc_unq());
          printf "This should read '5 6 7 8 9 15 20':\t"; print_break 3 1; (!t1)#print();
          print_break 3 1; printf "(duration, us=%d)" duration;
      print_newline (); print_newline ();
    let t2 = new Treelib.Treedef.tree in
      let start = int_of_float (time_ms ()) in
        t2#fill 1.; t2#insert 1. 1 2;
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '1 1'::\t"; t2#print ();
          print_break 3 1; printf "(duration, us=%d)" duration;
      print_newline (); print_newline ();
      (*test remove*)
      Format.printf "@[ %s@ @]@." "Test remove";
      let start = int_of_float (time_ms ()) in
      t2#remove 1 2;
      let duration = int_of_float (time_ms ()) - start in
        printf "This should read '1'::\t"; t2#print ();
        print_break 3 1; printf "(duration, us=%d)" duration;
      t2#insert 1. 1 2;
      print_newline (); print_newline ();
      (*test alloc_lvl*)
      printf "unq_ is %d" (t2#get_unq ());
      print_break 3 1;
      printf "alc_unq_ should read '4':\t%d\n" (t2#get_alc_unq ());
      let start = int_of_float (time_ms ()) in
        t2#alloc_lvl ();
        let duration = int_of_float (time_ms ()) - start in
          printf "unq_ is %d" (t2#get_unq ());
          print_break 3 1;
          printf "alc_unq_ should read '13'. alc_unq_=%d" (t2#get_alc_unq ());
          print_break 3 1;
          printf "(duration, us=%d)" duration;
          print_newline (); print_newline ();
       let start = int_of_float (time_ms ()) in
        t2#alloc_by_bal ();
        let duration = int_of_float (time_ms ()) - start in
          printf "alloc_by_bal performed.\t";
          printf "(duration, us=%d)" duration;
          print_newline (); print_newline ();
      let start = int_of_float (time_ms ()) in
        t2#alloc_lvl ();
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '1 1':\t"; t2#print ();
          print_break 3 1;
          printf "(duration, us=%d)" duration;
          print_newline (); print_newline ();
  in tree_test_code ()
