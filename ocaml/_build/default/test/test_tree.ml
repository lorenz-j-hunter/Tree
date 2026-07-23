open Treelib.Treedef
open Treelib.Funcs
open Format
let () =
  let tree_test_code () =
    printf "@;<0 6>tree_test_code ()"; print_newline ();
    let t0 = new Treelib.Treedef.tree in
      printf "@;<0 9>t0 = new Treelib.Treedef.tree@."; print_newline ();
      (*Test fill*)
      printf "@;<0 9>@[(%s" "Test fill"; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>("; print_newline ();
        for i = 5 to 12 do t0#fill (float_of_int i); printf "@;<0 12>%s" "t0#fill"; print_newline (); done;
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9 10 11 12':"; print_newline ();
          printf "@;<0 15>"; t0#print (); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)"; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      (*Test pop*)
      Format.printf "@;<0 9>@[(%s" "Test pop"; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>("; for i = 1 to 3 do t0#pop (); printf "@;<0 12>t0#pop ()"; print_newline (); done;
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9':\t"; print_newline ();
          printf "@;<0 15>"; t0#print (); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)"; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      (*Test convert*)
      Format.printf "@;<0 9>@[(%s" "Test convert"; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "List.map unpack_pair_op (t0#convert ())"; print_newline ();
        let array = List.map unpack_pair_op (t0#convert ()) in
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9':"; print_newline ();
          printf "@;<0 15>"; for i = 0 to (List.length array) - 1 do printf "%f " (fst (List.nth array i)); done; print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
    printf "@;<0 6>)"; printf "@;<0 6>(";
    let t1 = ref (new Treelib.Treedef.tree) in
      (*Test operator=*)
      Format.printf "@;<0 9>@[(%s" "Test operator="; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "t0 := t0"; t1 := t0; print_newline ();
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9':"; print_newline ();
          printf "@;<0 15>"; (!t1)#print(); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      (*Test convert*)
      Format.printf "@;<0 9>@[(%s" "Test convert"; print_newline ();
      let start = int_of_float (time_us ()) in 
        printf "@;<0 12>(%s" "List.map unpack_pair_op ((!tl)#convert ())"; print_newline ();
        let array = List.map unpack_pair_op ((!t1)#convert ()) in
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9':"; print_newline ();
          printf "@;<0 15>"; for i = 0 to (List.length array) - 1 do Format.printf "%f " (fst (List.nth array i)); done; print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      (*Test insert*)
      Format.printf "@;<0 9>@[(%s" "Test insert"; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "(!t1)#insert 15. 2 2"; (!t1)#insert 15. 2 2; print_newline ();
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline (); 
          printf "@;<0 15>unq_ should read '7': %d\t.alc_unq_ should read '7': %d\n" ((!t1)#get_unq()) ((!t1)#get_alc_unq()); print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9 15':"; print_newline (); 
          printf "@;<0 15>"; (!t1)#print(); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      printf "@;<0 9>@["; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "(!t1)@insert 20. 2 7"; (!t1)#insert 20. 2 7; print_newline ();
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>unq_ should read '12': %d\t.alc_unq_ should read '13': %d\n" ((!t1)#get_unq()) ((!t1)#get_alc_unq()); print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9 15 20':"; print_newline ();
          printf "@;<0 15>"; (!t1)#print(); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
    printf "@;<0 6>)"; print_newline ();
    printf "@;<0 6>(%s" "t2 = new Treelib.Treedef.tree"; print_newline ();
    let t2 = new Treelib.Treedef.tree in
      printf "@;<0 9>@[("; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "t2#fill 1.; t2#insert 1. 1 2"; t2#fill 1.; t2#insert 1. 1 2; print_newline ();
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '1 1':"; print_newline ();
          printf "@;<0 15>"; t2#print (); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline (); 
      printf "@;<0 15>@])"; print_newline ();
      (*test remove*)
      Format.printf "@;<0 9>(@[<v 0>%s" "Test remove"; print_newline (); 
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>("; printf "t2#remove 1 2"; print_newline ();
        t2#remove 1 2;
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; 
          printf "This should read '1.00':\t"; t2#print (); printf "(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)"; print_newline (); 
        printf "@;<0 12>)"; print_newline ();
      t2#insert 1. 1 2; Format.printf "@;<0 9>@])"; print_newline ();
      (*test alloc_lvl*)
      Format.printf "@;<0 9>(@[<v 0>%s" "Test alloc_lvl"; print_newline (); 
      printf "@;<0 9>"; printf "alc_unq_ should read '4':\t%d\n" (t2#get_alc_unq ()); print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "t2#alloc_lvl ()"; t2#alloc_lvl (); print_newline ();
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; printf "unq_ is %d" (t2#get_unq ()); print_newline (); 
          printf "@;<0 15>"; printf "alc_unq_ should read '13'. alc_unq_=%d" (t2#get_alc_unq ()); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)";
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      Format.printf "@;<0 9>(@[<v 0>%s" "alloc_by_bal"; print_newline (); 
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "t2#alloc_by_bal ()"; t2#alloc_by_bal (); print_newline ();
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)"; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      Format.printf "@;<0 9>(@[<v 0>%s" "alloc_lvl ()"; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "t2#alloc_lvl ()"; t2#alloc_lvl (); print_newline ();
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>(%s" "This should read '1 1':\t"; t2#print (); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
  in tree_test_code ()
