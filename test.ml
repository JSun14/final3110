open OUnit2

(** [pp_string s] pretty-prints string [s]. 

    Taken from A3*)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. 
    
    Taken from A3*)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. 
    
    Taken from A2*)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

module DummyA = struct 
  (* open DummyA *)

  let tests = [
    "check dict8" >:: 
      (fun _ -> assert_equal 0 0 ~printer:string_of_int);
  ]
end 

module DummyB = struct
  (* open DummyB *)

  let tests = [
    "cdummy test" >:: (fun _ -> assert_equal 0 0 ~printer:string_of_int);
  ]
end 

let suite = "search test suite" >::: List.flatten 
  [DummyA.tests; DummyB.tests;]

let _ = run_test_tt_main suite