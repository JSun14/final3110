open OUnit2
(** [pp_string s] pretty-prints string [s]. 

    Taken from A3*)
let pp_string s = "\"" ^ s ^ "\""

let pp_tuple (f : 'a -> string) (x, y) = "(" ^ (f x) ^ ", " ^ (f y) ^ ")"
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

module AiT = struct 
  open Ai
  open State 

  let map = Main.json_file_to_map "map3.json"
  let w = Main.init_world map

  let tankA : Movable.tank = 
    {loc = (3.0,3.0); 
     past_loc = (5.0,5.0); 
     velocity = (1.0,1.0);
     health = 1;
     last_fire_time = 0;
     side = Enemy}
  
  let player = {
    tankA with side = Self;
                loc = (3.0, 14.124)
  }

  let s = {
    sys_time = 0.0;
    cycle_no = 0;
    score = 0;
    tanks = [tankA; player];
    projectiles = [];
    win_cond = Playing;
  }

  let tests = [
    "check dict8" >:: 
    (fun _ -> assert_equal true (Ai.clear_los w.wall_list player tankA));
  ]
end 

module InteractionsT = struct
  (* open Interactions and Block*)
  open Interactions
  open Block
  (**Expression that calls [execute wall state] and asserts equality with the 
     expected State*)
  let move_helper
      (name:string)
      (world:State.world)
      (state:State.state)
      (exp_out:State.state)=
    name >:: (fun _ ->  assert_equal exp_out (execute world state))

  (**Expression that calls [wall_execute wall state] and asserts equality with the 
     expected State*)
  let wall_helper
      (name:string)
      (world:State.world)
      (state:State.state)
      (exp_out:State.state)=
    name >:: (fun _ -> assert_equal exp_out (wall_execute world state))

  (**Expression that calls [entity_removal_execute wall state] and asserts 
     equality with the expected State*)
  let entity_removal_helper
      (name:string)
      (world:State.world)
      (state:State.state)
      (exp_out:State.state)=
    name >:: (fun _ -> assert_equal exp_out (entity_removal_execute world state))

  let tankA : Movable.tank = 
    {loc = (5.0,5.0); 
     past_loc = (5.0,5.0); 
     velocity = (1.0,1.0);
     health = 1;
     last_fire_time = 0;
     side = Enemy}
  let tankB : Movable.tank = 
    {tankA with loc = (6.2,6.2); 
                past_loc = (6.5,6.5)}
  let projA : Movable.projectile = 
    {loc = (5.0,5.0);
     past_loc = (5.0,5.0);
     velocity = (1.0,1.0);
     health = 1;
     weap_species = Bullet}
  let projB : Movable.projectile = 
    {projA with loc = (3.0,3.0)}
  let stateA : State.state=
    {sys_time=0.0; cycle_no=0; score=0; 
     tanks=[{tankA with velocity = (2.0, 2.0)}; 
            {tankA with velocity = (-2.0, 1.0)};
            {tankA with velocity = (-2.0, -1.0)};
            {tankA with velocity = (1.0, -2.0);}];
     projectiles=[{projA with velocity = (2.0, 2.0)}; 
                  {projA with velocity = (-2.0, 1.0)};
                  {projA with velocity = (-2.0, -1.0)}; 
                  {projA with velocity = (1.0, -2.0);}];
     win_cond=Playing}
  let stateB : State.state=
    {stateA with tanks = [{tankA with loc=(4.7,5.5); 
                                      past_loc = (4.0,4.0)}; 
                          tankB];}
  let stateC : State.state=
    {stateA with tanks = [{tankA with loc = (5.5,5.5)}; 
                          {tankB with loc = (7.0,7.0)}]; 
                 projectiles = [{projA with loc = (5.25,5.25)}; 
                                projB]}
  let blockA : Block.block =
    {id = "1"; 
     kind = Wall; 
     width = 0.5; 
     coord = (5.5,5.5);}
  let worldA : State.world = 
    {wall_list = [blockA];
     ditch_list = []}
  let worldB : State.world = 
    {worldA with wall_list = []}

  let tests = [
    move_helper "Testing execute for interactions" worldA stateA
      {stateA with 
       tanks=[
         {tankA with loc = (7.0,7.0);
                     velocity = (2.0, 2.0)};
         {tankA with loc = (3.0,6.0);
                     velocity = (-2.0, 1.0)};
         {tankA with loc = (3.0,4.0);
                     velocity = (-2.0, -1.0)};
         {tankA with loc = (6.0,3.0);
                     velocity = (1.0, -2.0);};];
       projectiles = [
         {projA with loc = (7.0,7.0);
                     velocity = (2.0, 2.0)};
         {projA with loc = (3.0,6.0);
                     velocity = (-2.0, 1.0)};
         {projA with loc = (3.0,4.0);
                     velocity = (-2.0, -1.0)};
         {projA with loc = (6.0,3.0);
                     velocity = (1.0, -2.0)}]};
    wall_helper "Testing execute for walls/other tanks" worldA stateB 
      {stateB with tanks=[{tankA with loc = (4.0,4.0); 
                                      past_loc = (4.0,4.0)};
                          {tankB with loc = (6.5,6.5)}]};
    entity_removal_helper "Testing entity removal for tanks/projs" worldB stateC 
      {stateC with tanks = [{tankB with loc = (7.0,7.0)}]; 
                   projectiles = [projB]};
  ]
end 

module UtilT = struct 
  open Util

  let tests = [
    "Test range function" >:: 
    (fun _ -> assert_equal [0;1;2;3] (range 4) 
        ~printer:(pp_list string_of_int));
    "Test diff function with (0,0)" >:: 
    (fun _ -> assert_equal (1,1) (diff (1, 1) (0, 0)) 
        ~printer:(pp_tuple string_of_int));
    "Test diff function" >:: 
    (fun _ -> assert_equal (-2,1) (diff (1, 4) (3, 3)) 
        ~printer:(pp_tuple string_of_int));
    "Test diff function with itself" >:: 
    (fun _ -> assert_equal (0,0) (diff (2, -1) (2, -1)) 
        ~printer:(pp_tuple string_of_int));
    "Test fdiff function" >:: 
    (fun _ -> assert_equal (-1.0,2.0) (fdiff (3.0, 2.0) (4.0, 0.0)) 
        ~printer:(pp_tuple string_of_float));
    "Test fsum function" >:: 
    (fun _ -> assert_equal (3.0, 3.1) (fsum (1.0, 1.0) (2.0, 2.1)) 
        ~printer:(pp_tuple string_of_float));
    "Test mult_inv function" >:: 
    (fun _ -> assert_equal 0.5 (mult_inv 2.0) ~printer:(string_of_float));
    "Test magn function" >:: 
    (fun _ -> assert_equal 5.0 (magn (3.0, 4.0)) ~printer:(string_of_float));
    "Test fscale function" >:: 
    (fun _ -> assert_equal (3.0, -4.0) (fscale (1.5, -2.0) 2.0) 
        ~printer:(pp_tuple string_of_float));
    "Test fscale function" >:: 
    (fun _ -> assert_equal (0.0, -4.5) (fscale (0.0, 1.5) (-3.0)) 
        ~printer:(pp_tuple string_of_float));
    "Test fcompare function when true" >:: 
    (fun _ -> assert_equal true (fcompare (-3.9) (1.3 *. -3.0)) 
        ~printer:(string_of_bool));
    "Test fcompare function when false" >:: 
    (fun _ -> assert_equal false (fcompare 4.0 4.2) ~printer:(string_of_bool));
    "Test pfloor function" >:: 
    (fun _ -> assert_equal (1, 2) (pfloor (1.5, 2.8)) 
        ~printer:(pp_tuple string_of_int));
    "Test pfloor function" >:: 
    (fun _ -> assert_equal (-1, 10) (pfloor (-0.5, 10.1)) 
        ~printer:(pp_tuple string_of_int));
    "Test pfloor function" >:: 
    (fun _ -> assert_equal (2, 0) (pfloor (2.0, 0.0)) 
        ~printer:(pp_tuple string_of_int));
    "Test comp_pair function when true" >:: 
    (fun _ -> assert_equal true (comp_pair (1.0, 2.0) (1.0, 2.0)) 
        ~printer:(string_of_bool));
    "Test comp_pair function when false" >:: 
    (fun _ -> assert_equal false (comp_pair (1.0, 2.0) (1.0, -2.0)) 
        ~printer:(string_of_bool));
    "Test comp_pair function when false" >:: 
    (fun _ -> assert_equal false (comp_pair (1.0, 2.0) (1.0, 2.00000001)) 
        ~printer:(string_of_bool));
    "Test comp_pair_s function" >:: 
    (fun _ -> assert_equal 0 (comp_pair_s (1.0, 2.0) (1.0, 2.0)) 
        ~printer:(string_of_int));
    "Test comp_pair_s function" >:: 
    (fun _ -> assert_equal ~-1 (comp_pair_s (0.0, 15.0) (1.0, -7.0)) 
        ~printer:(string_of_int));
    "Test comp_pair_s function" >:: 
    (fun _ -> assert_equal 1 (comp_pair_s (3.1, -0.5) (2.9, 1.1)) 
        ~printer:(string_of_int));
    "Test comp_pair_s function" >:: 
    (fun _ -> assert_equal 1 (comp_pair_s (-6.2, 1.0) (-6.2, 1.1)) 
        ~printer:(string_of_int));
    "Test unit_vec function" >:: 
    (fun _ -> assert_equal (0.6, 0.8) (unit_vec (3.0, 4.0)) 
        ~cmp:(compare_pairs) ~printer:(pp_tuple string_of_float));
  ]
end 

let suite = "We Play Tanks test suite" >::: List.flatten 
              [AiT.tests; InteractionsT.tests; UtilT.tests]

let _ = run_test_tt_main suite