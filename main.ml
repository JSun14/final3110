open State
open Render

(* eventually needs to do something with w *)
let init_state w = {
  cycle_no = 0; 
  score = 0;
  tanks = [];
  projectiles = [];
  win_cond = false;
}

(* eventually needs to load world from json *)
let init_world = {
  wall_list = [];
  ditch_list = [];
}

let rec game_helper w st =
(* print debug info about game state *)
  print_state st;
(* let s1 = Get_user_in.execute world state *)
(* let s2 = Physics.execute world s1*)
(* let s3 = Interactions.excute world s2 *)
(* Render.execute world s3 *)
  
  Unix.sleepf(0.01);

  let final_state = {
    st with cycle_no = st.cycle_no + 1
  } in 

  let () = Render.render_frame in
  if final_state.win_cond then 
    print_endline "GG EZ"
  else
    game_helper w final_state

  (* sleep for 1/100 of second *)

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Tank Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  (* match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name *)
  start_rend;

  let w = init_world in
  let s0 = init_state w in
  game_helper w s0

(* Execute the game engine. *)
let () = main ()