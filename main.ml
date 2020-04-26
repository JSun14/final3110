open State
open Render
open Interactions
open Input
open Process
open Read_json

(* eventually needs to do something with w *)
let init_state map = {
  cycle_no = 0; 
  score = 0;
  tanks = map.tank_list;
  projectiles = [];
  win_cond = false;
}

(* eventually needs to load world from json *)
let init_world map = {
  wall_list = map.wall_list;
  ditch_list = map.ditch_list;
}

let rec game_helper w st =
(* print debug info about game state *)
  print_state st;
  Render.clear;

  Unix.sleepf(0.01);

  let u_in = Input.get_user_in () in
  let _ = print_user_in u_in in
  let s2 = Process.process_u_in st u_in in
  let s3 = Interactions.execute w s2 in
(* Render.execute world s3 *)

  let final_state = {
    s3 with cycle_no = st.cycle_no + 1
  } in 

  let _ = Render.render_frame final_state in
  if final_state.win_cond then 
    print_endline "GG EZ"
  else
    game_helper w final_state

  (* sleep for 1/100 of second *)

let json_file_to_map f =
  let json = f |> Yojson.Basic.from_file in
    json |> Read_json.from_json

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Tank Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  (* match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name *)
  start_rend;

  let w = init_world (json_file_to_map "map0.json") in
  let s0 = init_state (json_file_to_map "map0.json") in
  game_helper w s0

(* Execute the game engine. *)
let () = main ()