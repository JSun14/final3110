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
  State.print_tank_info st;

  Unix.sleepf(0.005);

  let u_in = Input.get_user_in () in
  let _ = print_user_in u_in in
  let s2 = Process.process_u_in st u_in in
  let s3 = Interactions.execute w s2 in
  let s4 = Interactions.wall_execute w s3 in 
  (* Render.execute world s3 *)

  let final_state = {
    s4 with cycle_no = s4.cycle_no + 1
  } in 

  let _ = Render.render_frame w final_state in
  if final_state.win_cond then 
    print_endline "GG EZ"
  else
    game_helper w final_state

(* sleep for 1/100 of second *)

let json_file_to_map f =
  try let json = f |> Yojson.Basic.from_file in
    json |> Read_json.from_json
  with e -> failwith "invalid game file"

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Tank Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  let fn =
    match read_line () with
    | exception End_of_file -> failwith "BAD FILE NAME"
    | file_name -> file_name in
  let () = start_rend in
  let map = json_file_to_map fn in
  let w = init_world map in
  let s0 = init_state map in
  game_helper w s0

(* Execute the game engine. *)
let () = main ()