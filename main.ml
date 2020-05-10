open State
open Render
open Interactions
open Input
open Process
open Read_json
open Ai 
open Const 

(* eventually needs to do something with w *)
let init_state map = {
  sys_time = 0.0;
  cycle_no = 0; 
  score = 100 * 100 * 10;
  tanks = map.tank_list;
  projectiles = [];
  win_cond = Playing;
}

(* eventually needs to load world from json *)
let init_world map = {
  wall_list = map.wall_list;
  ditch_list = map.ditch_list;
}

(** [waiter ref_time] recusrively calls itself and waits until 
[Const.cycle_time] has elpased since [ref_time]. *)
let rec waiter ref_time = 
  if Unix.gettimeofday () -. ref_time > Const.cycle_time then 
    () 
  else begin
    Unix.sleepf(0.0001); 
    waiter ref_time
  end

(** [debug w st] prints debug information about the game state as a 
singly togglable function. *)
let debug w st =
  State.print_state st;
  State.print_tank_info st;
  State.print_proj_info st

let rec game_helper (w:State.world) (st:State.state) =
  (* print debug info about game state *)
  (* let () = debug w st in  *)

  (* if Unix.gettimeofday () -. st.sys_time > Const.cycle_time then
    print_endline "WARNING YOUR MACHINE IS SLOW"; *)

  (* delay until cycle_time has elpased *)
  let () = waiter st.sys_time in 

  (* set control cycle time *)
  let s = {
    st with sys_time = Unix.gettimeofday ();
  } in

  let u_in = Input.get_user_in () in
  (* let _ = print_user_in u_in in *)
  let s = Process.process_u_in s u_in in
  let s = Ai.attempt_shoot_map w s in 
  let s = Ai.move_all_enemies s in
  let s = Interactions.execute w s in
  let s = Interactions.wall_execute w s in 
  let s = Interactions.entity_removal_execute w s in 

  (* step forward top level state *)
  let s = {
    s with cycle_no = s.cycle_no + 1; 
              score = s.score - 1;
           win_cond = State.win_condition s;
  } in 
  let final_state = {
    s with score = if s.win_cond = Loss 
      then s.score - Const.on_death_subtraction
      else s.score
  } in

  let () = Render.render_frame w final_state in match final_state.win_cond with
  | Playing -> game_helper w final_state
  | Win -> 
    ANSITerminal.(print_string [green] "\n\nYou Won! Great job!\n");
    ANSITerminal.(print_string [green] 
      ("Your final score is: " ^ (string_of_int final_state.score) ^ "\n"));
    Stdlib.exit 0
  | Loss ->       
    ANSITerminal.(print_string [red] "\n\nYou Lost! Please try again.\n");
    ANSITerminal.(print_string [red] 
      ("Your final score is: " ^ (string_of_int final_state.score) ^ "\n"));
    Stdlib.exit 0

let json_file_to_map f =
  try let json = f |> Yojson.Basic.from_file in
    json |> Read_json.from_json
  with e ->      
    ANSITerminal.(print_string [red]
      "\n\nInvalid Map Name. Please try.\n");
    Stdlib.exit 0

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Tank Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  let fn =
    match read_line () with
    | exception End_of_file ->
      ANSITerminal.(print_string [red]
        "\n\nInvalid User Input. Please try.\n");
      Stdlib.exit 0
    | file_name -> file_name in
  let () = match start_rend () with
    | exception e -> 
      ANSITerminal.(print_string [red]
        "\n\nWindow not found. 
        Please start a valid X Server to render the game.\n");
      Stdlib.exit 0
    | _ -> ()
    in
  let map = json_file_to_map fn in
  let w = init_world map in
  let s0 = init_state map in
  game_helper w s0

(* Execute the game engine. *)
let () = main ()