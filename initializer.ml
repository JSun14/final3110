

open Read_json
open Block
open State

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
let init_world (map:Read_json.t) = {
  wall_list = map.wall_list;
  ditch_list = map.ditch_list;
}

(**[json_file_to_map] reads in a json file*)
let json_file_to_map f =
  try let json = f |> Yojson.Basic.from_file in
    json |> Read_json.from_json
  with e ->      
    ANSITerminal.(print_string [red]
                    "\n\nInvalid Map Name. Please try.\n");
    Stdlib.exit 0