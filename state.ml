open Movable
open Block

type progress = Playing | Win | Loss
(* Master state that is passed around in MCL *)
type state = {
  (* control cycle counter *)
  cycle_no: int;
  (* score of the player *)
  score : int;
  tanks: Movable.tank list;
  projectiles: Movable.projectile list;
  win_cond : progress
}

(** [string_of_progress prog] is a string value of [prog] *)
let string_of_progress (prog : progress) : string = match prog with
  | Playing -> "playing"
  | Win -> "won"
  | Loss -> "lost"

(** [print_state st] prints information about the state of the game
    including the cycle number and the progress of [st] 

    Requires: [st] is a valid game state. *)
let print_state st = 
  (* more print statements *)
  print_endline ("Cycle No: " ^ string_of_int st.cycle_no);
  print_endline ("win: " ^ string_of_progress st.win_cond)

type world = {
  wall_list : Block.block list;
  ditch_list : Block.block list;
}

(** [win_condition st] is whether the player has won the game. 
    It is true when the player has won the game and false if it is
    ongoing or the player has lost. 

    Requires: [st] is a valid game state. *)
let win_condition st = 
  (** [all_enemies_dead lst] is true when all the enemy tanks are inactive
      and false if at least one enemy tank still is active. *)
  let self_tank = List.exists (fun x -> x.side = Self) st.tanks in
  let enemy_tank = (List.exists (fun x -> x.side = Enemy) st.tanks) in
  if not self_tank then Loss
  else if not enemy_tank then Win
  else Playing

(** [print_tank_info st] prints information about every living tank
    in [st]

    Requires: [st] is a valid game state. *)
let print_tank_info st = 
  let rec helper lst = match lst with
    | [] -> print_endline "________________________"
    | h :: t -> Movable.tank_info h; helper t 
  in helper st.tanks

(** [print_proj_info st] prints information about every viable projectile
    in [st]

    Requires: [st] is a valid game state.  *)
let print_proj_info st =
  let rec helper lst = match lst with 
    | [] -> print_endline "________________________"
    | h :: t -> Movable.proj_info h; helper t 
  in helper st.projectiles

(** [get_player_tank st_stank_list] is the player tank

    Requires: [st_tank_list] is a valid game state tank list*)
let get_player_tank st_tank_list = 
  let player_list = List.filter (fun x -> x.side = Self) st_tank_list in
  if List.length player_list = 0 then failwith "player tank dead"
  else List.hd player_list

(** [update_tl_player old_tank_list new_player_tank] is a new tank list 
    with the player replaced by the new player*)
let update_tl_player old_tank_list new_player_tank =
  let enemies = List.filter (fun x -> x.side = Enemy) old_tank_list in
  new_player_tank::enemies

let update_tl_enemies old_tank_list new_enemies =
  let player = get_player_tank old_tank_list in 
  player::new_enemies

(* (** [player_loc st] is the location of the player tank on the map
    as a tuple of floats 

    Requires: [st] is a valid game state *)
   let player_loc st = 
   (get_player_tank st).loc  *)

let get_enemy_tanks st_tank_list = 
  List.filter (fun x -> x.side = Enemy) st_tank_list