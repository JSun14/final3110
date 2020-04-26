open Movable
open Block

(* Master state that is passed around in MCL *)
type state = {
  (* control cycle counter *)
  cycle_no: int;
  (* score of the player *)
  score : int;
  tanks: Movable.tank list;
  projectiles: Movable.projectile list;
  win_cond : bool
}

let print_state st = 
  (* more print statements *)
  print_endline ("Cycle No: " ^ string_of_int st.cycle_no);
  print_endline ("win: " ^ string_of_bool st.win_cond)

type world = {
  wall_list : Block.block list;
  ditch_list : Block.block list;
}

let init_state map = 
  failwith "unimplemented"

(** [is_player_dead st] is true when the player tank is dead and false
    when the player tank is alive. 

    Requires: [st] is a valid game state *)
let is_player_dead st = 
  (** [helper lst] is true when the player tank is dead and false
      when the player tank is alive. *)
  let rec helper lst = match lst with 
    | [] -> true
    | h :: t -> 
      if h.side = Enemy then helper t
      else false
  in helper st.tanks

(** [loss_condition st] is whether the player has lost the game. 
    [loss_condition st] is true when the player has lost, and false if
    the game is ongoing or the player has won. *)
let loss_condition st =
  is_player_dead st

(** [win_condition st] is whether the player has won the game. 
    It is true when the player has won the game and false if it is
    ongoing or the player has lost. 

    Requires: [st] is a valid game state. *)
let win_condition st = 
  (** [all_enemies_dead lst] is true when all the enemy tanks are inactive
      and false if at least one enemy tank still is active. *)
  let rec all_enemies_dead lst = match lst with
    | [] -> false
    | h :: t -> 
      if h.side = Self then all_enemies_dead t
      else false
  in all_enemies_dead st.tanks