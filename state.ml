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

let is_player_dead st = 
  let rec helper lst = match lst with 
    | [] -> true
    | h :: t -> 
      if h.side = Enemy then helper t
      else false
  in helper st.tanks
