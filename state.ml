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
}

type world = {
  wall_list : Block.block list;
  ditch_list : Block.block list;
}