open Movable
open Interactions

(* take a velocity and add a random velocity (mag and direction) *)
let randomize (t : Movable.tank) : Movable.tank =
  let x_vel_change = Random.float Const.player_speed in
  let y_vel_change = Random.float Const.player_speed in
  {t with velocity = (t.velocity |> fst |> (+.) x_vel_change, 
                      t.velocity |> snd |> (+.) y_vel_change)}