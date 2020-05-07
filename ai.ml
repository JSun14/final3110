open Movable
open Interactions
open State

let random_mag =
  if Random.bool () then (+.)
  else (-.)

(* take a velocity and add a random velocity (mag and direction) *)
let rec randomize (t : Movable.tank) : Movable.tank =
  let x_vel_change = Random.float Const.player_speed in
  let y_vel_change = Random.float Const.player_speed in
  {t with velocity = (t.velocity |> fst |> random_mag x_vel_change, 
                      t.velocity |> snd |> random_mag y_vel_change)}

let move_enemy_tank (t : Movable.tank) : Movable.tank =
  {t with loc = (fst t.loc +. fst t.velocity, snd t.loc +. snd t.velocity)}

let move_all_enemies lst = 
  List.map (fun x -> move_enemy_tank (randomize x)) lst