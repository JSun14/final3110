open Movable
open Interactions
open State
open Util
open Const

let random_mag =
  if Random.bool () then 1.0
  else -1.0

let cap_velocity (t : Movable.tank) accel = 
  fsum t.velocity accel |> magn <= max_enemy_speed

(* take a velocity and add a random velocity (mag and direction) *)
let rec randomize (t : Movable.tank) : Movable.tank =
  let x_accel = Random.float Const.player_speed in
  let y_accel = Random.float Const.player_speed in
  let accel = random_mag |> fscale (x_accel, y_accel) in
  if cap_velocity t accel then {t with velocity = fsum t.velocity accel}
  else randomize t

let move_enemy_tank (t : Movable.tank) : Movable.tank =
  {t with loc = (fst t.loc +. fst t.velocity, snd t.loc +. snd t.velocity)}

let move_all_enemies lst = 
  List.map (fun x -> move_enemy_tank (randomize x)) lst