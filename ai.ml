open Movable
open Interactions
open State
open Util
open Const

(** [random_mag] is either 1.0 or -1.0 with equal probability. *)
let random_mag =
  if Random.bool () then 1.0
  else -1.0

(** [cap_velocity t accel] returns true when the sum of [accel] and the 
    velocity of [t] is less than or equal to an enemy's maximum speed. *)
let cap_velocity (t : Movable.tank) accel = 
  fsum t.velocity accel |> magn <= max_enemy_speed

(* [randomize t] is a tank with a new, random acceleration added to 
   [t]'s velocity. *)
let rec randomize (t : Movable.tank) : Movable.tank =
  let x_accel = Random.float Const.player_speed in
  let y_accel = Random.float Const.player_speed in
  let accel = random_mag |> fscale (x_accel, y_accel) in
  if cap_velocity t accel then {t with velocity = fsum t.velocity accel}
  else randomize t

(** [move_all_enemies st] is a state where all enemy tanks in [st] have
    a randomized velocity.

    Requires: [st] is a valid game state.  *)
let move_all_enemies st = 
  let updated_tanks = 
    List.map (fun x -> if x.side = Enemy then randomize x else x) st.tanks in
  {st with tanks = updated_tanks}