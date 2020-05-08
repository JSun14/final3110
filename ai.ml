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
  let x_accel = (Random.float 1.0) *. 2.0 |> (-.) 1.0 in
  let y_accel = (Random.float 1.0) *. 2.0 |> (-.) 1.0 in
  let magnitude = max_enemy_speed in
  let accel = fscale ((x_accel, y_accel) |> unit_vec) magnitude in
  if cap_velocity t accel then {t with velocity = fsum t.velocity accel}
  else randomize t

(** [move_all_enemies st] is a state where all enemy tanks in [st] have
    a randomized velocity.

    Requires: [st] is a valid game state.  *)
let move_all_enemies st = 
  let updated_tanks = 
    List.map (fun x -> if x.side = Enemy then randomize x else x) st.tanks in
  {st with tanks = updated_tanks}

(**[clear_los w player enemy] is true if there's a clear los to player*)
let clear_los w (player:Movable.tank) (enemy:Movable.tank) =
  let target_vec_hat = fdiff player.loc enemy.loc |> unit_vec in 
  let tiny_dt = fscale target_vec_hat 0.01 in 
  false

(** [] returns a projectile option  *)
let can_shoot ccno tank = 
  ccno - tank.last_fire_time > Const.standard_reload

let attempt_shoot w ccno (player:Movable.tank)  (enemy:Movable.tank) =
  let fire = can_shoot ccno enemy in 
  if fire && clear_los w player enemy then 
    let new_tank = {
      enemy with last_fire_time = ccno;
    } in
    Some (make_bouncy (1.0,1.0) (1.0, 1.0), new_tank)
  else
    None

(* generates list of projectile options *)
let attempt_shoot_map w st=
  let player = get_player_tank st.tanks in 
  let enemy_tank_list = get_enemy_tanks st.tanks in
  let proj_tank_list = List.filter_map (fun t -> attempt_shoot w st.cycle_no player t) enemy_tank_list in 
  let (new_projs, new_tanks) = List.split proj_tank_list in 
  {
    st with projectiles = st.projectiles @ new_projs;
            tanks = st.tanks @ new_tanks;
  }

