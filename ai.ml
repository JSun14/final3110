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

(**[clear_los w player enemy] is true if there's a clear los to player*)
let clear_los w player enemy =
  false

(** [] returns a projectile option  *)
let can_shoot ccno tank = 
  ccno - tank.last_fire_time > Const.standard_reload

let attempt_shoot w ccno player enemy =
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

