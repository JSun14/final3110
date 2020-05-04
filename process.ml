open Input
open State
open Movable

open Util

let player_speed = 0.1
let input_scale = 5.0

(**[set_player_vel player u] sets the player tank's velocity according to keys pressed in u*)
let set_player_vel (player:Movable.tank) u =
    let x0 = match u.a, u.d with
    | true, false -> -1.0
    | false, true -> 1.0
    | _ -> 0.0 in
    let y0 = match u.w, u.s with
    | true, false -> 1.0
    | false, true -> -1.0
    | _ -> 0.0 in
    {
        player with velocity = (player_speed *. x0, player_speed *. y0)
    }

(* calc diff between tank pos and *)
let gen_bullet st u =
    let player = get_player_tank st.tanks in 
    let target_vec_hat = fdiff u.m_pos player.loc |> unit_vec in 
    let bull_vel = scale target_vec_hat Movable.standard_vel in
    let spawn_loc = fsum player.loc (scale target_vec_hat (Movable.tank_rad +. Util.eps)) in 
    (make_bullet spawn_loc bull_vel)::st.projectiles

(** [generate_palyer_proj player u] spawns a projectile *)
let player_shoot st u =
    let player = get_player_tank st.tanks in
    (* 5 is a hard coded min reload time *)
    let shoot = st.cycle_no - player.last_fire_time > 5 &&
    u.lmb in 
    if shoot then gen_bullet st u else st.projectiles

(**[process_u_in st u] sets velocities and spawns things as needed in [st] based on [u] *)
let process_u_in st (u:Input.user_in_data) =
    let player = get_player_tank st.tanks in
    let enemies = List.filter (fun t -> t.side = Enemy) st.tanks in
    let new_tank_list = (set_player_vel player u)::enemies in
    let new_projectile_list = player_shoot st u in
    {
        st with tanks = new_tank_list;
          projectiles = new_projectile_list;
    }