open Input
open State
open Movable

let get_player_tank (tl:Movable.tank list) =
    List.find (fun t -> t.side = Self) tl

let player_speed = 1.0

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
        player with velocity = (x0, y0)
    }

(**[process_u_in st u] sets velocities and spawns things as needed in [st] based on [u] *)
let process_u_in st (u:Input.user_in_data) =
    let player = get_player_tank st.tanks in
    let enemies = List.filter (fun t -> t.side = Enemy) st.tanks in
    let new_tank_list = (set_player_vel player u)::enemies in
    {
        st with tanks = new_tank_list
    }