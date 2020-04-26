open Graphics
open Movable
open State

let scale = 5.0

let start_rend = 
    open_graph ":0"

let clear = 
    clear_graph ()

let draw_tank (t:Movable.tank)=
(* NOT FINISHED LMAO, NEED TAKE ACTUAL ARGS *)
    if t.side = Enemy then set_color red else set_color blue;
    draw_circle (fst t.loc |> int_of_float) (snd t.loc |> int_of_float) 4

let draw_tanks (tl:Movable.tank list) =
    List.map draw_tank tl

let remap_tank (t:Movable.tank) =
    let new_loc = match t.loc with
    | (x, y) -> (scale *. x, scale *. y) in
    {
        t with loc = new_loc
    }

let remap_proj (t:Movable.projectile) =
    let new_loc = match t.loc with
    | (x, y) -> (scale *. x, scale *. y) in
    {
        t with loc = new_loc
    }

let remap_coords (st:State.state) =
    {
        st with tanks = List.map remap_tank st.tanks;
        projectiles = List.map remap_proj st.projectiles
    }

let render_frame (st:State.state) =
    let remapped = remap_coords st in 
    draw_tanks remapped.tanks

