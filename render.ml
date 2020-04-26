open Graphics
open Movable
open State

let scale = 5.0

let start_rend = 
    open_graph ":0"

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

let draw_grid () =
    (* let dummy = [0.0; 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0] in  *)
    set_color yellow;
    let () = moveto 0 10 in 
    let () = rlineto 100 0 in
    ()


let render_frame (st:State.state) =
    let () = clear_graph () in
    let remapped = remap_coords st in 
    draw_tanks remapped.tanks

