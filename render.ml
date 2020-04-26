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

let draw_wall (t:Block.block)=
  if t.kind = Wall then set_color yellow else set_color black;
  let unscaled_x = fst t.coord |> int_of_float in
  let unscaled_y = snd t.coord |> int_of_float in
  draw_rect (int_of_float scale * unscaled_x) 
    (int_of_float scale * unscaled_y) (int_of_float scale * 2) 
    (int_of_float scale * 2)

let draw_walls (tl:Block.block list) =
  List.map draw_wall tl

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

let remap_block (t:Block.block) =
  let new_loc = match t.coord with
    | (x, y) -> (scale *. x, scale *. y) in
  {
    t with coord = new_loc
  }

(* take in world file, multiply grid coords by scale factor and draw them *)
let remap_coords_state (st:State.state) =
  {
    st with tanks = List.map remap_tank st.tanks;
            projectiles = List.map remap_proj st.projectiles
  }

let remap_coords_world (w:State.world) =
  {
    wall_list = List.map remap_block w.wall_list;
    ditch_list = List.map remap_block w.ditch_list;
  }

let draw_grid () =
    (* let dummy = [0.0; 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0] in  *)
    set_color yellow;
    let () = moveto 0 10 in 
    let () = rlineto 100 0 in
    ()

let render_frame (w:State.world) (st:State.state) =
  let () = clear_graph () in
  let remapped_state = remap_coords_state st in 
  let remapped_world = remap_coords_world w in

  let _ = draw_tanks remapped_state.tanks in
  let _ = draw_walls remapped_world.wall_list in
  let _ = draw_walls remapped_world.ditch_list in
  ()