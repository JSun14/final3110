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

let draw_wall (t:Block.block)=
  (* NOT FINISHED LMAO, NEED TAKE ACTUAL ARGS *)
  if t.kind = Wall then set_color yellow else set_color black;
  draw_rect (fst t.coord |> int_of_float) (snd t.coord |> int_of_float) 1 1

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

let render_frame (st:State.state) (w:State.world) =
  let remapped_state = remap_coords_state st in 
  let remapped_world = remap_coords_world w in
  draw_tanks remapped_state.tanks;
  draw_walls remapped_world.wall_list;
  draw_walls remapped_world.ditch_list;

