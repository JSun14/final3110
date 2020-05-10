open Graphics
open Movable
open State
open Input 

open Const

let start_rend () = 
  open_graph ":0";
  auto_synchronize false

let draw_tank (t:Movable.tank)=
  (* NOT FINISHED LMAO, NEED TAKE ACTUAL ARGS *)
  if t.side = Enemy then set_color red else set_color blue;
  fill_circle (fst t.loc |> int_of_float) (snd t.loc |> int_of_float) 
    ((scale *. 0.4) |> int_of_float)

let draw_wall (t:Block.block)=
  if t.kind = Wall then set_color yellow else set_color (rgb 139 69 19);
  let scaled_x = fst t.coord |> int_of_float in
  let scaled_y = snd t.coord |> int_of_float in
  fill_rect scaled_x scaled_y (int_of_float scale * 1) 
    (int_of_float scale * 1)

let draw_projectile (p:Movable.projectile) =
  set_color black;
  set_line_width 2;
  let x = fst p.loc |> int_of_float in
  let y = snd p.loc |> int_of_float in
  let x_vel = fst p.velocity |> int_of_float in
  let y_vel = snd p.velocity |> int_of_float in
  moveto x y;
  lineto (x + x_vel) (y + y_vel);
  set_line_width 1

let draw_projectiles (pl : Movable.projectile list) =
  List.map draw_projectile pl

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
    | (x, y) -> (scale *. (x -. 0.5), scale *. (y -. 0.5)) in
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

  set_color yellow;

  let dummy = [0.0; 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0; 
               11.0; 12.0; 13.0; 14.0; 15.0; 16.0; 17.0; 18.0; 19.0; 20.0] in 
  let d2 = dummy @ [21.0; 22.0; 23.0; 24.0; 25.0; 26.0; 27.0; 28.0; 29.0] in
  let d_scaled = List.map (fun x -> x *. scale |> int_of_float) d2 in 

  let draw_horiz y_val = 
    moveto 0 y_val;
    rlineto 1000 0;
  in
  let draw_vert x_val =
    moveto x_val 0;
    rlineto 0 1000;
  in
  let _ = List.map draw_horiz d_scaled in 
  let _ = List.map draw_vert d_scaled in 
  ()


let render_frame (w:State.world) (st:State.state) =
  let () = clear_graph () in
  let remapped_state = remap_coords_state st in 
  let remapped_world = remap_coords_world w in

  let _ = draw_grid () in

  let _ = draw_tanks remapped_state.tanks in
  let _ = draw_walls remapped_world.wall_list in
  let _ = draw_walls remapped_world.ditch_list in
  let _ = draw_projectiles remapped_state.projectiles in

  let () = synchronize () in
  ()