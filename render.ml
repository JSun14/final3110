open Graphics
open Movable
open State

let start_rend = 
    open_graph ":0"

let clear = 
    clear_graph ()

let draw_tank =
(* NOT FINISHED LMAO, NEED TAKE ACTUAL ARGS *)
    set_color blue;
    draw_circle 100 100 4

let render_frame st =
    draw_tank

