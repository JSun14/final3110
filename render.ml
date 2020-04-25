open Graphics

let start_rend = 
    open_graph ":0"

let draw_tank =
(* NOT FINISHED LMAO, NEED TAKE ACTUAL ARGS *)
    set_color blue;
    draw_circle 100 100 4

let render_frame =
    clear_graph ();
    draw_tank

