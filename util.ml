(* UTILITY MODULE *)

(* int difference *)
let diff (a,b) (x,y) =
    (a-x, b-y)

(* flaot differece *)
let fdiff (a,b) (x,y) =
    (a -. x, b -. y)

(* float tuple sum *)
let fsum (a,b) (x,y) =
    (a +. x, b +. y)

(* multiplicative inverse *)
let mult_inv flt = 
    1.0 /. flt

(* magnitude of a vector *)
let magn (a,b) =
    sqrt (a *. a +. b *. b)

(* scale a vector by a scale factor *)
let fscale (a,b) scale_fact =
    (a *. scale_fact, b *. scale_fact) 

(* unit vector *)
let unit_vec vec = 
    let magn_inv = magn vec |> mult_inv in 
    fscale vec magn_inv

(**[get_distance_from pointA pointB] calculates the distance from 2 points*)
let get_distance_from pointA pointB =
  sqrt(Float.pow (fst(pointA) -. fst(pointB)) 2.0 +. Float.pow (snd(pointA) -. snd(pointB)) 2.0)