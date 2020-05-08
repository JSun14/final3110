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
  sqrt(Float.pow (fst(pointA) -. fst(pointB)) 2.0 +. 
       Float.pow (snd(pointA) -. snd(pointB)) 2.0)

let rec range_helper target iter acc =
    match target, iter with
    | t, i when t < 1 -> failwith "target must be > 0"
    | t, i when t = i -> acc 
    | t, i -> range_helper t (i+1) (i::acc)

(** [range target] is a list from [0; 1; 2; ... target-2; target-1]*)
let range target =
    range_helper target 0 [] |> List.rev

(** [range target] is a list from [0.0; 1.0; 2.0; ... target -. 2.0; target -. 1.0]*)
let float_range target = 
    List.map (fun x -> float_of_int x) (range (int_of_float target))
