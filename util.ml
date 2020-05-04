(* UTILITY MODULE *)

(* int difference *)
let diff (a,b) (x,y) =
    (a-x, b-y)

(* flaot differece *)
let fdiff (a,b) (x,y) =
    (a -. x, b -. y)

(**[get_distance_from pointA pointB] calculates the distance from 2 points*)
let get_distance_from pointA pointB =
  sqrt(Float.pow (fst(pointA) -. fst(pointB)) 2.0 +. Float.pow (snd(pointA) -. snd(pointB)) 2.0)