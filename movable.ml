type team = Self | Enemy
type proj_species = Bouncy | Standard

type tank = {
  loc : float * float;
  past_loc : float * float;
  velocity : float * float;
  health : int;
  last_fire_time : int;
  side : team;
}

type projectile = {
  loc : float * float;
  past_loc : float * float;
  velocity : float * float;
  health : int;
  weap_species: proj_species
}

(** [is dead t] returns whether or not if a movable is dead *)
let is_dead t =
  t.health = 0

let stop_tank t =
  {t with velocity = (0.0, 0.0)}

let move t =
  {t with loc = (fst t.loc +. fst t.velocity, snd t.loc +. snd t.velocity);
          past_loc = t.loc;
  }

(** [grid_loc (x, y)] is a tuple represting the grid location of a [(x, y)]. *)
let grid_loc (x, y) =
  (x |> int_of_float, y |> int_of_float)

let tuple_to_string (t : float * float) = 
  "(" ^ (fst t |> string_of_float) ^ ", " ^ (snd t |> string_of_float) ^ ")"

let print_tank_info t = 
  print_endline ("Current location: " ^ (tuple_to_string t.loc));
  print_endline ("Velocity: " ^ (tuple_to_string t.velocity));
  print_endline ("Health: " ^ (string_of_int t.health))