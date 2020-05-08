open Util

type team = Self | Enemy
type proj_species = Bouncy | Bullet

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

(** [make_bullet] spawns a standard projectile *)
let make_bullet l v health = {
  loc = l;
  past_loc = fdiff l v;
  velocity = v;
  health = health;
  weap_species = Bullet;
}

let make_bouncy l v health = {
  loc = l;
  past_loc = fdiff l v;
  velocity = v;
  health = health;
  weap_species = Bouncy;
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

let tank_info (t : tank) = 
  print_string ("Current location: " ^ (tuple_to_string t.loc));
  print_string (", Velocity: " ^ (tuple_to_string t.velocity));
  print_string (", Health: " ^ (string_of_int t.health))

let proj_info (p : projectile) = 
  print_string ("Current location: " ^ (tuple_to_string p.loc));
  print_string (", Velocity: " ^ (tuple_to_string p.velocity));
  print_string (", Health: " ^ (string_of_int p.health ^ "\n"))