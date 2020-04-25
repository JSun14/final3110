type team = Self | Enemy

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
  side : team;
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