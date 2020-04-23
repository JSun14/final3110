type team = Self | Enemy
type status = Dead | Alive 

type tank = {
  abs_loc : float * float;
  grid_loc : int * int;
  past_loc : int * int;
  velocity : float * float;
  health : status;
  last_fire_time : float;
  side : team;
}

type projectile = {
  abs_loc : float * float;
  current_loc : int * int;
  velocity : float * float;
  health : status;
  side : team;
}

let has_hit_obstacle t =
  not (t.grid_loc = t.past_loc)

let kill_tank t =
  {t with health = Dead}

let stop_tank t =
  {t with velocity = (0.0, 0.0)}

let move_tank t =
  {t with abs_loc = 
            (fst t.abs_loc +. fst t.velocity, snd t.abs_loc +. snd t.velocity)}