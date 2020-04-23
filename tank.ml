type team = Self | Enemy
type status = Dead | Alive 

type tank = {
  current_loc : int * int;
  past_loc : int * int;
  velocity : float * float;
  health : status;
  last_fire : float;
  side : team;
}

type projectile = {
  current_loc : int * int;
  velocity : float * float;
  health : status;
  side : team;
}