open Tank
(** 

   type tank_species = Blab | Blob | Bleb;
   type team = Player | Enemy;
   type weapon_species = Nuke | ICBM | Basic
   type obstacle_species = Wall | Ditch

   type obstacle = {
    abs_pos: (float*float);
    obstacle_type: obstacle_species;
   }

   type tanks = {
   abs_pos: (float*float);
   velocity: (float*float);
   grid_pos: (int*int);
   past_grid_pos: (int*int);
   active: bool;
   last_fire_time: float;
   tank_type: tank_species;
   team_type: team;
   }

   type weapons = {
   abs_pos: (float*float);
   velocity: (float*float);
   active: bool;
   weapon_type: weapon_species;
   }

*)


(**[get_distance_from pointA pointB] calculates the distance from 2 points*)
let get_distance_from pointA pointB =
  sqrt((fst(pointA) -. fst(pointB))*.(fst(pointA) -. fst(pointB)) +. 
       (snd(pointA) -. snd(pointB))*.(snd(pointA) -. snd(pointB)))

(**[hitbox_detect tank projs] takes in a tank and tests if any projectiles are
   in the tank's hitbox and returns a boolean of whether the tank was hit or not *)
let rec hitbox_detect tank projs= 
  match projs with
  | [] -> false
  | h::t -> if get_distance_from (get_pos h) (get_pos tank) < 1.0 then true else
      false

(**[tank removal projs tanks] takes in a list of tanks and projectiles and 
   returns back the list of active tanks and removes tanks hit by projectiles*)
let tank_removal projs tanks= 
  failwith ("Unimplemented")

(**[wall_detect proj walls] takes in a proj and a list of walls and returns back
   a boolean of whether the project has hit a wall *)
let wall_detect proj walls= 
  failwith ("Unimplemented")

(**[proj_removal projs tanks obs] takes in a list of projs, tanks, and obs and
   returns back an active list of projectiles *)
let proj_removal projs tanks obs= 
  failwith ("Unimplemented")