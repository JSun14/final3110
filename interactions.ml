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
   in the tank's hitbox and returns true if a projectile hit a tank and false
   if it didn't*)
let rec hitbox_detect tank projs= 
  match projs with
  | [] -> false
  | h::t -> if get_distance_from (get_pos h) (get_pos tank) < 1.0 then true 
    else hitbox_detect tank t

(**[tank removal projs tanks] takes in a list of tanks and projectiles and 
   returns back the list of active tanks and removes tanks hit by projectiles*)
let rec tank_removal projs tanks= 
  match tanks with
  | [] -> []
  | h::t -> if hitbox_detect h projs then h:: tank_removal projs t 
    else tank_removal projs t 

(**[wall_detect proj walls] takes in a projectile's coordinates as a tuple and a
   list of walls and returns back a boolean of whether the project hit a wall*)
let rec wall_detect proj walls= 
  match walls with
  | [] -> false
  | h::t -> let x = get_pos h in 
    if (fst x) <= (fst proj) && (fst x +. 1.0) >= (fst proj) 
       && (snd x) <= (snd proj) && (snd x +. 1.0) >= (snd proj) then true 
    else wall_detect proj t

let rec tank_detect tanks proj=
  match tanks with
  | [] -> false
  | h::t -> if get_distance_from (get_pos proj) (get_post h) < 1.0 then true
    else tank_detect t proj

(**[proj_removal projs tanks obs] takes in a list of projs, tanks, and walls and
   returns back an active list of projectiles *)
let rec proj_removal projs tanks walls= 
  match projs with
  | [] -> []
  | h::t -> if tank_detect tanks h && wall_detect h walls 
    then h::proj_removal t tanks walls else proj_removal t tanks walls