open Block
open Movable

(**[wall_detect coords walls] takes in a entity's coordinates as a tuple and a
   list of walls and returns back a boolean of whether the entity is in a wall*)
let rec wall_detect coords walls= 
  match walls with
  | [] -> None
  | h::t -> if (fst h.coord -. 0.5) <= (fst coords) && 
               (fst h.coord +. 0.5) >= (fst coords) && 
               (snd h.coord -. 0.5) <= (snd coords) && 
               (snd h.coord +. 0.5) >= (snd coords) then Some h 
    else wall_detect coords t

(**[move_tank tanks walls] takes in a list of tanks and walls and moves the
   tanks according to their velocities. If the tank is in a wall then move the 
   tank up to the wall. Set the tanks' velocities to 0.*)
let rec move_tank (tanks:Movable.tank list) walls= 
  match tanks with  
  | [] -> []
  | h::t -> let new_loc = 
              (fst h.velocity +. fst h.loc, snd h.velocity +. snd h.loc) in
    match wall_detect new_loc walls with
    | Some w -> {loc = w.coord; past_loc = h.loc; velocity = (0.0,0.0); 
                 health = h.health; last_fire_time = h.last_fire_time; 
                 side = h.side;}::move_tank t walls
    | None -> {loc = new_loc; past_loc = h.loc; velocity = (0.0,0.0); 
               health = h.health; last_fire_time = h.last_fire_time; 
               side = h.side;}::move_tank t walls



let rec move_projs (projs:Movable.projectile list) walls=
  match projs with
  | [] -> []
  | h::t -> let new_loc = 
              (fst h.velocity +. fst h.loc, snd h.velocity +. snd h.loc) in
    match wall_detect new_loc walls with
    | Some w -> move_projs t walls
    | None -> {loc = new_loc; past_loc = h.loc; velocity = (0.0,0.0); 
               health = h.health; weap_species= h.weap_species;}
              ::move_projs t walls



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
  | h::t -> if get_distance_from h.loc tank.loc < 4.0 then true 
    else hitbox_detect tank t

(**[tank removal projs tanks] takes in a list of tanks and projectiles and 
   returns back the list of active tanks and removes tanks hit by projectiles*)
let rec tank_removal projs tanks= 
  match tanks with
  | [] -> []
  | h::t -> if hitbox_detect h projs then h:: tank_removal projs t 
    else tank_removal projs t 



(**[tank_detect tanks proj] takes in a list of tanks and a projectile and checks
   to see if a projectile should be removed due to it hitting a tank via bool *)
let rec tank_detect tanks proj=
  match tanks with
  | [] -> false
  | h::t -> if get_distance_from proj.loc h.loc < 4.0 then true
    else tank_detect t proj

(**[proj_removal projs tanks obs] takes in a list of projs, tanks, and walls and
   returns back an active list of projectiles *)
let rec proj_removal projs tanks walls= 
  match projs with
  | [] -> []
  | h::t -> if tank_detect tanks h  
    then h::proj_removal t tanks walls else proj_removal t tanks walls

let execute state = 
  failwith "Unimplemented"