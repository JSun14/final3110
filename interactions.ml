open Block
open Movable
(** 



   (**[wall_detect coords walls] takes in a entity's coordinates as a tuple and a
   list of walls and returns back a boolean of whether the entity is in a wall*)
   let rec wall_detect coords walls= 
   match walls with
   | [] -> false
   | h::t -> if (fst h.coord -. 0.5) <= (fst proj) && (fst x +. 0.5) >= (fst proj) 
               && (snd x -. 0.5) <= (snd proj) && (snd x +. 0.5) >= (snd proj) then true 
    else wall_detect proj t

   let move_tank tanks walls = 
   match tanks with
   | [] -> []
   | h::t -> 



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
   | h::t -> if get_distance_from (get_pos h) (get_pos tank) < 4.0 then true 
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
   | h::t -> if get_distance_from (get_pos proj) (get_post h) < 4.0 then true
    else tank_detect t proj

   (**[proj_removal projs tanks obs] takes in a list of projs, tanks, and walls and
   returns back an active list of projectiles *)
   let rec proj_removal projs tanks walls= 
   match projs with
   | [] -> []
   | h::t -> if tank_detect tanks h && wall_detect h walls 
    then h::proj_removal t tanks walls else proj_removal t tanks walls

   let execute state = 
   failwith "Unimplemented"