open Block
open Movable
open State
(**[move_tank tanks walls] takes in a list of tanks and walls and moves the
   tanks according to their velocities. If the tank is in a wall then move the 
   tank up to the wall. Set the tanks' velocities to 0.*)
let rec move_tank (tanks:Movable.tank list) walls= 
  match tanks with  
  | [] -> []
  | h::t -> let new_loc = 
              (fst h.velocity +. fst h.loc, snd h.velocity +. snd h.loc) in
    {loc = new_loc; past_loc = h.loc; velocity = (0.0,0.0); 
     health = h.health; last_fire_time = h.last_fire_time; 
     side = h.side;}::move_tank t walls

(*match wall_detect new_loc walls with
  | Some w -> {loc = h.loc; past_loc = h.loc; velocity = (0.0,0.0); 
             health = h.health; last_fire_time = h.last_fire_time; 
             side = h.side;}::move_tank t walls
  | None -> {loc = new_loc; past_loc = h.loc; velocity = (0.0,0.0); 
           health = h.health; last_fire_time = h.last_fire_time; 
           side = h.side;}::move_tank t walls*)


(**[move_projs projs walls] is a list of active projectiles with updated 
   locations. If the proj's new location is inside a wall then remove it, but if 
   it is  *)
let rec move_projs (projs:Movable.projectile list) walls=
  match projs with
  | [] -> []
  | h::t -> let new_loc = 
              (fst h.velocity +. fst h.loc, snd h.velocity +. snd h.loc) in
    {loc = new_loc; past_loc = h.loc; velocity = (0.0,0.0); 
     health = h.health; weap_species= h.weap_species;}
    ::move_projs t walls

(*match wall_detect new_loc walls with
  | None -> {loc = new_loc; past_loc = h.loc; velocity = (0.0,0.0); 
           health = h.health; weap_species= h.weap_species;}
          ::move_projs t walls
  | Some w -> match h.weap_species with
  | Bouncy -> if h.health = 2 
    then {loc = h.loc; past_loc = h.loc; velocity = (0.0,0.0); 
          health = h.health; weap_species= h.weap_species;}::move_projs t walls
    else move_projs t walls
  | Standard -> move_projs t walls*)

(**[get_distance_from pointA pointB] calculates the distance from 2 points*)
let get_distance_from pointA pointB =
  sqrt((fst(pointA) -. fst(pointB))*.(fst(pointA) -. fst(pointB)) +. 
       (snd(pointA) -. snd(pointB))*.(snd(pointA) -. snd(pointB)))


(**[wall_detect coords walls] takes in a entity's coordinates as a tuple and a
   list of walls and returns back a boolean of whether the entity is in a wall*)
let rec wall_detect coords walls= 
  if (fst walls.coord -. 0.5) <= (fst coords) && 
     (fst walls.coord +. 0.5) >= fst coords && 
     (snd walls.coord -. 0.5) <= snd coords && 
     (snd walls.coord +. 0.5) >= snd coords then true else false

(**[check_grid coordA coordB] sees if 2 coordinates are less than 2 grid pos 
   away in both x and y directions *)
let check_grid coordA coordB = 
  if  Float.to_int (fst coordA)|> (-) (Float.to_int (fst coordB)) |> Int.abs < 2
   && Float.to_int (snd coordA)|> (-) (Float.to_int (snd coordB)) |> Int.abs < 2
  then true else false

(**[tank_touch_wall wall tank] returns a bool as to whether a tank is touching
   a specific wall*)
let tank_touch_wall wall (tank:Movable.tank)=
  let corn1 = (fst wall.coord-.0.5, snd wall.coord-.0.5) in
  let corn2 = (fst wall.coord+.0.5, snd wall.coord-.0.5) in
  let corn3 = (fst wall.coord+.0.5, snd wall.coord+.0.5) in
  let corn4 = (fst wall.coord-.0.5, snd wall.coord+.0.5) in
  let edge1 = Float.abs ((fst corn4 -. fst corn1)*.(fst tank.loc) +. (snd corn1 -. snd corn4)*.(snd tank.loc) +. (fst corn1 -. fst corn4)*.(snd corn1) +. (snd corn4 -. snd corn1)*.(fst corn1)) /. Float.sqrt ((Float.pow (fst corn4 -. fst corn1) 2.0)+.(Float.pow (snd corn1 -. snd corn4) 2.0)) in
  let edge2 = Float.abs ((fst corn2 -. fst corn1)*.(fst tank.loc) +. (snd corn1 -. snd corn2)*.(snd tank.loc) +. (fst corn1 -. fst corn2)*.(snd corn1) +. (snd corn2 -. snd corn1)*.(fst corn1)) /. Float.sqrt ((Float.pow (fst corn2 -. fst corn1) 2.0)+.(Float.pow (snd corn1 -. snd corn2) 2.0)) in
  let edge3 = Float.abs ((fst corn2 -. fst corn3)*.(fst tank.loc) +. (snd corn3 -. snd corn2)*.(snd tank.loc) +. (fst corn3 -. fst corn2)*.(snd corn3) +. (snd corn2 -. snd corn3)*.(fst corn3)) /. Float.sqrt ((Float.pow (fst corn2 -. fst corn3) 2.0)+.(Float.pow (snd corn3 -. snd corn2) 2.0)) in
  let edge4 = Float.abs ((fst corn4 -. fst corn3)*.(fst tank.loc) +. (snd corn3 -. snd corn4)*.(snd tank.loc) +. (fst corn3 -. fst corn4)*.(snd corn3) +. (snd corn4 -. snd corn3)*.(fst corn3)) /. Float.sqrt ((Float.pow (fst corn4 -. fst corn3) 2.0)+.(Float.pow (snd corn3 -. snd corn4) 2.0)) in
  if get_distance_from tank.loc corn1 < 0.4 || 
     get_distance_from tank.loc corn2 < 0.4 || 
     get_distance_from tank.loc corn3 < 0.4 ||
     get_distance_from tank.loc corn4 < 0.4 || edge1 < 0.4 || edge2 < 0.4 || 
     edge3 < 0.4 || edge4 < 0.4 then true else false

(**[tank_phys_engine tank walls] returns a tank with values either unchanged or 
   changed depending on if the tank touched a wall*)
let rec tank_phys_engine (tank:Movable.tank) walls : Movable.tank =
  match walls with
  | [] -> tank
  | h::t -> if check_grid h.coord tank.loc then if tank_touch_wall h tank
      then {tank with loc=tank.past_loc;}
      else tank_phys_engine tank t
    else tank_phys_engine tank t

(**[proj_phys_engine proj walls] returns None if a proj is not in a wall and
   Some proj if it is with modified proj values.*)
let rec proj_phys_engine proj walls=
  match walls with
  | [] -> Some proj
  | h::t -> if wall_detect proj.loc h then match proj.weap_species with
      | Bouncy -> Some proj (**IMPLEMENT PROJ REFLECTION *)
      | Standard -> None
    else proj_phys_engine proj t

(**[check_tank_wall tanks walls] returns list of tanks after performing 
   interactions with walls*)
let rec check_tank_wall (tanks:Movable.tank list) walls : Movable.tank list=
  match tanks with
  | [] -> [] 
  | h::t -> tank_phys_engine h walls :: check_tank_wall t walls

let rec check_proj_wall projs walls=
  match projs with
  | [] -> []
  | h::t -> match proj_phys_engine h walls with
    | None -> check_proj_wall t walls
    | Some proj -> proj::check_proj_wall t walls


(**[hitbox_detect tank projs] takes in a tank and tests if any projectiles are
   in the tank's hitbox and returns true if a projectile hit a tank and false
   if it didn't*)
let rec hitbox_detect tank projs= 
  match projs with
  | [] -> false
  | h::t -> if get_distance_from h.loc tank.loc < 0.4 then true 
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
  | h::t -> if get_distance_from proj.loc h.loc < 0.4 then true
    else tank_detect t proj

(**[proj_removal projs tanks obs] takes in a list of projs, tanks, and walls and
   returns back an active list of projectiles *)
let rec proj_removal projs tanks walls= 
  match projs with
  | [] -> []
  | h::t -> if tank_detect tanks h  
    then h::proj_removal t tanks walls else proj_removal t tanks walls

let wall_execute w (st:State.state)=
  {st with tanks= check_tank_wall (move_tank st.tanks w.wall_list) w.wall_list;}
let execute w (st:State.state)= 
  {st with tanks=move_tank st.tanks w.wall_list; projectiles=move_projs st.projectiles w.wall_list;}