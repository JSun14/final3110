open Yojson.Basic.Util
open Movable
open Block

type t = {
  tank_list : Movable.tank list;
  wall_list : Block.block list;
  ditch_list : Block.block list;
}

let read_side str = match str with
  | "Self" -> Self
  | "Enemy" -> Enemy
  | _ -> failwith "not valid tank type"

let lst_to_tuple lst = match lst with
  | h :: s :: [] -> (h, s)  
  | _ -> failwith "invalid list"

let tank_of_json json = {
  loc = json 
        |> member "loc" 
        |> to_list
        |> List.map (to_float) 
        |> lst_to_tuple;
  past_loc = json 
             |> member "loc" 
             |> to_list 
             |> List.map (to_float) 
             |> lst_to_tuple;
  velocity = (0.0, 0.0);
  health = json |> member "health" |> to_int;
  last_fire_time = 0;
  side = json |> member "side" |> to_string |> read_side;
}

let read_block_kind str = match str with
  | "Wall" -> Wall
  | "Ditch" -> Ditch
  | _ -> failwith "invalid block kind"

let block_of_json json = {
  id = json |> member "name" |> to_string;
  kind = json |> member "kind" |> to_string |> read_block_kind;
  width = json |> member "width" |> to_float;
  coord = json 
          |> member "coord" 
          |> to_list 
          |> List.map (to_float) 
          |> lst_to_tuple;
}

let from_json json = {
  tank_list = json |> member "tanks" |> to_list |> List.map tank_of_json;
  wall_list = json 
              |> member "blocks" 
              |> member "walls"
              |> to_list 
              |> List.map block_of_json;
  ditch_list = json 
               |> member "blocks" 
               |> member "ditches" 
               |> to_list 
               |> List.map block_of_json;
}