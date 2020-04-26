open Yojson.Basic.Util
open Movable
open Block

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
             |> member "past_loc" 
             |> to_list 
             |> List.map (to_float) 
             |> lst_to_tuple;
  velocity = json 
             |> member "velocity" 
             |> to_list 
             |> List.map (to_float) 
             |> lst_to_tuple;
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