open Tank
open Wall

(* Master state that is passed around in MCL *)
type state = {
  current : room_id;
  visited : room_id list;
  score : int;
  inventory: string list;
  loc_dict: (string * string list) list;
  win_cond: bool;
}

type world = {
    wall_list_cord : ()
}