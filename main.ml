open State

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Tank Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  (* match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name *)
  let init = init_state 


let rec game_helper w st =
(* let s1 = Get_user_in.execute world state *)
(* let s2 = Physics.execute world s1*)
(* let s3 = Interactions.excute world s2 *)
(* Render.execute world s3 *)
  
  let final_state = {
    st with cycle_no = st.cycle_no + 1
  }
  game_helper w st;

(* eventually needs to do something with w *)
let init_state w = {
  cycle_no = 0; 
  score = 0;
  tanks = [];
  projectiles = [];
}

(* eventually needs to load world from json *)
let init_world w = {
  
}
(* Execute the game engine. *)
let () = main ()