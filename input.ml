open Graphics

type user_in_data = {
    lmb : bool;
    m_pos : float * float;
    w : bool;
    a : bool;
    s : bool;
    d : bool;
}

(**[print_user_in u] prints the data of [u]*)
let print_user_in u = 
    "LMB: " ^ string_of_bool u.lmb |> print_endline;
    "M_POS: " ^ (fst u.m_pos |> string_of_float) ^ ", " ^ (snd u.m_pos |> string_of_float) |> print_endline;
    if u.w then print_string "w" else ();
    if u.a then print_string "a" else ();
    if u.s then print_string "s" else ();
    if u.d then print_string "d" else ();
    print_endline ""

let rec get_keys acc =
    if key_pressed () 
        then get_keys (read_key () :: acc) 
    else acc 

let get_user_in () =
    let keys = get_keys [] in
    let m_p = mouse_pos () in
    {
        lmb = button_down ();
        m_pos = (fst m_p |> float_of_int, snd m_p |> float_of_int);
        w = List.mem 'x' keys;
        a = List.mem 'a' keys;
        s = List.mem 's' keys;
        d = List.mem 'd' keys;
    }