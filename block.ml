type block_type = Wall | Ditch

type block = {
    id : string;
    kind : block_type;
    width : int;
    (* Walls should be centrally located at multiples of 5 *)
    coord : float * float;
}