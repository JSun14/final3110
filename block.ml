type block_type = Wall | Ditch

type block = {
    id : string;
    kind : block_type;
    width : float;
    (* Walls should be centrally located at multiples of n + 0.5 *)
    coord : float * float;
}