open Input
open State

(**[process_u_in st u] sets velocities and spawns things as needed in [st] based on [u] *)
let process_u_in st u =
    st