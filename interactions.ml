(** 

   type tank_species = Blab | Blob | Bleb;
   type team = Player | Enemy;
   type weapon_species = Nuke | ICBM | Basic
   type obstacle_species = Wall | Ditch

   type obstacle = {
    abs_pos: (float*float);
    obstacle_type: obstacle_species;
   }

   type tanks = {
   abs_pos: (float*float);
   velocity: (float*float);
   grid_pos: (int*int);
   past_grid_pos: (int*int);
   active: bool;
   last_fire_time: float;
   tank_type: tank_species;
   team_type: team;
   }

   type weapons = {
   abs_pos: (float*float);
   velocity: (float*float);
   active: bool;
   weapon_type: weapon_species;
   }

*)