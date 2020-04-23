module type Interactions = sig
  val tank_removal : Tank.projectile list -> Tank.tank list -> Tank.tank list
  val proj_removal : Tank.projectile list -> Tank.tank list -> Tank.obstacle list -> Tank.projectile list

end