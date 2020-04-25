module type Interactions = sig

  (**Moves the tank; if next to a wall, set its position at the wall and its
     velocity to (0.0,0.0) *)
  val move_tank : Tank.tank list -> Block.block list -> Tank.tank list
  (**Moves the projectiles regardless of inside wall or not*)
  val move_projs : Tank.projectile list -> Tank.projectile list
  (**If a projectile is inside a tank, remove the tank*)
  val tank_removal : Tank.projectile list -> Tank.tank list -> Tank.tank list
  (**If a projectile is inside a tank, remove the projectile. If it is inside a
     wall remove the projectile.*)
  val proj_removal : Tank.projectile list -> Tank.tank list -> Tank.obstacle list -> Tank.projectile list

end