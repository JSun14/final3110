module type Interactions = sig

  (**Moves the tank; if next to a wall, set its position at the wall and its
     velocity to (0.0,0.0) *)
  val move_tank : Movable.tank list -> Block.block list -> Movable.tank list
  (**Moves the projectiles regardless of inside wall or not*)
  val move_projs : Movable.projectile list -> Movable.projectile list
  (**If a projectile is inside a tank, remove the tank*)
  val tank_removal : Movable.projectile list -> Movable.tank list -> Movable.tank list
  (**If a projectile is inside a tank, remove the projectile. If it is inside a
     wall remove the projectile.*)
  val proj_removal : Movable.projectile list -> Movable.tank list -> Block.block list -> Movable.projectile list

end