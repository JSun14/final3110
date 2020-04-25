module type Interactions = sig
  val tank_removal : Movable.projectile list -> Movable.tank list -> Movable.tank list
  val proj_removal : Movable.projectile list -> Movable.tank list -> Movable.obstacle list -> Movable.projectile list

end