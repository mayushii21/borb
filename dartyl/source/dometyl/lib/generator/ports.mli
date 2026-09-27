open! Base
open! Scad_ml

type t =
  { plus : Scad.d3 option
  ; minus : Scad.d3 option
  }

type cutter = walls:Walls.t -> connections:Connect.t -> t

val apply : t -> Scad.d3 -> Scad.d3

val place_tray
  :  ?x_off:float
  -> ?y_off:float
  -> ?z_rot:float
  -> Walls.t
  -> Scad.d3
  -> Scad.d3

val reversible_holder
  :  ?reset_button:bool
  -> ?rail_w:float
  -> ?x_off:float
  -> ?y_off:float
  -> ?z_rot:float
  -> unit
  -> cutter
