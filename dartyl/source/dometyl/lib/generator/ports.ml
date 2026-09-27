open! Base
open! Scad_ml

type t =
  { plus : Scad.d3 option
  ; minus : Scad.d3 option
  }

type cutter = walls:Walls.t -> connections:Connect.t -> t

let apply t scad =
  let added =
    Option.value_map ~default:scad ~f:(fun s -> Scad.union [ s; scad ]) t.plus
  in
  Option.value_map ~default:added ~f:(fun s -> Scad.difference added [ s ]) t.minus

let place_tray
    ?(x_off = 0.)
    ?(y_off = -0.25)
    ?(z_rot = 0.)
    Walls.{ body = { north; _ }; _ }
    scad =
  let left_foot = (Map.find_exn north 0).foot
  and right_foot = (Map.find_exn north 1).foot in
  let x = Vec3.get_x left_foot.bot_left +. x_off
  and y =
    let outer (ps : Points.t) = Vec3.(get_y ps.top_left +. get_y ps.top_right) /. 2. in
    y_off +. ((outer left_foot +. outer right_foot) /. 2.)
  in
  Scad.rotate (0., 0., z_rot) scad |> Scad.translate (x, y, 0.)

let reversible_holder
    ?(reset_button = false)
    ?(rail_w = 1.4)
    ?x_off
    ?y_off
    ?z_rot
    ()
    ~walls
    ~connections:_ =
  let w = 30.6 and h = if reset_button then 15. else 8.4 in
  let tray =
    Scad.cube (27.6, 38.8, 7.7)
    |> Scad.translate (3., -38.8, 0.)
    |> Scad.color ~alpha:0.5 Color.Salmon
  and front =
    Scad.cube (w, 13., h) |> Scad.translate (0., -8., 0.) |> Scad.color ~alpha:0.5 Color.Salmon
  and rails =
    let rail = Scad.cube ~center:true (rail_w +. 0.01, rail_w, h +. 0.01) in
    [ Scad.translate (rail_w /. 2., -2.25, h /. 2.) rail
    ; Scad.translate (w -. (rail_w /. 2.), -2.25, h /. 2.) rail
    ]
  in
  let minus =
    Scad.difference (Scad.union [ tray; front ]) rails
    |> place_tray ?x_off ?y_off ?z_rot walls
    |> Option.some
  in
  { plus = None; minus }
