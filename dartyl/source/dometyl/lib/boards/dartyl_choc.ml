(* A Choc-flavoured Dactyl with 36 keys. *)

open! Base
open! Scad_ml
open Generator

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~n_steps:(`Flat 4)
          ~north_clearance:2.19
          ~south_clearance:2.19
          ~side_clearance:2.19
          ~west_lookup:(fun i -> if i = 0 then Eye else No)
          ~north_lookup:(fun i -> if i = 2 || i = 4 then Eye else Yes)
          ~south_lookup:(function
            | 0 -> No
            | 3 -> Eye
            | _ -> Yes)
          ~thickness:5.0
          plate
    ; thumb =
        auto_thumb
          ~north_lookup:(fun _ -> No)
          ~south_lookup:(fun _ -> Yes)
          ~north_clearance:0.5
          ~south_clearance:0.5
          ~side_clearance:0.5
          ~d1:3.
          ~d2:4.75
          ~n_facets:60
          ~n_steps:(`Flat 15)
          plate
    }

(* With the third thumb key, the end wall of the thumb cluster overlaps the index
   column's south wall, so the east link has to cross diagonally between them.
   The original snake link degenerates there; a straight link (made solid by the
   orientation check in [Util.prism_exn]) closes the gap. *)
let east_link = Connect.straight ~height:11. ()

(* The straight link reports only two corner points for the bottom-plate outline,
   which lie inside its diagonal outer face. Route the outline along the link's
   real outer face instead: from where it meets the index wall's outer face, to
   its thumb-side foot, then around the end of the thumb wall. *)
let fix_east_link_outline (walls : Walls.t) (connections : Connect.t) =
  let index_wall = Option.map ~f:snd (Map.min_elt walls.body.south)
  and thumb_wall =
    (* Same choice as [Connect.manual]: first thumb east wall, else last south. *)
    Option.map
      ~f:snd
      (Option.first_some (Map.min_elt walls.thumb.east) (Map.max_elt walls.thumb.south))
  in
  match index_wall, thumb_wall with
  | Some (w1 : Wall.t), Some (w2 : Wall.t) ->
    let link = Connect.connect east_link w1 w2 in
    let link_start, link_end =
      match link.inline with
      | [ _; s; e ] -> s, e
      | _ -> failwith "Unexpected east link inline points."
    in
    (* Intersection (in XY) of the link's outer face with the index wall's outer
       face. *)
    let meet =
      let x1, y1, _ = link_start
      and x2, y2, _ = link_end
      and x3, y3, _ = w1.foot.top_left
      and x4, y4, _ = w1.foot.top_right in
      let den = ((x1 -. x2) *. (y3 -. y4)) -. ((y1 -. y2) *. (x3 -. x4)) in
      let t = (((x1 -. x3) *. (y3 -. y4)) -. ((y1 -. y3) *. (x3 -. x4))) /. den in
      x1 +. (t *. (x2 -. x1)), y1 +. (t *. (y2 -. y1)), 0.
    in
    let hits = List.count connections.outline ~f:(Poly.equal w1.foot.top_right) in
    if hits <> 1 then failwith "East link outline corner not found exactly once.";
    let outline =
      List.concat_map connections.outline ~f:(fun p ->
          if Poly.equal p w1.foot.top_right
          then [ meet; link_end; w2.foot.bot_left ]
          else [ p ] )
    in
    { connections with outline }
  | _ -> connections

let base_connector (walls : Walls.t) =
  let connections =
    Connect.skeleton
      ~height:6.
      ~thumb_height:9.5
      ~east_link
      ~west_link:(Connect.straight ())
      ~cubic_d:3.
      ~cubic_scale:1.
      ~index_height:20.
      ~thumb_join_steps:(`Flat 14)
      ~body_join_steps:(`Flat 10)
      ~fudge_factor:10.
      ~north_joins:(fun i -> i < 2)
      ~close_thumb:true
      ~south_joins:(fun _ -> false)
      walls
  in
  fix_east_link_outline walls connections

let body_lookups =
  let offset = function
    | 2 -> -2.33, 4.6, -3. (* middle *)
    | 3 -> -1.66, -4.5, 0. (* ring *)
    | i when i >= 4 -> -3.5, -19., 6.5 (* pinky *)
    | 0 -> -0.85, -3.5, 13. (* inside *)
    | _ -> -1.66, -2., 5. (* index *)
  and curve = function
    | 0 ->
      Curvature.(
        curve ~well:(spec ~radius:42. (Float.pi /. 5.95) ~tilt:(Float.pi /. 5.)) ())
    | _ ->
      Curvature.(
        curve ~well:(spec ~radius:42. (Float.pi /. 6.1) ~tilt:(Float.pi /. 15.)) ())
  and swing = function
    | 2 -> Float.pi /. -48.
    | 3 -> Float.pi /. -23.
    | 4 -> Float.pi /. -20.0
    | _ -> 0.
  and splay = function
    | 4 -> -0.13
    | 3 -> -0.05
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~swing ~rows ()


let thumb_lookups =
  let curve _ =
    Curvature.(curve 
      ~fan:{ angle = Float.pi /. 10.2; radius = 75.; tilt = Float.pi /. 48. } 
      ~well:{ angle = Float.pi /. 7.5; radius = 59.; tilt = 0. } ())
  and rows _ = 3 in
  Plate.Lookups.thumb ~curve ~rows ()


let plate_welder = Plate.skeleton_bridges

let ports_cutter =
  Ports.reversible_holder ~reset_button:true ~x_off:(-1.5) ~y_off:(-2.9) ~z_rot:0.09 ~rail_w:1.15 ()

(* The four M4 eyelets are generated from the wall lookups above. [Bottom.make]
   uses the same centres for countersunk holes.  Omit bumpon recesses so the
   underside stays flat for an adhesive mounting ring. *)
let bottom ?(thickness = 2.4) case =
  Bottom.make ~thickness ~bump_locs:[] case

(* [right_hand] controls the reversible case transform.  Keep it at the board's
   historical right-hand default while exposing the non-mirrored hotswap build
   as a first-class option. *)
let build ?hotswap ?(right_hand = true) () =
  let keyhole = Choc.make_hole ?hotswap ~outer_h:18. ~thickness:2.8 () in

  let plate_builder =
    Plate.make
      ~n_body_cols:5
      ~spacing:1.5
      ~tent:(Float.pi /. 12.)
      ~body_lookups
      ~thumb_lookups
      ~caps:Caps.MBK.uniform
      ~thumb_caps:Caps.MBK.uniform
      ~thumb_offset:(1., -48., 10.)
      ~thumb_angle:Float.(0.23, -0.3, pi /. 17.)
  in
  Case.make
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    ~plate_builder
    keyhole
    ~right_hand
