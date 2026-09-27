open! Base
open! Scad_ml

let path n = Printf.sprintf "../things/caps/%s" n

module MBK = struct
  (* Reproduction by darryldh, found at https://www.thingiverse.com/thing:4564253 *)
  let mbk =
    Scad.import_3d (path "darryldh_MBK/MBK_1u.stl")
    |> Scad.color Color.DarkSlateBlue

  let uniform _ = mbk
end
