open! Base
open! Scad_ml
open! Generator
open! Boards

(* Generate each hand independently so the asymmetric V1 socket pockets keep
   the correct orientation. *)
let dartyl_choc_36_hotswap_right = Dartyl_choc.build ~hotswap:`South ()
let dartyl_choc_36_hotswap_left =
  Dartyl_choc.build ~right_hand:false ~hotswap:`South ()

let () =
  Stdio.print_endline "Building minimal 36-key Dartyl Choc...";
  Write.thing
    "dartyl_choc_36_minimal_hotswap_right"
    (Case.to_scad dartyl_choc_36_hotswap_right);
  Write.thing
    "dartyl_choc_36_minimal_hotswap_left"
    (Case.to_scad dartyl_choc_36_hotswap_left);
  Write.thing
    "dartyl_choc_36_minimal_hotswap_bottom_right"
    (Dartyl_choc.bottom dartyl_choc_36_hotswap_right);
  Write.thing
    "dartyl_choc_36_minimal_hotswap_bottom_left"
    (Dartyl_choc.bottom dartyl_choc_36_hotswap_left);
  Stdio.print_endline "Done!"
