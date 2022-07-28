(*
    file: oditor.ml
    dependencies: editor.ml display.ml input.ml
    Main file
*)

open Editor;;
open Display;;
open Input;;

(* Main loop
    Quit if ctrl+q is pressed *)
let rec loop () =
    refresh_screen ();
    if process_key () then loop ()
    else (clear_screen (); exit_raw ());;

let () = enter_raw (); loop ();;
