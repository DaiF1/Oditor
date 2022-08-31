(*
    file: oditor.ml
    dependencies: editor.ml display.ml input.ml
    Main file
*)

open Editor;;
open Display;;
open Input;;

(* Main loop
    Refresh screen and process keys. If process returns false, exit editor *)
let rec loop () =
    refresh_screen ();
    if process_key () then loop ()
    else (clear_screen (); exit_raw ());;

(* Activate raw mode before starting main loop *)
let () = enter_raw (); loop ();;
