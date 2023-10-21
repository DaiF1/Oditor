(*
    file: oditor.ml
    dependencies: editor.ml display.ml input.ml
    Main file
*)

open Editor;;
open Files;;
open Display;;
open Input;;
open Default_keymaps;;
open Vim_keymaps;;

(* Command line instructions setup *)
let filename = ref "";;

let usage_msg = "oditor [file]";;
let anonymous_process file = filename := file;;
let speclist = [];;

(* Keymap setup *)
store_keymap "default" setup_defaultkeymaps;;
store_keymap "vim" setup_vimkeymaps;;

(* Exit oditor *)
let exit () =
    clear_screen ();
    exit_raw ();;

(* Main loop
    Refresh screen and process keys. If process returns false, exit editor *)
let rec loop () =
    refresh_screen ();
    if process_key () then loop ()
    else exit ();;

(* Activate raw mode before starting main loop *)
let () = 
    Arg.parse speclist anonymous_process usage_msg;
    enter_raw (); load_keymap "default";
    if !filename <> "" then
        open_file !filename;
    try
        loop ()
    with e ->
        let msg = Printexc.to_string e
        and stack = Printexc.get_backtrace () in
            Printf.eprintf "Something went wrong: %s%s" msg stack;
            exit ();;