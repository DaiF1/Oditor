(*
    file: oditor.ml
    dependencies: editor.ml display.ml input.ml config.ml
    Main file
*)

open Editor;;
open Files;;
open Display;;
open Input;;
open Default_keymaps;;
open Vim_keymaps;;
open Config;;

(* Command line instructions setup *)
let filename = ref "";;
let config_path = ref (Unix.getenv "HOME" ^ "/.config/oditor/config.yml");;

let usage_msg = "oditor [-c config] [file]";;
let anonymous_process file = filename := file;;
let speclist = [
    ("-c", Arg.Set_string config_path, "config file")
];;

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
    enter_raw (); parse_config !config_path;
    if !filename <> "" then
        open_file !filename;
    try
        loop ()
    with e ->
        let msg = Printexc.to_string e
        and stack = Printexc.get_backtrace () in
            Printf.eprintf "Something went wrong: %s%s" msg stack;
            exit ();;
