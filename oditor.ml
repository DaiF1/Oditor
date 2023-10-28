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
let config_path = ref "./config.yml";;

let usage_msg = "oditor [-c config] [file]";;
let anonymous_process file = filename := file;;
let speclist = [
    ("-c", Arg.Set_string config_path, "config file")
];;

(* Keymap setup *)
store_keymap "default" setup_defaultkeymaps;;
store_keymap "vim" setup_vimkeymaps;;

(* Laod configuration file *)
let find_value key yaml = match yaml with
    | `O assoc -> List.assoc_opt key assoc
    | _ -> None;;

let parse_config () =
    let load config value load_func default = 
        match find_value value config with
            | Some value -> begin
                                match value with
                                | `String s -> load_func s
                                | _ -> load_func default
                            end
            | None -> load_func default
    in if Sys.file_exists !config_path then
        let config = (Yaml_unix.of_file_exn (Fpath.v !config_path)) in
        load config "keymaps" load_keymap "default"
    else
        load_keymap "default";;

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
    enter_raw (); parse_config ();
    if !filename <> "" then
        open_file !filename;
    try
        loop ()
    with e ->
        let msg = Printexc.to_string e
        and stack = Printexc.get_backtrace () in
            Printf.eprintf "Something went wrong: %s%s" msg stack;
            exit ();;
