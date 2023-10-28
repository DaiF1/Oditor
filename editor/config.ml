(*
    file: config.ml
    dependencies: editor.ml input.ml
    Load and process config files
*)

open Editor;;
open Input;;

(* Laod configuration file *)
let find_value key yaml = match yaml with
    | `O assoc -> List.assoc_opt key assoc
    | _ -> None;;


(* Load keymap config *)
let config_keymap config =
    match find_value "keymaps" config with
        | Some value -> begin
                            match value with
                            | `String s -> load_keymap s
                            | _ -> load_keymap "default"
                        end
        | None -> load_keymap "default";;

(* Load color config *)
let config_colors config =
    let rec process_colors l = match l with
        | [] -> ()
        | (key, elt)::l -> match elt with
                | `String s -> term.colors <- ColorList.remove key term.colors;
                    term.colors <- ColorList.add key s term.colors;
                    process_colors l
                | _ -> ()
    in match find_value "colors" config with
        | Some value -> begin
                            match value with
                            | `O colors -> process_colors colors
                            | _ -> ()
                        end
        | None -> ();;

(* Parse config file *)
let parse_config config_path =
    if Sys.file_exists config_path then
        begin
            let config = (Yaml_unix.of_file_exn (Fpath.v config_path)) in
            config_keymap config;
            config_colors config
        end
    else
        load_keymap "default";;

