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

let color_regex = "^#[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]$";;

let valid_color color =
    let regex_col = Str.regexp color_regex in
    Str.string_match regex_col color 0;;

let rgb_to_ansi color =
    (* Use int_of_string to convert hex to decimal, and put it back into string *)
    let r = string_of_int (int_of_string ("0x" ^ String.sub color 1 2)) and
    g = string_of_int (int_of_string ("0x" ^ String.sub color 3 2)) and
    b = string_of_int (int_of_string ("0x" ^ String.sub color 5 2)) in
    "\x1b[38;2;" ^ r ^ ";" ^ g ^ ";" ^ b ^ "m";;

let config_colors config =
    let rec process_colors l = match l with
        | [] -> ()
        | (key, elt)::l -> match elt with
                | `String s -> if valid_color s then
                    begin
                        term.colors <- ColorList.remove key term.colors;
                        term.colors <- ColorList.add key (rgb_to_ansi s) term.colors;
                        process_colors l
                    end
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

