(*
    file: input.ml
    dependencies: editor.ml files.ml insert.ml
    Utility for editor input management
*)

open Editor;;

(* Return true if ctrl+k was pressed *)
let ctrl key = Char.chr ((Char.code key) land 0x1f);;

(* Read byte from stdin
    Return the read byte *)
let read_key () = 
    try input_char stdin
    with End_of_file -> '\000';;

(* Add key binding to editor.
    Will overwrite if the binding already exists *)
let add_key key func =
    Hashtbl.add term.controls key func;;

(* Set default input processing function.
    Must take char and editor mode as a parameter *)
let set_default_proc func =
    term.default_proc <- func;;

(* Process key presses. Return false if exit key pressed *)
let process_key () = 
    match read_key () with
        | '\000' -> true
        | k -> let func = Hashtbl.find_opt term.controls k in
            match func with
                | None -> term.default_proc k term.mode
                | Some f -> f term.mode;;

(* Store a given keymap into the hashtbl
    param name: keymap name
    param setup_fun: keymap setup function. Must be unit -> unit *)
let store_keymap name setup_fun =
    Hashtbl.add term.keymaps name setup_fun;;

(* Load a given keymap
    param name: keymap name *)
let load_keymap name =
    let func = Hashtbl.find_opt term.keymaps name in
    match func with
        | None -> term.help <- "Unable to load keymap"
        | Some f -> f ();;
