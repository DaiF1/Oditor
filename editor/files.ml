(*
    file: files.ml
    dependencies: editor.ml colors.ml
    Editor file management
*)

open Editor;;
open Colors;;

(* Add line to text buffer
    param str: line to add
    param len: length of line *)
let add_line str len = 
    let rec loop text = match text with
        | [] -> let row = {chars = str; size = len; hl = []} in [row]
        | l::t -> l::loop t
    in term.text <- loop term.text;;

(* Open file in editor 
    param path: path to file (string) *)
let open_file path =
    let ic = try Some (open_in path) with Sys_error _ -> None in
    match ic with
    | None -> term.help <- path ^ ": no such file or directory"
    | Some ic -> begin
            term.filename <- path;
            term.text <- [];
            let read () = try Some (input_line ic) with End_of_file -> None
            in
            let rec loop () = match read () with
                | None -> close_in ic; 0
                | Some s -> add_line s (String.length s); loop () + 1
            in term.numlines <- loop ()
    end; update_hl ();;


(* Convert text buffer to string 
    param text: erow to convert *)
let rec buff_to_string text = match text with
    | [] -> ""
    | e::l -> e.chars ^ "\n" ^ buff_to_string l;;

(* Write buffer to file
    param path: path to file *)
let write_file path =
    term.filename <- path;
    term.changed <- false;
    let oc = open_out path in
    output_string oc (buff_to_string term.text); close_out oc;; 
