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

(* Get line at given position 
    param i: line index *)
let get_line i =
    let rec loop text i = match text with
        | [] -> {size = 0; chars = ""; hl = []}
        | e::_ when i = 0 -> e
        | _::l -> loop l (i - 1)
    in loop term.text i;;

(* Replace escape codes by hex value *)
let rep_esc text =
    let rec loop i = match i with
        | -1 -> ""
        | i -> let c = text.[i] in
            if c = '\027' then loop (i - 1) ^ "\x1b"
            else loop (i - 1) ^ Char.escaped c
    in
    let len = String.length text in
    loop (len - 1);;

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
                | Some s -> let s = rep_esc s in
                add_line s (String.length s); loop () + 1
            in term.numlines <- loop ()
    end; update_hl ();;


(* Open test result file *)
let open_tests () =
    let prev_path = term.filename in
    open_file "test_results";
    term.filename <- prev_path;;


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
