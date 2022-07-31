(*
    file: files.ml
    dependencies: editor.ml
    Editor file management
*)

open Editor;;

(* Add line to text buffer *)
let add_line str len = 
    let rec loop text = match text with
        | [] -> [{chars = str; size = len}]
        | l::t -> l::loop t
    in term.text <- loop term.text;;

(* Get line at given position *)
let get_line i =
    let rec loop text i = match text with
        | [] -> {size = 0; chars = ""}
        | e::_ when i = 0 -> e
        | _::l -> loop l (i - 1)
    in loop term.text i;;

(* Open file in editor *)
let open_file path =
    term.filename <- path;
    term.text <- [];
    let ic = try Some (open_in path) with Sys_error _ -> None in
    match ic with
    | None -> term.help <- path ^ ": no such file or directory"
    | Some ic -> begin
            let read () = try Some (input_line ic) with End_of_file -> None
            in
            let rec loop () = match read () with
                | None -> close_in ic; 0
                | Some s -> add_line s (String.length s); loop () + 1
            in term.numlines <- loop ()
        end;;


