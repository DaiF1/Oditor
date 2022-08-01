(*
    file: insert.ml
    dependencies: editor.ml
    Definition of all insert operations
*)

open Editor;;

(* Insert char in row at given position *)
let insert_char row c i =
    row.chars <- String.sub row.chars 0 i ^ Char.escaped c ^
        String.sub row.chars i (row.size - i);
    row.size <- row.size + 1; term.x <- term.x + 1;;

(* Insert new row in text at given position *)
let insert_row i = 
    let rec loop text i = match text with
        | [] -> [{size = 0; chars = ""}]
        | l when i = 0 -> {size = 0; chars = ""}::l
        | e::l -> e::loop l (i - 1)
    in term.text <- loop term.text i; term.numlines <- term.numlines + 1;;

(* Delete char in row at given position *)
let delete_char row i =
    row.chars <- String.sub row.chars 0 (i - 1) ^ 
        String.sub row.chars i (row.size - i);
    row.size <- row.size - 1;
    term.x <- term.x - 1;;

(* Delete row in text at given position *)
let delete_row i =
    let rec loop text i = match text with
        | [] -> []
        | e::l when i = 1 -> begin
                match l with
                    | [] -> [e]
                    | last::text ->
                        term.y <- term.y - 1;
                        term.x <- e.size;
                        term.numlines <- term.numlines - 1;
                        e.size <- e.size + last.size;
                        e.chars <- e.chars ^ last.chars;
                        e::text
                end
        | e::l -> e::loop l (i - 1)
    in term.text <- loop term.text i;;
