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
    row.size <- row.size + 1;;

(* Insert new row in text at given position *)
let insert_row i = 
    let rec loop text i = match text with
        | [] -> [{size = 0; chars = ""}]
        | l when i = 0 -> {size = 0; chars = ""}::l
        | e::l -> e::loop l (i - 1)
    in term.text <- loop term.text i;;
