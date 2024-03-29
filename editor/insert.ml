(*
    file: insert.ml
    dependencies: editor.ml colors.ml
    Definition of all insert operations
*)

open Editor;;
open Colors;;

(* Insert char in row at given position 
    param row: row to be modified
    param c: char to insert 
    param i: position of inserted char *)
let insert_char row c i =
    term.changed <- true;
    row.chars <- String.sub row.chars 0 i ^ String.make 1 c ^
        String.sub row.chars i (row.size - i);
    row.size <- row.size + 1; term.x <- term.x + 1;
    update_hl ();;

(* Insert string in row at given position 
    param row: row to be modified
    param s: string to insert
    param len: length of inserted string
    param i: starting position of inserted string *)
let insert_string row s len i =
    term.changed <- true;
    row.chars <- String.sub row.chars 0 i ^ s ^
        String.sub row.chars i (row.size - i);
    row.size <- row.size + len; term.x <- term.x + len;
    update_hl ();;

(* Insert new row in text at given position
    param i: position of new row in text. must be > 0*)
let insert_row i = 
    term.changed <- true;
    let rec loop text i prev = match text with
        | [] when i <> 0 -> [{size = 0; chars = ""; hl = []}]
        | e::l when i <> 0 -> e::loop l (i - 1) e
        | l -> let str = String.sub prev.chars term.x (prev.size - term.x) and 
                len = prev.size - term.x in
                prev.chars <- String.sub prev.chars 0 term.x;
                prev.size <- term.x;
                term.x <- 0;
                {size = len; chars = str; hl = []}::l
    in term.text <- loop term.text i {size = 0; chars = ""; hl = []};
        term.numlines <- term.numlines + 1; update_hl ();;

(* Delete char in row at given position. Row must not be empty.
    param row: row to delete from
    param i: position of char to delete*)
let delete_char row i =
    term.changed <- true;
    row.chars <- String.sub row.chars 0 (i - 1) ^ 
        String.sub row.chars i (row.size - i);
    row.size <- row.size - 1;
    term.x <- term.x - 1;
    update_hl ();;

(* Delete row in text at given position
    param i: position of row to delete. must be > 0*)
let delete_row i =
    term.changed <- true;
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
                        e.hl <- e.hl @ last.hl;
                        e::text
                end
        | e::l -> e::loop l (i - 1)
    in term.text <- loop term.text i;;
