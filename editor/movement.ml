(*
    file: movement.ml
    dependencies: editor.ml
    Implementation of cursor movement
*)
open Editor;;

(* Get line at given position 
    param i: line index *)
let get_line i =
    let rec loop text i = match text with
        | [] -> {size = 0; chars = ""; hl = []}
        | e::_ when i = 0 -> e
        | _::l -> loop l (i - 1)
    in loop term.text i;;

(* Move cursor on x axis *)
let move_cx x =
    term.x <- if term.x + x < 0 then
            begin
                term.colsoff <- if term.colsoff <= 0 then 0
                else term.colsoff - 1; 0
            end
        else if term.x + x > term.cols then 
            begin 
                term.colsoff <- term.colsoff + 1; term.cols
            end
        else term.x + x;;

(* Move cursor on y axis *)
let move_cy y =
    term.y <- 
        if term.y + y >= term.numlines && term.numlines <= term.rows - 2 then
            term.numlines - 1
        else if term.y + y < 0 then
            begin
                term.rowoff <- if term.rowoff <= 0 then 0 
                else term.rowoff - 1; 0
            end
        else if term.y + y > term.rows - 3 then 
            begin
                term.rowoff <- if term.rowoff >= term.numlines - term.rows + 2
                then term.numlines - term.rows + 2
                else term.rowoff + 1; term.rows - 3
            end
        else term.y + y;;

(* Move to start of next word. Goto the end of the line if no word found *)
let start_word i =
    let row = get_line (term.y + term.rowoff) in
    let rec next i n = if i < row.size then
            let chr = row.chars.[i] in
            if chr = ' ' then n + 1
            else next (i + 1) (n + 1)
        else n
    in term.x <- term.x + next i 0;;

(* Move to end of next word. Goto the end of the line if no word found *)
let end_word i =
    let row = get_line (term.y + term.rowoff) in
    let rec next i n = if i < row.size then
            let chr = row.chars.[i] in
            if chr = ' ' then
                if n > 1 then n - 1
                else n + 1
            else next (i + 1) (n + 1)
        else n
    in term.x <- term.x + next i 0;;

(* Move to start of previous word. Goto start of the line if no word found *)
let back_word i =
    let row = get_line (term.y + term.rowoff) in
    let rec next i n = if i > 0 then
            let chr = row.chars.[i - 1] in
            if chr = ' ' && n <> 0 then n
            else next (i - 1) (n + 1)
        else n
    in term.x <- term.x - next i 0;;


