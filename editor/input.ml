(*
    file: input.ml
    dependencies: editor.ml files.ml 
    Process editor input
*)

open Editor;;
open Files;;
open Display;;

(*** Cursor input ***)

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

(* Move cursor on screen based on key pressed *)
let move_cursor key = 
    let process () = match key with
        | 'h' -> move_cx (-1)
        | 'l' -> move_cx 1
        | 'k' -> move_cy (-1)
        | 'j' -> move_cy 1
        | _ -> ()
    in toggle_cursor true; process (); 
        let linelen = (get_line (term.y + term.rowoff)).size in
        term.x <- if term.x >= linelen then linelen - 1
                else term.x;
        toggle_cursor false;;


(*** Command mode input ***)

(* Execute command stored in buffer and empty it. 
    Return false if exit command entered *)
let read_command () = 
    let entries = String.split_on_char ' ' term.command in
    match entries with 
        | [] ->  true
        | c::args -> 
            let result = match c with
                | "q" -> false
                | "edit" ->
                    begin
                        match args with
                            | [] -> term.mode <- NORMAL;
                                    term.help <- "No file given"; true
                            | file::_ -> open_file file;
                                term.mode <- NORMAL; true
                    end
                | c -> term.mode <- NORMAL;
                    term.help <- c ^ ": unknown command"; true
            in term.command <- ""; result;;


(*** Keyboard input ***)

(* Read byte from stdin
    Return the read byte *)
let read_key () = 
    try input_char stdin
    with End_of_file -> '\000';;

(* Process key presses. Return false if exit key pressed *)
let process_key () = 
    match term.mode with
        | NORMAL -> 
            begin
                match read_key () with
                    | '\000' -> true
                    | ':' -> term.mode <- COMMAND; true
                    | 'i' -> term.mode <- INSERT; true 
                    | c -> term.help <- ""; move_cursor c; true
            end
        | COMMAND -> 
            begin
                match read_key () with
                    | '\000' | ':' -> true
                    | '\127' -> let l = String.length term.command in
                            term.command <- 
                                if l > 0 then String.sub term.command 0 (l - 1)
                                else "";
                            true
                    | '\x1b' -> let seq1 = read_key () in
                            if seq1 = '\000' then term.mode <- NORMAL; true
                    | '\r' -> read_command ()
                    | c -> term.command <- term.command ^ Char.escaped c; true
            end
        | INSERT ->
            begin
                match read_key () with
                    | '\x1b' -> let seq1 = read_key () in
                            if seq1 = '\000' then term.mode <- NORMAL; true
                    | _ -> true
            end

