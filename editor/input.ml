(*
    file: input.ml
    dependencies: editor.ml files.ml insert.ml
    Process editor input
*)

open Editor;;
open Files;;
open Display;;
open Insert;;

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

(* Move to start of next word. Goto the end of the line if no word found *)
let start_word i =
    let row = get_line term.y in
    let rec next i n = if i < row.size then
            let chr = row.chars.[i] in
            if chr = ' ' then n + 1
            else next (i + 1) (n + 1)
        else n
    in term.x <- term.x + next i 0;;

(* Move to end of next word. Goto the end of the line if no word found *)
let end_word i =
    let row = get_line term.y in
    let rec next i n = if i < row.size then
            let chr = row.chars.[i] in
            if chr = ' ' && n <> 1 then n - 1
            else next (i + 1) (n + 1)
        else n
    in term.x <- term.x + next i 0;;

(* Move to start of previous word. Goto start of the line if no word found *)
let back_word i =
    let row = get_line term.y in
    let rec next i n = if i > 0 then
            let chr = row.chars.[i - 1] in
            if chr = ' ' && n <> 0 then n
            else next (i - 1) (n + 1)
        else n + 1
    in term.x <- term.x - next i 0;;

(* Move cursor on screen based on key pressed *)
let move_cursor key = 
    let process () = match key with
        | 'h' -> move_cx (-1)
        | 'l' -> move_cx 1
        | 'k' -> move_cy (-1)
        | 'j' -> move_cy 1
        | 'w' -> start_word term.x
        | 'e' -> end_word term.x
        | 'b' -> back_word term.x
        | _ -> ()
    in toggle_cursor true; process (); 
        let linelen = (get_line (term.y + term.rowoff)).size in
        term.x <- if term.x > linelen then linelen
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
                | "q" -> if term.changed then 
                        begin
                            term.help <- "Unsaved changes. Use 'q!' to override";
                            term.mode <- NORMAL;
                            true
                        end
                        else false
                | "q!" -> false
                | "w" -> begin
                        match args with
                            | [] when term.filename = "" -> term.mode <- NORMAL;
                                term.help <- "No file name"; true
                            | [] -> term.mode <- NORMAL; 
                                write_file term.filename; true
                            | file::_ -> term.mode <- NORMAL;
                                write_file file; true
                        end
                | "wq" -> begin
                        match args with
                            | [] when term.filename = "" -> term.mode <- NORMAL;
                                term.help <- "No file name"; false
                            | [] -> term.mode <- NORMAL; 
                                write_file term.filename; false
                            | file::_ -> term.mode <- NORMAL;
                                write_file file; false
                        end
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

(* Return true if ctrl+k was pressed *)
let ctrl key = Char.chr ((Char.code key) land 0x1f);;

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
                    | ':' -> term.mode <- COMMAND; term.help <- ""; true
                    | 'i' -> term.mode <- INSERT;
                        if term.text = [] then insert_row 0; true 
                    | 'a' -> term.x <- term.x + 1; term.mode <- INSERT;
                        if term.text = [] then insert_row 0; true
                    | 'I' -> term.x <- 0; term.mode <- INSERT;
                        if term.text = [] then insert_row 0; true 
                    | 'A' -> term.x <- (get_line term.y).size; 
                        term.mode <- INSERT;
                        if term.text = [] then insert_row 0; true
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
                    | c when c = ctrl 'w' -> term.command <- ""; true
                    | '\x1b' -> let seq1 = read_key () in
                            if seq1 = '\000' then 
                                (term.mode <- NORMAL; term.command <- ""); 
                            true
                    | '\r' -> read_command ()
                    | c -> term.command <- term.command ^ Char.escaped c; true
            end
        | INSERT ->
            begin
                match read_key () with
                    | '\000' -> true
                    | '\x1b' -> let seq1 = read_key () in
                            if seq1 = '\000' then term.mode <- NORMAL; true
                    | '\127' ->
                            if term.x <> 0 then 
                                let row = get_line term.y in
                                delete_char row term.x
                            else delete_row term.y; true
                    | '\r' -> term.y <- term.y + 1;
                            insert_row term.y; true
                    | '\t' -> insert_string (get_line term.y) "    " 4 term.x; true
                    | '\'' -> insert_string (get_line term.y) "'" 1 term.x; true
                    | c -> insert_char (get_line term.y) c term.x; true
            end

