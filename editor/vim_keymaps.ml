(*
    file: vim_keymaps.ml
    dependencies: editor.ml input.ml
    Bindings for vim like controls
*)

open Editor;;
open Input;;
open Files;;
open Display;;
open Insert;;
open Movement;;

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
                            term.help <- "Unsaved changes. Use 'q!' to override.";
                            term.mode <- NORMAL;
                            true
                        end
                        else false
                | "q!" -> false
                | "w" -> begin
                        match args with
                            | [] when term.filename = "" -> term.mode <- NORMAL;
                                term.help <- "No file name given."; true
                            | [] -> term.mode <- NORMAL; 
                                write_file term.filename; true
                            | file::_ -> term.mode <- NORMAL;
                                write_file file; true
                        end
                | "wq" | "x" -> begin
                        match args with
                            | [] when term.filename = "" -> term.mode <- NORMAL;
                                term.help <- "No file name given."; true
                            | [] -> term.mode <- NORMAL; 
                                write_file term.filename; false
                            | file::_ -> term.mode <- NORMAL;
                                write_file file; false
                        end
                | "edit" ->
                    if term.changed then
                    begin
                        term.help <- "Unsaved changes. Use 'edit!' to override.";
                        term.mode <- NORMAL;
                        true
                    end else
                    begin
                        match args with
                            | [] -> term.mode <- NORMAL;
                                    term.help <- "No file given"; true
                            | file::_ -> open_file file;
                                term.mode <- NORMAL; true
                    end
                | "edit!" ->
                    begin
                        match args with
                            | [] -> term.mode <- NORMAL;
                                    term.help <- "No file given"; true
                            | file::_ -> open_file file;
                                term.mode <- NORMAL; true
                    end
                | "setkmap" -> begin
                    match args with
                        | [] -> term.mode <- NORMAL; true
                        | map::_ -> load_keymap map; true
                    end

                | c -> term.mode <- NORMAL;
                    term.help <- c ^ ": unknown command"; true
            in term.command <- ""; result;;


(*** Input Processing ***)

(* Special inputs *)

let process_back mode = match mode with
    | COMMAND -> let l = String.length term.command in
            term.command <- 
                if l > 0 then String.sub term.command 0 (l - 1)
                else ""; true
    | INSERT -> if term.x <> 0 then 
                    let row = get_line (term.y + term.rowoff) in
                    delete_char row (term.x + term.colsoff)
                else delete_row (term.y + term.rowoff); true
    | NORMAL -> true;;

let process_esc mode = match mode with
    | COMMAND -> let seq1 = read_key () in
        if seq1 = '\000' then 
            (term.mode <- NORMAL; term.command <- ""); true
    | INSERT -> let seq1 = read_key () in
        if seq1 = '\000' then term.mode <- NORMAL; true
    | NORMAL -> true;;

let process_return mode = match mode with
    | COMMAND -> read_command ()
    | INSERT -> term.y <- term.y + 1;
        insert_row (term.y + term.rowoff); true
    | NORMAL -> term.mode <- COMMAND; term.help <- ""; true;;

let process_tab mode = match mode with
    | INSERT -> insert_string (get_line (term.y + term.rowoff)) 
        "    " 4 (term.x + term.colsoff); true
    | _ -> true

(* Mode change inputs *)

let process_ddots mode = match mode with
    | NORMAL -> term.mode <- COMMAND; term.help <- ""; true
    | COMMAND -> term.command <- term.command ^ String.make 1 ':'; true
    | INSERT -> insert_char (get_line (term.y + term.rowoff)) ':' 
            (term.x + term.colsoff); true;;

let process_i mode = match mode with
    | NORMAL -> term.mode <- INSERT;
        if term.text = [] then insert_row 0; true 
    | COMMAND -> term.command <- term.command ^ String.make 1 'i'; true
    | INSERT -> insert_char (get_line (term.y + term.rowoff)) 'i' 
            (term.x + term.colsoff); true;;

let process_I mode = match mode with
    | NORMAL -> term.x <- 0; term.mode <- INSERT;
            if term.text = [] then insert_row 0; true 
    | COMMAND -> term.command <- term.command ^ String.make 1 'I'; true
    | INSERT -> insert_char (get_line (term.y + term.rowoff)) 'I' 
            (term.x + term.colsoff); true;;

let process_a mode = match mode with
    | NORMAL -> term.x <- term.x + 1; term.mode <- INSERT;
            if term.text = [] then insert_row 0; true
    | COMMAND -> term.command <- term.command ^ String.make 1 'a'; true
    | INSERT -> insert_char (get_line (term.y + term.rowoff)) 'a' 
            (term.x + term.colsoff); true;;

let process_A mode = match mode with
    | NORMAL -> term.x <- (get_line term.y).size; term.mode <- INSERT;
            if term.text = [] then insert_row 0; true
    | COMMAND -> term.command <- term.command ^ String.make 1 'A'; true
    | INSERT -> insert_char (get_line (term.y + term.rowoff)) 'A' 
            (term.x + term.colsoff); true;;

(* Control inputs *)

let process_ctrl_w mode = match mode with
    | COMMAND -> term.command <- ""; true
    | _ -> true;;

(* Default input *)

let process_char c mode = match mode with
    | NORMAL -> term.help <- ""; move_cursor c; true
    | COMMAND -> term.command <- term.command ^ String.make 1 c; true
    | INSERT -> insert_char (get_line (term.y + term.rowoff)) c 
            (term.x + term.colsoff); true;;

let setup_vimkeymaps () =
    term.mode <- NORMAL;
    add_key '\127' process_back;
    add_key '\x1b' process_esc;
    add_key '\r' process_return;
    add_key '\t' process_tab;
    add_key ':' process_ddots;
    add_key 'i' process_i;
    add_key 'I' process_I;
    add_key 'a' process_a;
    add_key 'A' process_A;
    add_key (ctrl 'w') process_ctrl_w;
    set_default_proc process_char;;
