(*
    file: default_keymaps.ml
    dependencies: editor.ml input.ml display.ml insert.ml movement.ml
    Default key bindings
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
        | 'D' -> move_cx (-1)
        | 'C' -> move_cx 1
        | 'A' -> move_cy (-1)
        | 'B' -> move_cy 1
        | 'L' -> start_word term.x
        | 'H' -> back_word term.x
        | 'K' -> move_cy 10
        | 'J' -> move_cy (-10)
        | _ -> ()
    in toggle_cursor true; process (); 
        let linelen = (get_line (term.y + term.rowoff)).size in
        term.x <- if term.x > linelen then linelen
                else term.x;
        toggle_cursor false;;

(* File management command type *)

type file_status = {
    mutable openfile : bool;
    mutable savefile : bool;
};;

let fstatus = {
    openfile = false;
    savefile = false;
};;

let read_command () =
    let entries = String.split_on_char ' ' term.command in
    term.command <- "";
    match entries with
    | [] -> true
    | cmd::args ->
            if fstatus.openfile then
            begin
                fstatus.openfile <- false;
                open_file cmd;
                term.mode <- INSERT; true
            end
            else if fstatus.savefile then
            begin
                fstatus.savefile <- false;
                term.mode <- INSERT;
                term.filename <- cmd;
                write_file cmd; true
            end
            else if cmd = "setkmap" then
            begin
                match args with
                    | [] -> term.mode <- INSERT; true
                    | map::_ -> load_keymap map; true
            end
            else (term.mode <- INSERT; true);;

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
    | INSERT -> let seq1 = read_key () in
        if seq1 = '\000' then 
        begin
            term.command <- "";
            true
        end else 
        if seq1 = '[' then
        begin
            let seq2 = read_key () in
            if seq2 = '1' then
                let _ = read_key () and
                _ = read_key () and
                seq5 = read_key () in
                if seq5 = 'D' then move_cursor 'H'
                else if seq5 = 'C' then move_cursor 'L'
                else if seq5 = 'A' then move_cursor 'J'
                else if seq5 = 'B' then move_cursor 'K';
            else
                move_cursor seq2;
            true
        end
        else true
        
    | COMMAND -> let seq1 = read_key () in
        if seq1 = '\000' then term.mode <- INSERT; true
    | NORMAL -> true;;

let process_return mode = match mode with
    | COMMAND -> read_command ()
    | INSERT -> term.y <- term.y + 1;
        insert_row (term.y + term.rowoff); true
    | NORMAL -> true;;

let process_tab mode = match mode with
    | INSERT -> insert_string (get_line (term.y + term.rowoff)) 
        "    " 4 (term.x + term.colsoff); true
    | _ -> true;;

(* Ctrl inputs *)

let process_ctrl_q mode = match mode with
    | INSERT -> if term.changed then
                begin
                    term.help <- "Unsaved changes. Press again to exit anyways.";
                    term.mode <- NORMAL;
                    true
                end
                else false
    | NORMAL-> false
    | COMMAND -> true;;

let process_ctrl_backquote mode = match mode with
    | INSERT -> term.command <- ":";
            term.mode <- COMMAND; term.help <- ""; true
    | _ -> true;;

let process_ctrl_o mode = match mode with
    | INSERT -> if term.changed then
                begin
                    term.help <- "Unsaved changes. Save file before opening a new one";
                    true
                end
                else 
                begin
                    term.mode <- COMMAND;
                    fstatus.openfile <- true;
                    term.help <- "";
                    true
                end
    | _ -> true;;

let process_ctrl_s mode = match mode with
    | INSERT -> if term.filename = "" then
                begin
                    term.mode <- COMMAND;
                    fstatus.savefile <- true;
                    term.help <- "";
                    true
                end
                else 
                begin
                    fstatus.savefile <- true;
                    term.help <- "";
                    write_file term.filename;
                    true
                end
    | _ -> true

let process_ctrl_majs mode = match mode with
    | INSERT -> begin
            term.mode <- COMMAND;
            fstatus.savefile <- true;
            term.help <- "";
            true
        end
    | _ -> true

(* Default input *)

let process_char c mode = match mode with
    | COMMAND -> term.command <- term.command ^ String.make 1 c; true
    | INSERT -> insert_char (get_line (term.y + term.rowoff)) c 
            (term.x + term.colsoff); true
    | _ -> true;;

let setup_defaultkeymaps () =
    insert_row 0;
    term.changed <- false;
    term.mode <- INSERT;
    add_key '\127' process_back;
    add_key '\x1b' process_esc;
    add_key '\r' process_return;
    add_key '\t' process_tab;
    add_key (ctrl 'q') process_ctrl_q;
    add_key (ctrl 'r') process_ctrl_backquote;
    add_key (ctrl 'o') process_ctrl_o;
    add_key (ctrl 's') process_ctrl_s;
    add_key (ctrl 'S') process_ctrl_majs;
    set_default_proc process_char;;
