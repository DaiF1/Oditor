(* Editor Row definition *)
type erow = {
    mutable size : int;
    mutable chars : string
};;

(* Editor mode *)
type emode =
    | NORMAL
    | COMMAND
    | INSERT;;

let string_of_mode mode = match mode with
    | NORMAL -> "NORMAL"
    | COMMAND -> "COMMAND"
    | INSERT -> "INSERT";;

(* Terminal definition *)
type termio = {
    mutable rows : int;         (* Number of rows in terminal *) 
    mutable rowoff : int;       (* Current row offset *)
    mutable cols : int;         (* Number of columns in terminal *)
    mutable colsoff : int;      (* Current column offset *)
    mutable x : int;            (* Cursor x position *)
    mutable y : int;            (* Cursor y position *)
    mutable text : erow list;   (* Text buffer *)
    mutable numlines : int;     (* Number of lines in text buffer *)
    mutable mode : emode;       (* Current Editor mode*)
    mutable command : string;   (* Command buffer *)
    io : Unix.terminal_io       (* Editor terminal io *)
};;

(* Convert int option to int *)
let int_of_intop = function None -> 0 | Some n -> n;;

(* Global Editor *)
let term = 
    let (r, c) = (Terminal_size.get_rows (), Terminal_size.get_columns ()) in
    {
        rows = int_of_intop r;
        rowoff = 0;
        cols = int_of_intop c;
        colsoff = 0;
        x = 0;
        y = 0;
        text = [];
        numlines = 0;
        mode = NORMAL;
        command = "";
        io = Unix.tcgetattr Unix.stdin
    };;

(* Load current term size *)
let load_term_size () = 
    let (r, c) = (Terminal_size.get_rows (), Terminal_size.get_columns ()) in
    term.rows <- (int_of_intop r); term.cols <- (int_of_intop c);;

(* Initial terminal char size *)
let isize = term.io.c_csize;;
(* Initial terminal read minimal input *)
let imin = term.io.c_vmin;;
(* Initial terminal read timeout *)
let itime = term.io.c_vtime

(* Return ctrl+key code *)
(*let ctrl key = Char.chr ((Char.code key) land 0x1f);;*)

(* Open terminal raw mode
    Terminal with disabled echo and read byte by byte *)
let enter_raw () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { term.io with Unix.c_icanon = false; Unix.c_echo = false;
            Unix.c_isig = false; Unix.c_ixon = false; 
            Unix.c_icrnl = false; Unix.c_opost = false;
            Unix.c_brkint = false; Unix.c_inpck = false;
            Unix.c_istrip = false; Unix.c_csize = 8;
            Unix.c_vmin = 0; Unix.c_vtime = 1};;

(* Exit terminal raw mode *)
let exit_raw () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { term.io with Unix.c_icanon = true; Unix.c_echo = true;
            Unix.c_isig = true; Unix.c_ixon = true; 
            Unix.c_icrnl = true; Unix.c_opost = true;
            Unix.c_brkint = true; Unix.c_inpck = true;
            Unix.c_istrip = true; Unix.c_csize = isize;
            Unix.c_vmin = imin; Unix.c_vtime = itime};;


(* Return true if char is printable *)
(*let printable c =
    let c = Char.code c in
    c >= 32 && c < 127;;*)


(* Add line to text buffer *)
let add_line str len = 
    let rec loop text = match text with
        | [] -> [{chars = str; size = len}]
        | l::t -> l::loop t
    in term.text <- loop term.text;;

(* Open file in editor *)
let open_file path =
    let ic = open_in path in
    let read () = try Some (input_line ic) with End_of_file -> None in
    let rec loop () = match read () with
        | None -> close_in ic; 0
        | Some s -> add_line s (String.length s); loop () + 1
    in term.numlines <- loop ();;


(* Draw tildes on each row *)
let draw_rows () =
    let cut_lign line off =
        let max = term.cols in
        let l = String.length line in
        let len = if l - off > max then max
            else l - off in
        if len > 0 then String.sub line off len
        else ""
    in let rec prepare_text text off = match (text, off) with
        | (t, 0) -> t
        | ([], _) -> []
        | (_::t, o) -> prepare_text t (o - 1)
    in let rec draw y text = match (y, text) with
        | (0, _) -> output_string stdout "\x1b[K"; 
            let str = if term.mode = COMMAND then ":" ^ term.command
                else "" in
            output_string stdout str
        | (1, l) -> output_string stdout "\x1b[K"; 
            output_string stdout (string_of_mode term.mode); 
            output_string stdout "\r\n"; draw (y - 1) l
        | (y, l::t) ->
            output_string stdout "\x1b[K";
            output_string stdout (cut_lign l.chars term.colsoff);
            output_string stdout "\r\n"; draw (y - 1) t
        | (y, []) -> output_string stdout "\x1b[K"; 
            output_string stdout "~\r\n"; draw (y - 1) []
    in draw (term.rows - 1) (prepare_text term.text term.rowoff);;

(* Refresh editor screen *)
let refresh_screen () =
    load_term_size ();
    output_string stdout "\x1b[?25l";
    output_string stdout "\x1b[H";
    draw_rows ();
    output_string stdout (Printf.sprintf "\x1b[%d;%dH" (term.y + 1) (term.x + 1));
    output_string stdout "\x1b[?25h";
    flush stdout;;

(* Clear terminal screen *)
let clear_screen () =
    output_string stdout "\x1b[2J";
    output_string stdout "\x1b[H";
    flush stdout;;

(* Read byte from stdin
    Return the read byte *)
let read_key () = 
    try input_char stdin
    with End_of_file -> '\000';;

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
    term.y <- if term.y + y < 0 then
            begin
                term.rowoff <- if term.rowoff <= 0 then 0 
                else term.rowoff - 1; 0
            end
        else if term.y + y > term.rows then 
            begin
                term.rowoff <- if term.rowoff >= term.numlines - term.rows 
                then term.numlines - term.rows
                else term.rowoff + 1; term.rows
            end
        else term.y + y;;

(* Move cursor on screen based on key pressed *)
let move_cursor key = match key with
    | 'h' -> move_cx (-1)
    | 'l' -> move_cx 1
    | 'k' -> move_cy (-1)
    | 'j' -> move_cy 1
    | _ -> ();;

(* Execute command stored in buffer and empty it. 
    Return false if exit command entered *)
let read_command () = let result = match term.command with
        | "q" -> false
        | _ ->true
    in term.command <- ""; result;;

(* Process key presses. Return false if exit key pressed *)
let process_key () = 
    match term.mode with
        | NORMAL -> 
            begin
                match read_key () with
                    | c when c = ':' -> term.mode <- COMMAND; true
                    | c when c = 'i' -> term.mode <- INSERT; true 
                    | c -> move_cursor c; true
            end
        | COMMAND -> 
            begin
                match read_key () with
                    | '\000' -> true
                    | '\127' -> let l = String.length term.command in
                            term.command <- String.sub term.command 0 (l - 1);
                            true
                    | c when c = '\x1b' ->
                            let seq1 = read_key () in
                            if seq1 = '\000' then term.mode <- NORMAL; true
                    | c when c = '\r' -> read_command ()
                    | c -> term.command <- term.command ^ Char.escaped c; true
            end
        | INSERT ->
            begin
                match read_key () with
                    | c when c = '\x1b' ->
                            let seq1 = read_key () in
                            if seq1 = '\000' then term.mode <- NORMAL; true
                    | _ -> true
            end


                    (* Main loop
    Quit if ctrl+q is pressed *)
let rec loop () =
    refresh_screen ();
    if process_key () then loop ()
    else (clear_screen (); exit_raw ());;

let () = enter_raw (); open_file "oditor.ml"; loop ();;
