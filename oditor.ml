(* Editor Row definition *)
type erow = {
    mutable size : int;
    mutable chars : string
};;

(* Terminal definition *)
type termio = {
    mutable rows : int; 
    mutable cols : int;
    mutable x : int;
    mutable y : int;
    mutable text : erow list;
    io : Unix.terminal_io 
};;

(* Convert int option to int *)
let int_of_intop = function None -> 0 | Some n -> n;;

(* Global Term *)
let term = 
    let (r, c) = (Terminal_size.get_rows (), Terminal_size.get_columns ()) in
    {
        rows = int_of_intop r;
        cols = int_of_intop c;
        x = 0;
        y = 0;
        text = [];
        io = Unix.tcgetattr Unix.stdin
    };;

(* Initial terminal char size *)
let isize = term.io.c_csize;;
(* Initial terminal read minimal input *)
let imin = term.io.c_vmin;;
(* Initial terminal read timeout *)
let itime = term.io.c_vtime

(* Return ctrl+key code *)
let ctrl key = Char.chr ((Char.code key) land 0x1f);;

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
        | None -> close_in ic
        | Some s -> add_line s (String.length s); loop ()
    in loop ();;


(* Draw tildes on each row *)
let draw_rows () =
    let rec draw y text = match (y, text) with
        | (0, []) -> output_string stdout "\x1b[K"; 
            output_string stdout "~"
        | (0, l::_) -> output_string stdout "\x1b[K";
            output_string stdout l.chars
        | (y, l::t) ->
            output_string stdout "\x1b[K";
            output_string stdout l.chars;
            output_string stdout "\r\n"; draw (y - 1) t
        | (y, []) -> output_string stdout "\x1b[K"; 
            output_string stdout "~\r\n"; draw (y - 1) []
    in draw (term.rows - 2) term.text;;

(* Refresh editor screen *)
let refresh_screen () =
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
    term.x <- if term.x + x < 0 then 0
        else if term.x + x > term.cols then term.cols
        else term.x + x;;

(* Move cursor on y axis *)
let move_cy y =
    term.y <- if term.y + y < 0 then 0
        else if term.y + y > term.rows then term.rows
        else term.y + y;;

(* Move cursor on screen based on key pressed *)
let move_cursor key = match key with
    | 'h' -> move_cx (-1)
    | 'l' -> move_cx 1
    | 'k' -> move_cy (-1)
    | 'j' -> move_cy 1
    | _ -> ();;

(* Process key presses. Return false if exit key pressed *)
let process_key () = match read_key () with
    | c when c = ctrl 'q' -> clear_screen (); exit_raw (); false
    | c -> move_cursor c; true;;


(* Main loop
    Quit if ctrl+q is pressed *)
let rec loop () =
    refresh_screen ();
    if process_key () then loop ();;

let () = enter_raw (); open_file "oditor.ml"; loop ();;
