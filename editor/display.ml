(*
    file: display.ml
    dependencies: editor.ml colors.ml
    Editor display functions
*)

open Editor;;
open Colors;;

let str_sub str off max = 
    let rec sub str off max esc = match str with
        | [] -> []
        | _::_ when max = 0 -> []
        | c::str -> begin
                    match c with
                    | '\x1b' -> c::sub str off max true
                    | c when esc && c = 'm' -> c::sub str off max false
                    | c when esc -> c::sub str off max esc
                    | _ when off > 0 -> sub str (off - 1) max esc
                    | c -> c::sub str off (max - 1) esc
               end
    in let str_l = str |> String.to_seq |> List.of_seq
    in sub str_l off max false |> List.to_seq |> String.of_seq;;

(* Draw text on editor, tildes if buffer empty *)
let draw_rows () =
    (* Oditor text description. Visible only with empty buffer *)
    let welcome_text =
        let text = "Oditor -- An editor for OCaml, in OCaml" and len = 39 in
        let offset = (term.cols - len) / 2 in
        "~" ^ String.make offset ' ' ^ text ^ "\r\n"
    (* Oditor version text. Visible only with empty buffer *)
    and version_text =
        let offset = (term.cols - 13) / 2 in
        "~" ^ String.make offset ' ' ^ "version " ^ version ^ "\r\n"
    (* Bottom status bar string *)
    and status_bar =
        (* Cursor position in file (in %) *)
        let completion = if term.numlines = 0 then 100
            else int_of_float (float_of_int (term.y + term.rowoff) /. 
                float_of_int (term.numlines - 1) *. 100.0) 
        (* Current file name *)
        in let file = if term.filename = "" then "[No Name]" else term.filename
        (* Editor mode *)
        in let status = 
            "\x1b[7m\x1b[1m " ^ string_of_mode term.mode ^ " \x1b[0m " ^ file
        (* Current lign and completion *)
        and stats = "line " ^ string_of_int (term.y + term.rowoff) ^ " (" ^
            string_of_int completion ^ "%)" in
        (* the '+11' is to nullify the escape codes for formatting text *)
        let offset = (term.cols - String.length status - String.length stats + 11)
        in status ^ String.make offset ' ' ^ stats

    (* Cut lign if larger than term size 
        param line: line to process (string)
        param off: terminal x offset *)
    in let cut_lign line off =
        str_sub line off term.cols
    (* Return text buffer after applying vertical offset
        param text: text to load
        param off: line offset before current text *)
    in let rec prepare_text text off = match (text, off) with
        | (t, 0) -> t
        | ([], _) -> []
        | (_::t, o) -> prepare_text t (o - 1)

    (* Draw each line of text on screen 
        param y: current lign index
        param text: text to write *)
    in let rec draw y text = match (y, text) with
        (* Last line of the screen. Used to write in command mode *)
        | (0, _) -> output_string stdout "\x1b[K"; (* Clear lign *)
            let str = if term.mode = COMMAND then ":" ^ term.command
                else term.help in
            output_string stdout str
        (* Status bar lign *)
        | (1, l) -> output_string stdout "\x1b[K"; (* Clear lign *)
            output_string stdout status_bar; 
            output_string stdout "\r\n"; draw (y - 1) l
        (* Default state. Write lign to screen *)
        | (y, l::t) ->
            output_string stdout "\x1b[K"; (* Clear lign *)
            output_string stdout (cut_lign (hl_row l DEFAULT) term.colsoff);
            output_string stdout "\r\n"; draw (y - 1) t
        (* Buffer empty case. Writes '~' or welcome_text to screen *)
        | (y, []) -> output_string stdout "\x1b[K"; (* Clear lign *)
            if term.text = [] then
            begin
                if y = term.rows / 2 + 2 then output_string stdout welcome_text
                else if y = term.rows / 2 then output_string stdout version_text
                else output_string stdout "~\r\n"
            end
            else output_string stdout "~\r\n";
            draw (y - 1) []
    in draw (term.rows - 1) (prepare_text term.text term.rowoff);;

(* Show or hide cursor 
    param hide: true if cursor needs to be hidden *)
let toggle_cursor hide =
    if hide then output_string stdout "\x1b[?25l"
    else output_string stdout "\x1b[?25h";;

(* Refresh editor screen *)
let refresh_screen () =
    load_term_size ();
    toggle_cursor true;
    (* Reset cursor position *)
    output_string stdout "\x1b[H";
    draw_rows ();
    (* Set cursor to current position *)
    output_string stdout (Printf.sprintf "\x1b[%d;%dH" (term.y + 1) (term.x + 1));
    toggle_cursor false;
    flush stdout;;

(* Clear terminal screen *)
let clear_screen () =
    (* Clear screen *)
    output_string stdout "\x1b[2J";
    (* Reset cursor position *)
    output_string stdout "\x1b[H";
    flush stdout;;

