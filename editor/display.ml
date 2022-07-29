(*
    file: display.ml
    dependencies: editor.ml
    Editor display functions
*)

open Editor;;

(* Draw text on editor, tildes if buffer empty *)
let draw_rows () =
    let welcome_text =
        let text = "Oditor -- An editor for OCaml, in OCaml" and len = 39 in
        let offset = (term.cols - len) / 2 in
        "~" ^ String.make offset ' ' ^ text ^ "\r\n"
    and version_text =
        let offset = (term.cols - 13) / 2 in
        "~" ^ String.make offset ' ' ^ "version " ^ version ^ "\r\n"

    in let cut_lign line off =
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
            if y = term.rows / 2 + 2 then output_string stdout welcome_text
            else if y = term.rows / 2 then output_string stdout version_text
            else output_string stdout "~\r\n"; 
            draw (y - 1) []
    in draw (term.rows - 1) (prepare_text term.text term.rowoff);;

(* Show or hide cursor *)
let toggle_cursor hide =
    if hide then output_string stdout "\x1b[?25l"
    else output_string stdout "\x1b[?25h";;

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

