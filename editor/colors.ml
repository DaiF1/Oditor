(*
    file: colors.ml
    dependencies: editor.ml
    Syntax highlighting implementation
*)

open Editor;;

(* Convert hltype to escape code *)
let hl_to_esc code = match code with
    | DEFAULT -> "\x1b[0m"
    | DIGIT -> "\x1b[34m"
    | STRING -> "\x1b[32m"
    | CHAR -> "\x1b[35m"
    | COMMENT -> "\x1b[37m"
    | KEYWORD -> "\x1b[31m";;

(* Return true if char is digit *)
let is_digit c = let code = Char.code c - Char.code '0'
    in code < 10 && code >= 0;;

(* Return true if char is separator *)
let is_separator c = String.contains_from ",.()+-/*=~%<>[]; \000" 0 c;;

(* Update row syntax highlighting *)
let update_hl row =
        let rec hl i prev_hl prev_sep = match i with
            | i when i = row.size -> []
            | i -> let chr = row.chars.[i] in begin
                match chr with
                    | chr when is_digit chr && (prev_sep || prev_hl = DIGIT) -> 
                            DIGIT::hl (i + 1) DIGIT false
                    | '.' when prev_hl = DIGIT && not prev_sep ->
                                DIGIT::hl (i + 1) DIGIT true
                    | chr -> DEFAULT::hl (i + 1) DEFAULT (is_separator chr)
                end
        in row.hl <- hl 0 DEFAULT true;;

(* Apply syntax highlighting to a given lign. Takes hltype of 
    last char of previous line. *)
let hl_row row prev =
    if row.size = 0 then row.chars
    else 
    let rec process_string hl i prev = match hl with
        | [] -> ""
        | e::hl -> if e <> prev then 
                    hl_to_esc e ^ Char.escaped row.chars.[i] ^ 
                    process_string hl (i + 1) e  
            else Char.escaped row.chars.[i] ^ process_string hl (i + 1) e 
    in process_string row.hl 0 prev ^ hl_to_esc DEFAULT;;

(* Cut syntax list into 2 sublists. 
    Returns a tuple with objects strictly before index and after *)
let rec cut_syntax i l = match l with
    | [] -> ([], [])
    | l when i = 0 -> ([], l)
    | e::l -> let (l1, l2) = cut_syntax (i - 1) l in
            (e::l1, l2);;
