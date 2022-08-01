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

(* Update row syntax highlighting *)
let update_hl row =
        let rec hl i = match i with
            | i when i = row.size -> []
            | i -> let chr = row.chars.[i] in
                if is_digit chr then DIGIT::hl (i + 1)
                else DEFAULT::hl (i + 1)
        in row.hl <- hl 0;;

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
