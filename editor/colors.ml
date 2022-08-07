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
    | KEYWORD1 -> "\x1b[31m"
    | KEYWORD2 -> "\x1b[33m";;

(* OCaml keywords in alphabetic order *)
let keywords = ["and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint";
    "do"; "done"; "downto"; "else"; "end"; "exception"; "external"; "false"; 
    "for"; "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit"; 
    "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match";
    "method"; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; 
    "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true";
    "try"; "type"; "val"; "virtual"; "when"; "while"; "with"
];;

(* Ocaml other keywords in alphabetic order *)
let other_keywords = ["!="; "#"; "&"; "&&"; "'"; "("; ")"; "*"; "*."; "+"; "+."; 
    ","; "-"; "-."; "->"; "."; ".."; ".~"; ":"; "::"; ":="; ":>"; ";"; ";;"; 
    "<"; "<-"; "="; ">"; ">]"; ">}"; "?"; "["; "[<"; "[>"; "[|"; "]"; "_"; 
    "`"; "{"; "{<"; "|"; "|]"; "||"; "}"; "~"
]

(* Return true if char is digit *)
let is_digit c = let code = Char.code c - Char.code '0'
    in code < 10 && code >= 0;;

(* Return true if char is separator *)
let is_separator c = String.contains_from ",.()+-/*=~%<>[]; \000" 0 c;;

(* Update row syntax highlighting *)
let update_hl_row row prev=
    let rec build_key_hl len key =
        if len = 0 then []
        else key::build_key_hl (len - 1) key
    in let rec check_kw keys i = match keys with
        | [] -> 0
        | e::_ when e.[0] > row.chars.[i] -> 0
        | e::l -> if e.[0] <> row.chars.[i] then check_kw l i
                else let n = String.length e in
                    if i + n <= row.size then
                        let str = String.sub row.chars i n in
                        if str = e then 
                            if i + n = row.size ||
                                is_separator (row.chars.[i + n]) then n
                            else check_kw l i 
                        else check_kw l i 
                    else check_kw l i 
    in let rec hl i prev_hl prev_sep = match i with
        | i when i = row.size -> []
        | i -> let chr = row.chars.[i] in begin
            match chr with
                | chr when is_digit chr && (prev_sep || prev_hl = DIGIT) -> 
                        DIGIT::hl (i + 1) DIGIT false
                | '.' when prev_hl = DIGIT && not prev_sep ->
                            DIGIT::hl (i + 1) DIGIT true
                | '"' when prev_hl = STRING || prev_hl = DEFAULT ->
                        if prev_hl = STRING then
                            STRING::hl (i + 1) DEFAULT false
                        else STRING::hl (i + 1) STRING false
                | '(' when prev_hl <> STRING ->
                        if i + 1 < row.size && row.chars.[i + 1] = '*' then
                            COMMENT::COMMENT::hl (i + 2) COMMENT false
                        else KEYWORD2::hl (i + 1) DEFAULT true
                | '*' when prev_hl = COMMENT ->
                        if i + 1 < row.size && row.chars.[i + 1] = ')' then
                            COMMENT::COMMENT::hl (i + 2) DEFAULT true
                        else COMMENT::hl (i + 1) COMMENT false

                | chr -> 
                        if prev_hl = STRING then
                            STRING::hl (i + 1) STRING false
                        else if prev_hl = COMMENT then
                            COMMENT::hl (i + 1) COMMENT false
                        else let n = check_kw other_keywords i in
                            if n <> 0 then
                            build_key_hl n KEYWORD2 @ hl (i + n) DEFAULT true
                        else if prev_sep then
                            let n = check_kw keywords i in
                            if n = 0 then 
                                DEFAULT::hl (i + 1) DEFAULT (is_separator chr)
                            else build_key_hl n KEYWORD1 @ hl (i + n) DEFAULT false 
                        else DEFAULT::hl (i + 1) DEFAULT (is_separator chr)
            end
    in row.hl <- hl 0 prev true;;

(* Update file syntax highlighting *)
let update_hl () =
    let rec get_prev hl = match hl with
        | [] -> DEFAULT
        | [e] -> e
        | _::l -> get_prev l
    in let rec update text prev = match text with
        | [] -> ()
        | e::l -> let p = if get_prev prev = COMMENT then COMMENT
                    else DEFAULT in
                update_hl_row e p; update l e.hl
    in update term.text [];;

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
