(*
    file: colors.ml
    dependencies: editor.ml
    Syntax highlighting implementation
*)

open Editor;;

(* Convert hltype to escape code
    param code: hltype to convert *)
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

(* Update row syntax highlighting 
    param row: erow to process
    param prev: last hltype of previous row
    return: last hltype of the row *)
let update_hl_row row prev =
    (* build hl list of length 'len' and filled with 'key' elements *)
    let rec build_key_hl len key =
        if len = 0 then []
        else key::build_key_hl (len - 1) key
    (* Check if keyword 'keys' is in row starting from 'i' position *)
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
    (* Build hl list with 'i' current index, 'prev_hl' hltype of prev char,
        'prev_sep' if prev char was a separator.
        Returns a tuple with the list and the last 'prev_hl' *)
    in let rec hl i prev_hl prev_sep = match i with
        | i when i = row.size -> ([], prev_hl)
        | i -> let chr = row.chars.[i] in begin
            match chr with
                (* Process digit *)
                | chr when is_digit chr && (prev_sep || prev_hl = DIGIT) -> 
                        let (h, prev) = hl (i + 1) DIGIT false in
                            (DIGIT::h, prev)
                | '.' when prev_hl = DIGIT && not prev_sep ->
                        let (h, prev) = hl (i + 1) DIGIT true in
                            (DIGIT::h, prev)
                (* Process strings *)
                | '"' when prev_hl = STRING || prev_hl = DEFAULT ->
                        if prev_hl = STRING then
                            let (h, prev) = hl (i + 1) DEFAULT false in
                                (STRING::h, prev)
                        else let (h, prev) = hl (i + 1) STRING false in
                            (STRING::h, prev)
                (* Process chars *)
                | '\'' when prev_hl = DEFAULT ->
                        if i + 2 < row.size && row.chars.[i + 2] = '\'' then
                            let (h, prev) = hl (i + 3) CHAR true in
                                (CHAR::CHAR::CHAR::h, prev)
                        else let (h, prev) = hl (i + 1) DEFAULT true in
                                (DEFAULT::h, prev)
                (* Process comments *)
                | '(' when prev_hl <> STRING ->
                        if i + 1 < row.size && row.chars.[i + 1] = '*' then
                            let (h, prev) = hl (i + 2) COMMENT false in
                            (COMMENT::COMMENT::h, prev)
                        else let (h, prev) = hl (i + 1) DEFAULT true in
                            (KEYWORD2::h, prev)
                | '*' when prev_hl = COMMENT ->
                        if i + 1 < row.size && row.chars.[i + 1] = ')' then
                            let (h, prev) = hl (i + 2) DEFAULT true in
                            (COMMENT::COMMENT::h, prev)
                        else let (h, prev) = hl (i + 1) COMMENT false in
                            (COMMENT::h, prev)
                (* Process other chars *)
                | chr -> 
                        if prev_hl = STRING then
                            let (h, prev) = hl (i + 1) STRING false in 
                                (STRING::h, prev)
                        else if prev_hl = COMMENT then
                            let (h, prev) = hl (i + 1) COMMENT false in
                                (COMMENT::h, prev)
                        else let n = check_kw other_keywords i in
                            if n <> 0 then
                                let (h, prev) = hl (i + n) DEFAULT true in
                                (build_key_hl n KEYWORD2 @ h, prev)
                        else if prev_sep then
                            let n = check_kw keywords i in
                            if n = 0 then 
                                let (h, prev) = hl (i + 1) DEFAULT (is_separator chr) in
                                (DEFAULT::h, prev)
                            else let (h, prev) = hl (i + n) DEFAULT false in
                                (build_key_hl n KEYWORD1 @ h, prev)
                        else let (h, prev) = hl (i + 1) DEFAULT (is_separator chr) in
                            (DEFAULT::h, prev)
            end
    in let (h, prev) = hl 0 prev true in
        row.hl <- h; prev;;

(* Update file syntax highlighting *)
let update_hl () =
    let rec update text prev = match text with
        | [] -> ()
        | e::l -> update l (update_hl_row e prev)
    in update term.text DEFAULT;;

(* Apply syntax highlighting to a given lign.
    param row: erow to process
    param prev: last hltype of previous row *)
let hl_row row prev =
    if row.size = 0 then row.chars
    else 
    let rec process_string hl i prev = match hl with
        | [] -> ""
        | e::hl -> if e <> prev then 
                    hl_to_esc e ^ String.make 1 row.chars.[i] ^ 
                    process_string hl (i + 1) e  
            else String.make 1 row.chars.[i] ^ process_string hl (i + 1) e 
    in process_string row.hl 0 prev ^ hl_to_esc DEFAULT;;

(* Cut syntax list into 2 sublists.
    Returns a tuple with objects strictly before index and after *)
let rec cut_syntax i l = match l with
    | [] -> ([], [])
    | l when i = 0 -> ([], l)
    | e::l -> let (l1, l2) = cut_syntax (i - 1) l in
            (e::l1, l2);;
