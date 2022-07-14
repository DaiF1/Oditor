(* Return a termios for reading input.
    Copy of stdin with disabled echo and waiting for return key *)
let raw_term () =
    let term = Unix.tcgetattr Unix.stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { term with Unix.c_icanon = false; Unix.c_echo = false};
    term;;

(* Read a char from given termios
    Return the read char *)
let get_char term = 
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN term;
    res

let rec loop () =
    let term = raw_term () in
    if get_char term == 'q' then ()
    else loop ();;

let () = loop ();;
