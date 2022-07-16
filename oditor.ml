(* Global terminal *)
let term = Unix.tcgetattr Unix.stdin;;

(* Open terminal raw mode
    Terminal with disabled echo and no return needed to read buffer *)
let enter_raw () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { term with Unix.c_icanon = false; Unix.c_echo = false};;

(* Exit terminal raw mode *)
let exit_raw () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { term with Unix.c_icanon = true; Unix.c_echo = true};;

(* Read a char from given termios
    Return the read char *)
let get_char () = 
    input_char stdin;;

(* Main loop
    Quit if letter 'q' pressed *)
let rec loop () =
    if get_char () == 'q' then exit_raw ()
    else loop ();;

let () = enter_raw (); loop ();;
