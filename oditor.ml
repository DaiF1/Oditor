(* Global terminal *)
let term = Unix.tcgetattr Unix.stdin;;
(* Initial terminal char size *)
let isize = term.c_csize;;
(* Initial terminal read minimal input *)
let imin = term.c_vmin;;
(* Initial terminal read timeout *)
let itime = term.c_vtime

(* Return ctrl+key code *)
let ctrl key = Char.chr ((Char.code key) land 0x1f);;

(* Open terminal raw mode
    Terminal with disabled echo and read byte by byte *)
let enter_raw () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { term with Unix.c_icanon = false; Unix.c_echo = false;
            Unix.c_isig = false; Unix.c_ixon = false; 
            Unix.c_icrnl = false; Unix.c_opost = false;
            Unix.c_brkint = false; Unix.c_inpck = false;
            Unix.c_istrip = false; Unix.c_csize = 8;
            Unix.c_vmin = 0; Unix.c_vtime = 1};;

(* Exit terminal raw mode *)
let exit_raw () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { term with Unix.c_icanon = true; Unix.c_echo = true;
            Unix.c_isig = true; Unix.c_ixon = true; 
            Unix.c_icrnl = true; Unix.c_opost = true;
            Unix.c_brkint = true; Unix.c_inpck = true;
            Unix.c_istrip = true; Unix.c_csize = isize;
            Unix.c_vmin = imin; Unix.c_vtime = itime};;


(* Return true if char is printable *)
(*let printable c =
    let c = Char.code c in
    c >= 32 && c < 127;;*)

(* Clear terminal screen *)
let clear_screen () =
    output_string stdout "\x1b[2J";
    output_string stdout "\x1b[H";;

(* Read byte from stdin
    Return the read byte *)
let read_key () = 
    try input_char stdin
    with End_of_file -> '\000';;

(* Process key presses *)
let process_key () = match read_key () with
    | c when c = ctrl 'q' -> clear_screen (); exit_raw (); 1
    | _ -> 0;;


(* Main loop
    Quit if ctrl+q is pressed *)
let rec loop () =
    if process_key () = 0 then loop ();;

let () = enter_raw (); loop ();;
