(* Global terminal *)
let term = Unix.tcgetattr Unix.stdin;;
(* Initial terminal char size. Used to reset terminal parameters *)
let isize = term.c_csize;;

(* Open terminal raw mode
    Terminal with disabled echo and read byte by byte *)
let enter_raw () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { term with Unix.c_icanon = false; Unix.c_echo = false;
            Unix.c_isig = false; Unix.c_ixon = false; 
            Unix.c_icrnl = false; Unix.c_opost = false;
            Unix.c_brkint = false; Unix.c_inpck = false;
            Unix.c_istrip = false; Unix.c_cisize = 8};;

(* Exit terminal raw mode *)
let exit_raw () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { term with Unix.c_icanon = true; Unix.c_echo = true;
            Unix.c_isig = true; Unix.c_ixon = true; 
            Unix.c_icrnl = true; Unix.c_opost = true
            Unix.c_brkint = true; Unix.c_inpck = true;
            Unix.c_istrip = true; Unix.c_csize = isize};;

(* Read a char from given termios
    Return the read char *)
let get_char () = 
    input_char stdin;;

(* Return true if char is printable *)
let printable c =
    let c = Char.code c in
    c >= 32 && c < 127;;

(* Main loop
    Quit if letter 'q' pressed *)
let rec loop () =
    let c = get_char () in
    if c == 'q' then exit_raw ()
    else
        begin
            begin
                if printable c then
                    (Printf.printf "%d ('%c')\r\n" (Char.code c) c;
                    flush stdout)
                else
                    (Printf.printf "%d\r\n" (Char.code c);
                    flush stdout)
            end;
            loop ()
        end;;

let () = enter_raw (); loop ();;
