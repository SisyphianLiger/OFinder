open Curses
(* Take in Input 
   u
    From Ocaml.org docs 
    While loops are useful with references
    If we use a ref here that being false, 
    we are saying while the loop is not true 
    print_string q to quit and if we see that 
    the string.[0] is true then we set file_loop to true 
    setting the boolean value to the reference file_loop 
    to true terminating the loop

    Initialize an empty search string before the loop starts.
    In your loop, take each character input from read_line ()
    and append it to your search string.
    Apply your algorithms to the search string inside the loop.
    If you do this, every time a new character is inputted, 
    the search string would get longer by one character, 
    and the algorithms would be applied to an increasingly 
    longer string, just as you intended.


 *)

(* attron (A.color_pair 1); *)
(* attroff (A.color_pair 1); *)
let create_color_window () = 
    (* intializing library and color functionality *) 
    let mainwindow = initscr () in
    let _ = cbreak () in
    let _ = noecho () in  
    let _ = start_color () in

    (* Line Implementations *)
    let (maxy, maxx) = getmaxyx mainwindow in
    let _ = hline (Char.code '=') maxx in
    let side_line  = int_of_float(float_of_int(maxy) /. 0.72) in
    let _ = mvhline (maxy - side_line) 0 (Char.code '=') maxx in
    let _ = mvhline (maxy - 17) 0 (Char.code '=') maxx in
    let _ = mvhline (maxy - 1) 0 (Char.code '=') maxx in


    let _ = mvvline 0 (maxx - 1) (Char.code '|') (maxy - 16) in
    let _ = mvvline 0 (maxx - 2) (Char.code '|') (maxy - 16) in
    let _ = mvvline 0 1 (Char.code '|') (maxy - 16) in
    let _ = mvvline 0 0 (Char.code '|') (maxy - 16) in

    let _ = mvvline (maxy - 15) (maxx - 1) (Char.code '|') 50 in
    let _ = mvvline (maxy - 15) (maxx - 2) (Char.code '|') 50 in
    let _ = mvvline (maxy - 15) 0  (Char.code '|') 50 in
    let _ = mvvline (maxy - 15) 1  (Char.code '|') 50 in

    let _ = refresh () in
    (* Line Implementations *)
    List.init 10 (fun _ -> "Hello World") |>
    List.mapi (fun i sentence -> mvaddstr (maxx / 3 + i + 1) ((maxx - 20)) sentence) |> ignore;

    let _ = mvaddstr (maxy / 3) ((maxx - 20) / 2) "Press any key to exit." in
    let _ = mvaddstr (maxy - 10) ((maxx - 20) / 2) "Press any key to exit." in

    let _ = getch () in 
    endwin ()




let create_window () = 
    let _ = initscr () in
    let _ = start_color () in 
    let win = newwin 50 50 0 0 in
    (* let pair = init_pair 1 3 7 in *)
    (* Initialize keypad listening *)
    let _ = keypad win true in
    (* let _ = wattron win pair in *)
    let _ = waddstr win "Hello World" in

    let s_input = ref "" in
    let file_loop = ref false in 
    let key_backspace = 127 in 
    let key_escape = 27 in
    while not !file_loop do 
        let key = getch () in 
        match key with 
        | key when key = key_backspace -> if String.length !s_input > 0 then
                            s_input := String.sub !s_input 0 (String.length !s_input - 1)
        | key when key = key_escape -> file_loop := true

        | _ ->  s_input := !s_input  ^ Char.escaped (Char.chr key);
    done;
    (* let _ = refresh () in *)
    (* let input = getch () in  *)
    (* let _ = move 50 50 in *)
    (* Printf.printf "%s" input; *)
    endwin ()



(* let start_file_loop () =  *)
(*     () *)

let start_file_loop () = 
    (* Initialize curses *)
    let win = initscr () in

    (* Initialize keypad listening *)
    let _ = keypad win true in

    (* Disable echo of input characters *)
    let _ = noecho () in  

    (* Initialize s_input and while loop*)
    let s_input = ref "" in
    let file_loop = ref false in 
    print_endline "";
    let key_backspace = 127 in 
    let key_escape = 27 in

    while not !file_loop do 
        let key = getch () in 
        match key with 
        | key when key = key_backspace -> if String.length !s_input > 0 then
                            s_input := String.sub !s_input 0 (String.length !s_input - 1)
        | key when key = key_escape -> file_loop := true

        | _ ->  s_input := !s_input  ^ Char.escaped (Char.chr key);
    done;

    let _ = waddstr win !s_input in
    let _ = wrefresh win in

    endwin ()
