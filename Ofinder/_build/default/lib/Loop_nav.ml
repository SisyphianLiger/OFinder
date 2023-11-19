open Curses
open Cli 
open Ls
open Match_str
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

    I have a sentence and I want to ask more questions.


 *)
(* supposed window initialization happened before this line *)
let print_in_red win str = 
    let _ = start_color () in
    let _ = init_pair 1 1 0 in (* initializing red *)
    wattron win (A.color_pair 1); (* turning on color *)
    ignore (waddstr win str); (* printing the string *)
    wattroff win A.(color_pair 1); (* turning off color *)
    refresh ()

(* Time to make the str_match Red *)
let str_colorizer win str ss = 
        let _ = colors () in
        let _ = init_pair 1 2 0 in
        match ss with
        | None                                   ->  str
        | Some (x,y)    ->  
                            let tuple_one = x in 
                            let tuple_two = y in 
                            let str_a_len = String.length str in 
                            let first_string = String.sub str 0 tuple_one in 
                            wattron win (A.color_pair 1);
                            let highlighted = String.sub str tuple_one (tuple_two - tuple_one) in
                            wattroff win (A.color_pair 1);
                            let second_string = String.sub str tuple_two (str_a_len - tuple_two) in 
                            first_string ^ " ==> " ^highlighted ^ " <== " ^ second_string


let unpacker_str x = if x = 0 then "F" else "D"


(* Helper Function for sorting *)
let compare_ls ls_one ls_two = 
    match ls_one.ls_score, ls_two.ls_score with
    | None, None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
    | Some x, Some y -> compare x y


let window_search path = 
    let mainwindow = initscr () in 
    let _ = colors () in

    (* we take this and use it to display the results *)
    let f_d_found = find_fdl path in
    
    (* Needed to read text contiguously *)
    let _ = cbreak () in
    let _ = noecho () in  

    let (maxy, maxx) = getmaxyx mainwindow in
    (* Window Trial *)
    (* let win = newwin 25 50 25 50 in  *)
    let win_top = newwin (maxy * 7 / 10) maxx 2 0 in
    let win_bot = newwin (maxy / 10)  maxx (maxy * 9 / 10) 0  in
    box win_top (Char.code '|') (Char.code '=');
    box win_bot (Char.code '|') (Char.code '=');
    let _ = refresh () in

    let _ = mvaddstr 1 2 "FILES OR DIRECTORIES" in

    let _ = mvaddstr ((maxy * 9 / 10) - 1) 2 "SEARCH TERM BOX:" in
    let _ = mvaddstr ((maxy * 9 / 10) + 1) 3 "" in

    let _ = wrefresh win_top in 
    let _ = wrefresh win_bot in 
   
    (* to display maximum amount of results *)
    let input_range = ((maxy * 7 / 10) - 2) in

    let backspace = 127 in 
    let escape = 27 in


    let ls_loop = ref true in
    let s_input = ref "" in
    while !ls_loop do 
        let key = getch () in 
        match key with 
        | key when key = backspace  ->  if String.length !s_input > 0 
                                        then 
                                            s_input := String.sub !s_input 0 (String.length !s_input - 1);
                                            ignore(mvaddstr ((maxy * 9 / 10) + 1) 2 (String.make maxx ' '));
                                            ignore(mvaddstr ((maxy * 9 / 10) + 1) 3 !s_input);
                                            List.iter (fun x -> x.ls_score <- Some(make_str_matrix !s_input x.fd_str);
                                                                x.sub_str_pnt <- my_match_str x.fd_str !s_input )f_d_found;
                                            (* Now we sort :( *)
                                            List.sort compare_ls f_d_found |> List.iteri (fun i x -> if i < input_range 
                                            then ignore(mvaddstr (input_range - i + 2) 2 ((unpacker_str x.file_or_dir) ^ " " ^  
                                            (str_colorizer win_top x.fd_str x.sub_str_pnt))));
                                            let _ = wrefresh win_top in 
                                            ls_loop := true

        | key when key = escape                         ->   ls_loop := false

        | _  when (String.length !s_input) < (maxx - 5) ->   s_input := !s_input  ^ Char.escaped (Char.chr key);            
                                                             ignore(mvaddstr ((maxy * 9 / 10) + 1) 3 !s_input); 
                                                             (* Output the match str *)
                                                             List.iter (fun x -> x.ls_score <- Some(make_str_matrix !s_input x.fd_str);
                                                                            x.sub_str_pnt <- my_match_str x.fd_str !s_input )f_d_found;
                                                             (* Now we sort :( *)
                                                             List.sort compare_ls f_d_found |> List.iteri (fun i x -> if i < input_range 
                                                             then ignore(mvaddstr (input_range - i + 2) 2 ((unpacker_str x.file_or_dir) ^ " " ^  
                                                             (str_colorizer win_top x.fd_str x.sub_str_pnt))

                                                             (* Here is where we change color based on str match *)
                                                             ));
                                                             let _ = wrefresh win_top in 
                                                             ls_loop := true

        | _                                             ->   ls_loop := true
    done;
    endwin ()
