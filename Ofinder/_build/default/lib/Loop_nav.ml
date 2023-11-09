open ANSITerminal 
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


let start_file_loop () = ()


(* Define a function to draw a box, given its top left position and size *)
let draw_box (x, y) (w, h) =
  (* Draw top and bottom *)
  for x' = x to x + w do
    set_cursor x' y;
    Printf.printf "%s" "+";
    set_cursor x' (y + h);
    Printf.printf "%s" "+";
  done;

  (* Draw left and right *)
  for y' = y to y + h do
    set_cursor x y';
    Printf.printf "%s" "|";
    set_cursor (x + w) y';
    Printf.printf "%s" "|";
  done 
