(* 
    unpack 
    
    l:  the length of str_a should be bigger than str_b, 
        to check we unpack the length

    Result: Gives a length for Some(l),
            Gives 0 for None
*)
let unpack l = 
    match l with
    | Some(l) -> l
    | None -> 0

(* 
    str_a_is_bigger

    str_a : will be the dir str
    str_b : the search_term str 

    Result : a < b == None 
             b > a == Some (l) 

    This is then transferred to unpack 
*)

let str_a_is_bigger str_a str_b = 
    let len_of_a = String.length str_a in
    let len_of_b = String.length str_b in
    if len_of_a < len_of_b then None else Some(len_of_a)
   

(* 
    my_match_str

    str_a : the dir file str 
    str_b : the search term 

    Result

 *)
let my_match_str str_a str_b = 
    let len_chk = str_a_is_bigger str_a str_b in 
    let len_b = String.length str_b in
    let len_a = String.length str_a in
    let len = unpack len_chk in 
    let rec matcher str_a str_b acc =
        match len with 
        | 0                                                         -> None 
        | _      when acc > (len_a - len_b)                         -> None
        | _               -> if str_b <> (String.sub str_a acc len_b)
                                then matcher str_a str_b (acc + 1)
                                else 
                                    Some(len_b)
    in
    matcher str_a str_b 0





   




   




    
