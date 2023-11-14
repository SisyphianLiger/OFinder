let match_str s1 s2 = 
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true




let red f_str s_str = Printf.printf"\027[31m%s\027[0m\n%s" f_str s_str

let unpack l = 
    match l with
    | Some(l) -> l
    | None -> 0

let str_a_is_bigger str_a str_b = 
    let len_of_a = String.length str_a in
    let len_of_b = String.length str_b in
    if len_of_a < len_of_b then None else Some(len_of_a)
    
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
                                    Some(acc, (acc + len_b))
    in
    matcher str_a str_b 0





   




   




    
