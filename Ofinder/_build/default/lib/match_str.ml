let match_str s1 s2 = 
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let unpack l = 
    match l with
    | Some(l) -> l
    | None -> 0

let my_match_str str_a str_b = 
    let len_b = String.length str_b in 
    let len_a = String.length str_a in
    let len_a_b = if len_b > len_a then None else Some(len_b) in
    let len = unpack(len_a_b)  in
    let rec chcker acc str_a str_b = 
        match len with
        | 0               -> None
        | len             -> if str_b <> (String.sub str_a acc len) 
                                then 
                                    chcker (acc + 1) str_a str_b
                                else
                                    let highlighted = str_b in 
                                    let () = Printf.printf "len_b is %i len_a is %i len is %i acc is %i" len_b len_a len acc in
                                    Some("Match Found: " ^ "||" ^ highlighted ^"||" ^ (String.sub str_a len (len_a - len)))

    in
    chcker 0 str_a str_b

