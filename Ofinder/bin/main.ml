(* open Lib.Cli *)
(* open Lib.Loop_nav *)
open Lib.Match_str
(* let home = Sys.getenv "$HOME/"  *)
(* let desktop = Sys.chdir  "Desktop/" *)

(* let cwd = cur_cwd () *)
(* let _ = Printf.printf "Starting Directory: %s \n" cwd *)
(* let home = Sys.getenv "HOME"  *)
(* let _ = change_dir home *)
(* let _ = Printf.printf "Ending Directory: %s" home *)
(**)

let res = my_match_str "Sunday" "Sun"
let upack r = 
    match r with 
    | Some(r) -> r 
    | None -> ""
let () = Printf.printf "The match is %s" (upack res)
let _ = print_endline ""
let res2 = my_match_str "Sunday" "day"
let () = Printf.printf "The match is %s" (upack res2)
let _ = print_endline ""
let res3 = my_match_str "Sunday" "lol"
let () = Printf.printf "The match is %s" (upack res3)

(* let () = start_file_loop ()  *)
