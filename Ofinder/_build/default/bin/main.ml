open Lib.Cli
(* open Lib.Loop_nav *)
(* open Lib.Match_str *)

(* "/Users/ryanmac/Desktop/10.BootDev_Portfolio/Ofinder/" *)


let res = find_fdl "/Users/ryanmac/Desktop/10.BootDev_Portfolio/Ofinder/Ofinder"
let () = List.iter (fun fd -> print_endline (string_of_int fd.file_or_dir)) res

(* let () = start_file_loop ()  *)
