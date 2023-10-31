open Lib.Cli
open Lib.Loop_nav
(* let home = Sys.getenv "$HOME/"  *)
(* let desktop = Sys.chdir  "Desktop/" *)

let cwd = cur_cwd ()
let _ = Printf.printf "Starting Directory: %s \n" cwd
let home = Sys.getenv "HOME" 
let _ = change_dir home
let _ = Printf.printf "Ending Directory: %s" home


let () = start_file_loop () 
