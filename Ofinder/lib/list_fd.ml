(*  
        In order to search through the files or directories, 
        the following type is created.

        file_or_dir: 1 if dir 0 if file 
        fd_str : string that specifies file/dir path 
        sub_str_pnt : start of sub_str in fd_str * length remaining to end pos 
        ls_score : the value derived from Levenshitein algorithm (used for sort)
             
 *)



type f_d_info = 
    { file_or_dir : int;
      fd_str : string;
      sub_str_pnt: int * int;
      ls_score: int
    }

    

(* let home = Sys.getenv "$HOME/"  *)
(* let desktop = Sys.chdir  "Desktop/" *)

(* let cwd = cur_cwd () *)
(* let _ = Printf.printf "Starting Directory: %s \n" cwd *)
(* let home = Sys.getenv "HOME"  *)
(* let _ = change_dir home *)
(* let _ = Printf.printf "Ending Directory: %s" home *)
