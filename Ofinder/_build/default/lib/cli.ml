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



(* makes cwd str *)
let cur_cwd () = Sys.getcwd ()

(* Used to change cwd *)
let change_dir s = Sys.chdir s


(* Finding Env
 USER: Contains the current username.
 PATH: Lists directories where executables are searched for.
 PWD: Holds the path to the current working directory.
 OLDPWD: Shows the path of the previous working directory.
 SHELL: Contains the shell the user is utilizing.
*)
let get_env_loc s = Sys.getenv_opt s


let start_desktop_path () = change_dir "/Users/ryanmac/Desktop/"
let test_path path = change_dir path

let list_of_fd path = 
    try Ok(Sys.readdir path |> Array.to_list)
    with Sys_error msg -> Error msg




(*  
    Path will be used as the first cd 
    Then we will recursively loop through all files with the 
    root file name from path + the new extension
    And store them into a string list for processing
 *)

let rec find_fdl path = 
    test_path path;
    let dir_fd = list_of_fd path in
    match dir_fd with 
    | Error m -> [m]
    | Ok xs ->  
            let rec add_path path xs acc  =
                match xs with 
                | []        -> acc 
                | x::xs     ->  let is_dir = 
                                    try Sys.is_directory x 
                                    with Sys_error _ -> false 
                                in
                                if is_dir then 
                                    let new_dir = find_fdl (path ^ "/" ^ x) in
                                    add_path path xs (new_dir @ acc);
                                else 
                                    add_path path xs (( path ^"/" ^x)::acc) 
            in 
            add_path path xs []
    

                                              
