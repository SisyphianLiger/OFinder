(*  
        In order to search through the files or directories, 
        the following type is created.

        file_or_dir: 1 if dir 0 if file 
        fd_str : string that specifies file/dir path 
        len_match : start of sub_str in fd_str * length remaining to end pos 
        ls_score : the value derived from Levenshitein algorithm (used for sort)
             
 *)



type f_d_info = 
    { 
      fd_str : string;
      mutable len_match: int option;
      mutable ls_score: int option;
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
let start_path path = change_dir path



let list_of_fd path = 
    try Ok(Sys.readdir path |> Array.to_list)
    with Sys_error msg -> Error msg


(*  
    We use Path to change the directory if with test_path
    Then we will recursively loop through all files with the 
    root file name from path + the new extension
    And store them into a string list for processing
 *)

let rec find_fdl path = 
    start_path path;
    let dir_fd = list_of_fd path in
    match dir_fd with 
    | Error _ -> []
    | Ok xs ->  
            let rec add_path path xs acc  =
                match xs with 
                | []        ->  acc 
                | x::xs     ->  let is_dir = 
                                    try Sys.is_directory (path ^ "/" ^ x)
                                    with Sys_error _ -> false 
                                in
                                let ignore_ch = String.sub x 0 1 in
                                if is_dir && ignore_ch <> "." && ignore_ch <> "_" && x <> "node_modules" then 

                                    let new_dir = find_fdl (path ^ "/" ^ x) in
                                    
                                    let f_d_info = {
                                        fd_str = path ^ "/" ^ x;
                                        len_match = None;
                                        ls_score = None;
                                    } in

                                    add_path path xs (f_d_info::new_dir @ acc);
                                else 
                                    add_path path xs acc 
            in 
            add_path path xs []    

