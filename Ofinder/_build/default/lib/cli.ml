(*  

        Record type: f_d_info

        fd_str : string that specifies dir path 
        len_match : start of sub_str in fd_str * length remaining to end pos 
        ls_score : the value derived from Levenshitein algorithm (used for sort)
             
 *)

type f_d_info = 
    { 
      fd_str : string;
      mutable len_match: int option;
      mutable ls_score: int option;
    }



(* 
   cur_cwd 

   No Parameters

   Result: Wrapper function that gets a current working directory 
*)
let cur_cwd () = Sys.getcwd ()

(*
    change_dir

    s : MUST BE A DIR path cannot be a file 

    Result: Changes directory "cd \mypath"
 *)
let change_dir s = Sys.chdir s


(* Finding Env
 USER: Contains the current username.
 PATH: Lists directories where executables are searched for.
 PWD: Holds the path to the current working directory.
 OLDPWD: Shows the path of the previous working directory.
 SHELL: Contains the shell the user is utilizing.
*)
let get_env_loc s = Sys.getenv_opt s


let start_path path = change_dir path



(* 
    list_of_fd
    
    path: a string that is used to test the path

    Result: upon successful read returns an OK(dir), 
            upon error returns an Error msg

*)
let list_of_fd path = 
    try Ok(Sys.readdir path |> Array.to_list)
    with Sys_error msg -> Error msg


(*  
    find_fdl 

    path : The starting point string used to search paths

    Result : Returns a list of Dir paths put into the record 
    type f_d_info, that will then be used in to populate the 
    Event loop.

    Description : We use Path to change the directory if with test_path
    Then we will recursively loop through all files with the root file 
    name from path + the new extension And store them into a string 
    list for processing

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
                                let ignore_ch = String.get x 0 in
                                if is_dir && ignore_ch <> '.' && ignore_ch <> '_' && x <> "node_modules" then 

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

