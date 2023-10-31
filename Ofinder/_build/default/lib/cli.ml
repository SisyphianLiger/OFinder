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
