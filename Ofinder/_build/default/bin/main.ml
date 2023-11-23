open Lib.Loop_nav
open Lib.Cli

let main () = 
    let path = "/Users/ryanmac/Desktop" in
    let res = find_fdl path in
    List.filteri (fun i _ -> i < 40) res |> List.iter (fun x -> Printf.printf "%s \n" x.fd_str);
    (* key_test path; *)
    (* let _ = Sys.command "nvim ." in *)
    ()

let () = main ()
