open Lib.Loop_nav

let main () = 
    let write_to_file output_area filename content =
        let complete_path = output_area ^ filename in
        let open_channel = open_out complete_path in
        let () = output_string open_channel content in
        close_out open_channel
    in
    let output_area = "/Users/ryanmac/Desktop/9.BootDev_Portfolio/Ofinder/Ofinder/" in
    let path = "/Users/ryanmac/Desktop" in
    let target_dir_path = tui_searcher path in
    let _ = write_to_file output_area "target_dir.txt" target_dir_path in
    let _ = Sys.command ("nvim " ^ target_dir_path) in
    ()

let () = main ()
