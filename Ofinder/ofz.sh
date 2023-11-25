echo "Starting script"
echo "Finished Ocaml execution, now changing directory"
dune exec Ofinder
cd $(cat target_dir.txt)
echo "Changed directory to $(pwd)"

