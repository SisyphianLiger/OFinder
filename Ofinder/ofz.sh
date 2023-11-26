#!/bin/zsh
dune exec Ofinder
cd $(cat target_dir.txt)
nvim .
exec zsh

