# OFinder

This is the Personal Project for Boot.dev

The goal is to make a nvim. 

# TODOS:


## Task 1
Generalize Starting point: write now I have hard coded my base path
and put in in my zshrc file, however I intend to create a make file 
that will echo the path information to run the command in terminal.

The command I use is ofz.

## Task 2 
Find Optimizations both in searching the file system and in displaying
the system to the Users, and matching string algorithm

## Task 3
Implement Tests to check for validity of the LSD/Match/Directory Search 


# UPDATE:

The Fuzzy Finder first searches through your file system for you 
directories using the Ocaml Sys module. Once it finds all the files,
an event loop is started. The user is able to use the following 
commands:

    Type on the keyboard: Search Term will appear to the user at the bottom
    See incoming Files: the top files are displayed
    Navigate the directories:  Using Up/Down Arrows
    Backspace: Deleting characters for the user
    Enter: Enters the Directory selected
    Esc: Exits the Program

Upon Search terms for each char the List is sorted based on the 
Levenshteins Distance Algorithm. 

# Tutorial on How to Set up

Make sure the following has been downloaded via opam
## Dependencies 

opam --version 2.1.5

1. ocaml                --version 5.0.0
2. dune                 --version 3.11.1
3. base                 --version 0.16.3
4. stdio                --version 0.16.0
5. ppx_jane             --version 0.16.0
7. curses               --version 1.0.11

## Change the Main String Path Variable

    Main Line 11
    let path = "/Users/yourname/Desktop" in

## Create a Zsh/Bash Script

    #!/bin/zsh
    dune exec Ofinder
    cd $(cat target_dir.txt)
    exec zsh

## Set Up an Alias you zsh, bash . file i.e. /.zshrc 

    # Fuzzy Finder Alias and set up
    alias ofz='cd Your/Path/To/Dune/Ofinder && ./ofz.sh'
    
    Make sure chmod x+ the ofz.sh file!

    
