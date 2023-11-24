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
