# HASH : A Shell (Bash extention) written in Haskell.
![hash logo](https://raw.githubusercontent.com/IITH-SBJoshi/haskell-9/readmd/hash.jpg?token=AJAXZZ4AZITDUQTSTD5WFQC4ZRO6E)
![hash_gif](https://raw.githubusercontent.com/IITH-SBJoshi/haskell-9/gif/hash.gif?token=AJAXZZ47GX7MVHV735BAJ3C4ZSB6U)
## About

This project is a partial fullfillment for CS2433 (Principles of Programming Languages II) offered by Dr. Saurabh Joshi at IIT Hyderabad in Spring'19 semester.

## Dependencies
The dependencies for this project are the following:
1) Stack - Haskell Project Manager
2) System Modules for Haskell (Unix, directory, etc. full list in .cabal file)
3) Haskeline
4) Tasty

## Features
This project is an extended subset of the bash shell for unix.
There are various handy features, some of which are:
1) git config reading and git info display on prompt.
2) .rc file parsing to store aliases.
3) Intuitive builtins.

## Instructions
In the project root directory run `stack run`.

### Built-In Commands
- `help`: Displays this text
- `cd`: Change working Directory
- `history`: Display previous commands entered
- `export`: set export attribute for variables <export keyword optional>
- `unset`: unset export attribute for variables
- `find`: search for files in a directory hierarchy. <no args lists all files>

## Contributors

Sai Harsha Kottapalli <br>
Sagar Jain <br>
Tanmay Renugunta <br>
Srinivas Bogga
