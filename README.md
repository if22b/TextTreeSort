# Functional Programing - Project

Winter Semester 2024 - 5th Semester of BIF

Members:
- Luca Carpentieri
- Vladan Petkovic

## Project description (copied from FHTW-slides)
- Implement an application
    - Which, given a path to a text file on the disk, computes the sorted list of
unique words in this file using a red-black tree.
- So:
    - The application has one command line argument: the path to the text file to
be analyzed (for example, war and peace.txt)
    - The program reads the content, tokenizes it, and inserts it into a red-black
tree.
    - It then traverses this tree to get the unique words in sorted order.
    - The output is written to file „output.txt“.

## Usage with Makefile
Build the program with (ensure, that a bin-folder exists):
```
make
```
Run the program with:
```
bin/sort data/test.txt
```
or:
```
bin/sort data/war_and_piece.txt
```

and clean everything up with:
```
make clean
```

## Usage with script-files

### Windows
double-click the run_win.bat-file.

### Linux
Under linux, make the script executable by running:
```
chmod +x run_linux.sh
```
Then run it with:
```
./run_linux.sh
```
