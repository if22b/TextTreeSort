# Project Name

## Description
This project reads a large text file (e.g., 'War and Peace') and builds a persistent red-black tree with unique words, storing them in sorted order.

## Features
- Reads and tokenizes a text file
- Builds a red-black tree of unique words
- Outputs sorted words to `output.txt`
- Supports large text files with parallel processing

## Usage
Compile the program:
```bash
ghc -o uniquesorted main.hs -package containers
```
Run the program:
```bash
./uniquesorted data/war_and_piece.txt
```
