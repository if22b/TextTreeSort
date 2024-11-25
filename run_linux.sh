#!/bin/bash
ghc -o sort src/Main.hs src/RBTree.hs -package containers

./sort data/war_and_piece.txt