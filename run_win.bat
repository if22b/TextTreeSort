@echo off
ghc -o sort src/Main.hs src/RBTree.hs src/Helperfunctions.hs -package containers

.\sort data\war_and_peace.txt

pause