@echo off
ghc -o sort src/main.hs -package containers

.\sort data\war_and_piece.txt

pause