module Main where

import System.Environment (getArgs)
import RBTree ( inOrder, buildTree )
import Helperfunctions
    ( readFileContents,
      parallelTokenize,
      writeSortedWords,
      measureTimeOfFunction,
      printMessage )

processFile :: FilePath -> IO ()
processFile filePath = do
    content <- readFileContents filePath
    writeSortedWords "output.txt" (inOrder (buildTree (parallelTokenize content)))
    printMessage "Done!"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> measureTimeOfFunction (processFile filePath)
        _ -> printMessage "Error, try this usage: bin/sort <dateipfad>"