module Helperfunctions where

import System.CPUTime ( getCPUTime )
import Text.Printf (printf)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Char (isAlpha, toLower)
import System.IO (openFile, hGetContents, IOMode(..))

-- Liest den gesamten Inhalt einer Datei und gibt ihn als String zurück
readFileContents :: FilePath -> IO String
readFileContents filePath = do
    handle <- openFile filePath ReadMode  -- Datei im Lesemodus öffnen
    hGetContents handle  -- Den gesamten Inhalt der Datei lesen

-- in this function we are tokenizing the text with:
-- --> lowercase
-- --> removing punctuation marks
-- --> removing empty words
tokenize :: String -> [String]
tokenize content = 
    let wordsOnly = map (map toLower . filter isAlpha) (words content)
    in filter (not . null) wordsOnly

-- Parallele Tokenisierung großer Textdateien
-- Die Funktion splittet die Datei in Zeilen und wendet Tokenisierung auf jede Zeile parallel an
parallelTokenize :: String -> [String]
parallelTokenize content = concat $ parMap rpar tokenize (lines content)

-- Schreibt die sortierten Wörter in eine Ausgabedatei
-- Jede Zeile der Datei enthält ein Wort
writeSortedWords :: FilePath -> [String] -> IO ()
writeSortedWords outputPath words = writeFile outputPath (unlines words)

measureTimeOfFunction :: IO () -> IO ()
measureTimeOfFunction action = do
    startTime <- getCPUTime
    action
    endTime <- getCPUTime
    printf "Time elapsed: %.3f seconds\n" (fromIntegral (endTime - startTime) / 10^12 :: Double)

printMessage :: String -> IO ()
printMessage = putStrLn
