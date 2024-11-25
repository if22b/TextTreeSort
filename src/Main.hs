module Main where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Char (isAlpha, toLower)
import System.IO (openFile, hGetContents, IOMode(..))
import System.Environment (getArgs)
import RBTree ( inOrder, buildTree )

-- Liest den gesamten Inhalt einer Datei und gibt ihn als String zurück
readFileContents :: FilePath -> IO String
readFileContents filePath = do
    handle <- openFile filePath ReadMode  -- Datei im Lesemodus öffnen
    hGetContents handle  -- Den gesamten Inhalt der Datei lesen

-- Tokenisiert den Text, indem alle Wörter in Kleinbuchstaben umgewandelt und Satzzeichen entfernt werden
-- Jedes Wort wird in Kleinbuchstaben umgewandelt und Wörter, die leer sind, werden gefiltert
tokenize :: String -> [String]
tokenize content = 
    let wordsOnly = map (map toLower . filter isAlpha) (words content)
    in filter (not . null) wordsOnly  -- Entfernt leere Wörter

-- Parallele Tokenisierung großer Textdateien
-- Die Funktion splittet die Datei in Zeilen und wendet Tokenisierung auf jede Zeile parallel an
parallelTokenize :: String -> [String]
parallelTokenize content = concat $ parMap rpar tokenize (lines content)

-- Schreibt die sortierten Wörter in eine Ausgabedatei
-- Jede Zeile der Datei enthält ein Wort
writeSortedWords :: FilePath -> [String] -> IO ()
writeSortedWords outputPath words = writeFile outputPath (unlines words)

-- Funktion zum Verarbeiten der Datei
processFile :: FilePath -> IO ()
processFile filePath = do
    content <- readFileContents filePath  -- Dateiinhalt einlesen
    let wordsList = parallelTokenize content  -- Tokenisierung und parallele Verarbeitung anwenden
        tree = buildTree wordsList  -- Wörter in den Baum einfügen
        sortedList = inOrder tree  -- Baum in eine sortierte Liste umwandeln
    writeSortedWords "output.txt" sortedList  -- Sortierte Wörter in Datei schreiben
    printMessage "Sortierte eindeutige Wörter wurden in output.txt gespeichert."

printMessage :: String -> IO ()
printMessage = putStrLn

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> processFile filePath
        _ -> printMessage "Error, try this usage: bin/sort <dateipfad>"