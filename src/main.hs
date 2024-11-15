module Main where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Char (isAlpha, toLower)
import System.IO (openFile, hGetContents, IOMode(..))
import System.Environment (getArgs)

-- Definiert die Farbe der Knoten im Rot-Schwarz-Baum
-- Jeder Knoten kann entweder rot oder schwarz sein
data Color = Red | Black deriving (Show, Eq)

-- Struktur eines Rot-Schwarz-Baums
-- Ein Knoten enthält die Farbe Rot oder Schwarz, 
-- - einen linken und rechten Teilbaum und
-- - einen Wert vom Typ 'a'
data RBTree a = Empty | Node Color (RBTree a) a (RBTree a) deriving (Show)

-- Funktion zum Einfügen eines neuen Elements in den Rot-Schwarz-Baum
insert :: (Ord a) => a -> RBTree a -> RBTree a
insert x t = makeBlack (ins t)  -- Die Wurzel nach dem Einfügen schwarz machen
  where
    -- Funktion zum Einfügen des Elements x an der passenden Stelle
    ins Empty = Node Red Empty x Empty  -- Leerer Baum: neuer Knoten wird rot
    ins (Node color left val right)
      | x < val = balance color (ins left) val right  -- Einfügen im linken Teilbaum
      | x > val = balance color left val (ins right)  -- Einfügen im rechten Teilbaum
      | otherwise = Node color left val right  -- Andernfalls Duplikate ignorieren

    -- Stellt sicher, dass die Wurzel immer schwarz ist, um die Baum-Eigenschaften zu bewahren
    makeBlack (Node _ left val right) = Node Black left val right
    makeBlack Empty = Empty

-- Funktion, um die Rot-Schwarz-Eigenschaften nach einem Einfügevorgang zu gewährleisten
-- Diese Funktion wird aufgerufen, wenn durch das Einfügen eine Regel verletzt wurde
balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a

-- Links-Links-Fall
-- Ein schwarzer Knoten hat links einen roten Knoten, der einen roten linken Kindknoten hat
-- `y` wird zur neuen Wurzel, `x` und `z` werden zu schwarzen Kindern
balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)

-- Links-Rechts-Fall
-- Ein schwarzer Knoten hat links einen roten Knoten, der einen roten rechten Kindknoten hat
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)

-- Fall 3: Rechts-Links-Fall
-- Ein schwarzer Knoten hat rechts einen roten Knoten, der einen roten linken Kindknoten hat
balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)

-- Fall 4: Rechts-Rechts-Fall
-- Ein schwarzer Knoten hat rechts einen roten Knoten, der einen roten rechten Kindknoten hat
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)

-- Wenn keine Balance nötig ist, bleibt der Baum gleich
balance color left val right = Node color left val right 

-- In-Order-Durchlauf durch den Baum, um eine sortierte Liste zu erstellen
-- Der In-Order-Durchlauf liefert die Elemente in aufsteigender Reihenfolge
inOrder :: RBTree a -> [a]
inOrder Empty = []
inOrder (Node _ left val right) = inOrder left ++ [val] ++ inOrder right

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

-- Fügt alle Wörter aus einer Liste in den Rot-Schwarz-Baum ein und baut dadurch den Baum auf
buildTree :: [String] -> RBTree String
buildTree = foldr insert Empty  -- `foldr` fügt jedes Wort der Reihe nach in den Baum ein

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
    putStrLn "Sortierte eindeutige Wörter wurden in output.txt gespeichert."

-- Hauptfunktion, die die Kommandozeilenargumente verarbeitet und den gesamten Prozess startet
-- Erwartet den Pfad zur Eingabedatei als Argument
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> processFile filePath  -- Datei verarbeiten, wenn Pfad übergeben wurde
        _ -> putStrLn "Verwendung: uniquesorted <dateipfad>"  -- Anleitung für fehlerhafte Eingabe