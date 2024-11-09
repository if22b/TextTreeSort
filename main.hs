module Main where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Char (isAlpha, toLower)
import System.IO (openFile, hGetContents, IOMode(..))
import System.Environment (getArgs)

-- Define the color of the red-black tree nodes
data Color = Red | Black deriving (Show, Eq)

-- Define the Red-Black Tree structure
data RBTree a = Empty | Node Color (RBTree a) a (RBTree a) deriving (Show)

-- Insert a new element into the red-black tree
insert :: (Ord a) => a -> RBTree a -> RBTree a
insert x t = makeBlack (ins t)
  where
    ins Empty = Node Red Empty x Empty
    ins (Node color left val right)
      | x < val = balance color (ins left) val right
      | x > val = balance color left val (ins right)
      | otherwise = Node color left val right  -- ignore duplicates

    makeBlack (Node _ left val right) = Node Black left val right
    makeBlack Empty = Empty

-- Balance function to maintain red-black properties after insertion
balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balance color left val right = Node color left val right

-- In-order traversal to produce a sorted list of words from the tree
inOrder :: RBTree a -> [a]
inOrder Empty = []
inOrder (Node _ left val right) = inOrder left ++ [val] ++ inOrder right

-- Read the file contents
readFileContents :: FilePath -> IO String
readFileContents filePath = do
    handle <- openFile filePath ReadMode
    hGetContents handle

-- Tokenize text by filtering out punctuation and converting to lowercase
tokenize :: String -> [String]
tokenize content = 
    let wordsOnly = map (map toLower . filter isAlpha) (words content)
    in filter (not . null) wordsOnly

-- Parallel tokenization for large files
parallelTokenize :: String -> [String]
parallelTokenize content = concat $ parMap rpar tokenize (lines content)

-- Insert all words into the red-black tree
buildTree :: [String] -> RBTree String
buildTree = foldr insert Empty

-- Write sorted words to output file
writeSortedWords :: FilePath -> [String] -> IO ()
writeSortedWords outputPath words = writeFile outputPath (unlines words)

-- Process the file: tokenize, build tree, and write output
processFile :: FilePath -> IO ()
processFile filePath = do
    content <- readFileContents filePath
    let wordsList = parallelTokenize content
        tree = buildTree wordsList
        sortedList = inOrder tree
    writeSortedWords "output.txt" sortedList
    putStrLn "Sorted unique words written to output.txt"

-- Main function to handle command-line arguments and run the process
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> processFile filePath
        _ -> putStrLn "Usage: uniquesorted <file_path>"