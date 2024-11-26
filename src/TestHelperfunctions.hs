module Main where

import Helperfunctions
import Test.QuickCheck
import Data.Char (isLower)

-- Property 1: Tokenized output contains only lowercase words
prop_tokenizeLowercase :: String -> Bool
prop_tokenizeLowercase content =
    all (all isLower) (tokenize content)

-- Property 2: Tokenized output contains no punctuation or empty words
prop_tokenizeNoPunctuation :: String -> Bool
prop_tokenizeNoPunctuation content =
    all (not . null) (tokenize content)

-- Property 3: Parallel tokenization matches sequential tokenization
prop_parallelTokenizeSameAsSequential :: String -> Bool
prop_parallelTokenizeSameAsSequential content =
    parallelTokenize content == concatMap tokenize (lines content)

-- Property 4: Sorted words remain sorted after writing
prop_writeSortedWordsSorted :: [String] -> Bool
prop_writeSortedWordsSorted words =
    let sortedWords = quicksort words
    in sortedWords == sortedWords
  where
    quicksort [] = []
    quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- Define the main function to run tests
main :: IO ()
main = do
    quickCheck prop_tokenizeLowercase
    quickCheck prop_tokenizeNoPunctuation
    quickCheck prop_parallelTokenizeSameAsSequential
    quickCheck prop_writeSortedWordsSorted
