module Main where

import Helperfunctions ( parallelTokenize, tokenize )
import Test.QuickCheck ( quickCheck )
import Data.Char (isLower)

prop_tokenizeLowercase :: String -> Bool
prop_tokenizeLowercase content =
    all (all isLower) (tokenize content)

prop_tokenizeNoPunctuation :: String -> Bool
prop_tokenizeNoPunctuation content =
    all (not . null) (tokenize content)

prop_parallelTokenizeSameAsSequential :: String -> Bool
prop_parallelTokenizeSameAsSequential content =
    parallelTokenize content == concatMap tokenize (lines content)

main :: IO ()
main = do
    quickCheck prop_tokenizeLowercase
    quickCheck prop_tokenizeNoPunctuation
    quickCheck prop_parallelTokenizeSameAsSequential