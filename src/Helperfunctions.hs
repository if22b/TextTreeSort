module Helperfunctions where

import System.CPUTime ( getCPUTime )
import Text.Printf (printf)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Char (isAlpha, toLower)
import Data.List (splitAt)
import System.IO (openFile, hGetContents, IOMode(..))

readFileContents :: FilePath -> IO String
readFileContents filePath = do
    handle <- openFile filePath ReadMode
    hGetContents handle

-- in this function we are tokenizing the text with:
-- --> lowercase
-- --> removing punctuation marks
-- --> removing empty words
tokenize :: String -> [String]
tokenize content = filter (not . null) $ map (map toLower . filter isAlpha) (words content)

standardTokenize :: String -> [String]
standardTokenize content = concatMap tokenize (lines content)

parallelTokenize :: String -> [String]
parallelTokenize content = concat $ parMap rpar tokenize (lines content)

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