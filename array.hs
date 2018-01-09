{-
-- array.hs
-- @author Sidharth Mishra
-- @description Given an array of integers, a start point, and a end point, print out all the broken
-- intervals.
-- For eg: start = 1, end = 100, array = {2,3,4,6,7}, print {5, 8-100}
-- @copyright 2017 Sidharth Mishra
-- @created Mon Jan 08 2018 22:59:10 GMT-0800 (PST)
-- @last-modified Mon Jan 08 2018 22:59:10 GMT-0800 (PST)
-}
import Data.List (sort)

-- | Reads data from the STDIN as a string of comma separated digits.
-- Then, sorts the list of digits parsed from the input string.
readDigits :: String -> [Int]
readDigits ("") = []
readDigits s = sort $ map read (words (map replaceCommasAndBrackets s))
    where
        replaceCommasAndBrackets :: Char -> Char
        replaceCommasAndBrackets (',') = ' '
        replaceCommasAndBrackets ('{') = ' '
        replaceCommasAndBrackets ('}') = ' '
        replaceCommasAndBrackets c = c

-- | Computes the missing ranges from the list, given the start and end points.
computeMissingRng :: Int -> Int -> String -> [Int] -> String
computeMissingRng start end rng (x:xs) = "range"


main :: IO ()
main = do
    s <- getLine
    start <- getLine
    end <- getLine
    (putStrLn . computeMissingRng (read start :: Int) (read end :: Int) "" . readDigits) s