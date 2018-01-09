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

-- | computeMissingRng : computes the missing ranges
computeMissingRng :: [Int] -> Int -> Int -> String -> Bool -> String
computeMissingRng (x1:x2:xs) s e rng isContinuous
    | s > e || s > x1 || last xs > e = rng
    | (x1 - s > 1) && (x2 - x1 == 1) = if (!isContinuous)
        then computeMissingRng (x2:xs) (x1 + 1) e (rng ++ show s ++ "-" ++ show (x1 - 1) ++ ",") True
        else computeMissingRng (x2:xs) s e rng True
    | (x1 - s > 1) && (x2 - x1 > 1) = 
    | (x1 - s == 1) && (x2 - x1 == 1)  = if (!isContinuous)
        then computeMissingRng (x2:xs) (x1 + 1) e (rng ++ show s ++ ",") True
        else computeMissingRng (x2:xs) (x1 + 1) e rng
    | (x1 - s == 1) && (x2 - x1 > 1) = 
    | x1 == s = computeMissingRng (x2:xs) (x1 + 1) e rng isContinuous -- do nothing scenario, ignore



main :: IO ()
main = getLine >>= (putStrLn . computeMissingRng . readDigits)