module Day03 where

{-
1: Transpose and countOnes
2: Keep running ones and zeroes arrays and output the max for each index.
-}

keepCount :: Char -> [Int] -> String -> [Int]
keepCount c counts num = map go $ zip counts num
    where go (count,digit)
            | digit == c = count + 1
            | otherwise  = count 