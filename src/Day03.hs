module Day03 where

{-
1: Transpose and countOnes
2: Keep running ones and zeroes arrays and output the max for each index.
-}

keepCount :: Char -> String -> [Int] -> [Int]
keepCount c num counts = map go $ zip counts num
    where go (count,digit)
            | digit == c = count + 1
            | otherwise  = count

gamma :: [String] -> String
gamma nums = map setBit oneCounts
    where bitNumLen = length $ nums !! 0
          oneCounts = foldr (keepCount '1') (replicate bitNumLen 0) nums
          setBit x
            | x > (bitNumLen `div` 2) = '1'
            | otherwise = '0'

epsilon :: String -> String
epsilon = map flipBit
    where flipBit '1' = '0'
          flipBit '0' = '1'

day03a :: IO ()
day03a = readFile "data/day03.txt" >>= return . lines >>= putStrLn . show
