module Day01 where

getDepths :: IO [Int]
getDepths = readFile "data/day01.txt" >>= return . map read . lines

countGreaterThanPrevious :: [Int] -> Int
countGreaterThanPrevious xs = length $ filter (\(a,b) -> a < b) $ zip xs (tail xs) 

day01a :: IO ()
day01a = getDepths >>= putStrLn . show . countGreaterThanPrevious

day01b :: IO ()
day01b = getDepths >>= putStrLn . show . countGreaterThanPrevious . map sumThreeTuple . windowTransform
    where windowTransform xs = zip3 xs (tail xs) (tail . tail $ xs)
          sumThreeTuple (a,b,c) = a + b + c