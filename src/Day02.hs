module Day02 where

import Data.Either
import Data.Bifunctor
import Text.ParserCombinators.Parsec

{-
Parsing
Already kind of familiar with why applicative, but now how applicative.
This link changed that for me: https://www.fpcomplete.com/haskell/tutorial/applicative-syntax/
-}
commands :: GenParser Char st [(String,Int)]
commands = many command <* eof

command :: GenParser Char st (String,Int)
command = (,)
    <$> many letter <* char ' '
    <*> (read <$> many digit) <* many (char '\n')

parseCommands :: String -> Either ParseError [(String,Int)]
parseCommands = parse commands "(unknown)"

runCommandsB :: [(String,Int)] -> (Int,Int)
runCommandsB =  snd . foldl interpretCommand startingConditions
    where interpretCommand (aim,(x,y)) ("forward",n) = (aim,(x+n,y+aim*n))
          interpretCommand (aim,p) ("up",n)      = (aim-n,p)
          interpretCommand (aim,p) ("down",n)    = (aim+n,p)
          startingConditions = (0,(0,0))

runCommandsA :: [(String,Int)] -> (Int,Int)
runCommandsA =  bimap sum sum . unzip . map interpretCommand
    where interpretCommand ("forward",n) = (n,0)
          interpretCommand ("up",n)      = (0,-n)
          interpretCommand ("down",n)    = (0,n)

day02a :: IO ()
day02a = do
    input <- readFile "data/day02.txt"
    putStrLn $ show . runCommandsA . fromRight [] . parseCommands $ input


day02b :: IO ()
day02b = do
    input <- readFile "data/day02.txt"
    putStrLn $ show . runCommandsB . fromRight [] . parseCommands $ input

