module Day02 where

import Data.Either
import Data.Bifunctor
import Text.ParserCombinators.Parsec

commands :: GenParser Char st [(String,Int)]
commands =
    do result <- many command
       eof
       return result

command :: GenParser Char st (String,Int)
command =
    do commandId <- many letter
       char ' '
       delta <- many digit
       many $ char '\n'
       return (commandId, read delta)

parseCommands :: String -> Either ParseError [(String,Int)]
parseCommands = parse commands "(unknown)"

runCommandsB :: [(String,Int)] -> (Int,Int)
runCommandsB =  snd . foldl interpretCommand startingConditions
    where interpretCommand (aim,(x,y)) ("forward",n) = (aim,(x+n,y+aim*n))
          interpretCommand (aim,(x,y)) ("up",n)      = (aim-n,(x,y))
          interpretCommand (aim,(x,y)) ("down",n)    = (aim+n,(x,y))
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
