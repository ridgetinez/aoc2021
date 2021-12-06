module Day02 where

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
       delta     <- many digit
       many $ char '\n'
       return (commandId, read delta)

parseCommands :: String -> Either ParseError [(String,Int)]
parseCommands = parse commands "(unknown)"

day02a :: IO ()
day02a = do
    input <- readFile "data/day02.txt"
    putStrLn $ show . parseCommands $ input
