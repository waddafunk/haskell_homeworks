{-# OPTIONS_GHC -Wall #-}
module Lib
    ( someFunc
    ) where
import Log
import Text.Read
import Data.Maybe

--Checks if first char of a string is a space
checkSpace :: String -> Bool
checkSpace [] = False
checkSpace s
  | head s  == ' ' = True
  | otherwise = False

--Returns a pair from two strings, just a test
pairStrings :: String -> String -> (String, String)
pairStrings a b = (a, b)

--Returns a pair of string where the first string is the chars until space (included),
--the second is the remaining chars
parseTilNewSpace :: String -> (String, String)
parseTilNewSpace []= ([], [])
parseTilNewSpace (h:t)
  | t == [] = pairStrings [h] []
  | checkSpace [h] = ([' '], t)
  | otherwise = (h:fst(parseTilNewSpace t), snd(parseTilNewSpace(t)))

--Checks if a string is an integer
checkInteger :: String -> Bool
checkInteger s
  | (readMaybe s :: Maybe Int) == (readMaybe "lololol" :: Maybe Int) = False
  | otherwise = True

--Returns Int from Strings of integers, 0 otherwise
stringToInt :: String -> Int
stringToInt s
  | checkInteger s = fromJust(readMaybe s :: Maybe Int)
  | otherwise = 0
  
--Builds Error LogMessage
isError :: String -> LogMessage
isError s
  | checkInteger(fst(parseTilNewSpace(s))) && checkInteger(fst(parseTilNewSpace(snd(parseTilNewSpace(s))))) 
    = LogMessage(Error(stringToInt(fst(parseTilNewSpace(s)))))
    (stringToInt(fst(parseTilNewSpace(snd(parseTilNewSpace(s))))))
    (snd(parseTilNewSpace(snd(parseTilNewSpace(s)))))
  | otherwise = Unknown("E" ++ s)

--Returns Info or Warning
buildMessage :: Char -> MessageType
buildMessage c
  | c:[] == "I" = Info
  | otherwise = Warning

--Builds Warning or Info LogMessages
notError :: Char -> String -> LogMessage
notError c s
  | checkInteger(fst(parseTilNewSpace(s))) == True =
    LogMessage (buildMessage(c)) (stringToInt(fst(parseTilNewSpace(s)))) (snd(parseTilNewSpace(s)))
  | otherwise = Unknown(c:s)
    
--Parse a message and builds a LogMessage
parseMessage  :: String -> LogMessage
parseMessage [] = Unknown("Empty input")
parseMessage (h:t)
  | h == 'E' && checkSpace(head(t):[]) = isError (tail t) 
  | h == 'I' && checkSpace(head(t):[]) = notError h (tail t)
  | h == 'W' && checkSpace(head(t):[]) = notError h (tail t)
  | otherwise = Unknown(h:t)

--Get the LogMessage string
getString :: LogMessage -> String
getString (LogMessage _ _ n) = n
getString (Unknown n) = n

someFunc :: IO ()
someFunc = putStrLn(getString(parseMessage("E 42 69 hello world")))
