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

--Checks if string is LogMessage identifier
isIdentifier :: String -> Bool
isIdentifier s = s `elem` ["E","W","I"]  

--Returns Log arguments as a string from a list of words without checking
splitSingleLog :: [String] -> (String, [String])
splitSingleLog [] = ([], [])
splitSingleLog (h:t)
  | isIdentifier h = ([], h:t)
  | otherwise = ((h ++ " " ++  fst(splitSingleLog(t))), snd(splitSingleLog(t)))

--Returns Log arguments as a string from a list of words checking if the first is a LogMessage identifier
getSingleLog :: String -> (String, [String])
getSingleLog [] = ([], [])
getSingleLog s
  | isIdentifier(head(words(s))) = (
        (head(words(s)) ++ " " ++ fst(splitSingleLog(tail(words(s))))),
        (snd(splitSingleLog(tail(words(s)))))
      )
  | otherwise = ([], [])

--Parse a LogMessage string
parse :: String -> [LogMessage]
parse [] = []
parse s = (parseMessage(fst(getSingleLog(s)))):(parse(unwords(snd(getSingleLog(s)))))

--Get the LogMessage string
getString :: LogMessage -> String
getString (LogMessage _ _ n) = n
getString (Unknown n) = n

someFunc :: IO ()
someFunc = putStrLn(getString(head(parse("I 6 Completed armadillo processing I 1 Nothing to report E 99 10 Flange failed! I 4 Everything normal I 11 Initiating self-destruct sequence E 70 3 Way too many pickles E 65 8 Bad pickle-flange interaction detected W 5 Flange is due for a check-up I 7 Out for lunch, back in two time steps E 20 2 Too many pickles I 9 Back from lunch"))))
