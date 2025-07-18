module Toks where

import Data.Char
import Data.Function
import Data.List

type Tok = String

escape :: Char -> String
escape '\\' = "\\\\"
escape '\n' = "\\n"
escape x = pure x

unescape :: String -> String
unescape [] = []
unescape ('\\':'\\':xs) = '\\' : unescape xs
unescape ('\\':'n':xs) = '\n' : unescape xs
unescape ('\\':_) = error "bad escape on input"
unescape (x:xs) = x : unescape xs

tok ('.':_) = True
tok ('/':_) = True
tok _ = False

markSpace :: String -> Tok
markSpace [] = error "empty token"
markSpace s@(c:_)
  | isSpace c = '.' : s
  | otherwise = '/' : s

unmarkSpace :: Tok -> String
unmarkSpace ('.':s) = s
unmarkSpace ('/':s) = s
unmarkSpace _ = error "bad space marking on input"

space :: Tok -> Bool
space ('.':_) = True
space _ = False

joinSpaces :: [Tok] -> [Tok]
joinSpaces [] = []
joinSpaces (a@('.':as):xs) =
  case joinSpaces xs of
    (('.':bs):xs') -> ('.' : (as ++ bs)) : xs'
    xs' -> a : xs'
joinSpaces (x:xs) = x : joinSpaces xs

splitCategory :: String -> [Tok]
splitCategory = make . groupBy ((==) `on` generalCategory)

simpleCategory :: Char -> Int
simpleCategory c
  | isSpace c = 0
  | isAlpha c = 1
  | isNumber c = 2
  | otherwise = 3

splitSimple :: String -> [Tok]
splitSimple = make . groupBy ((==) `on` simpleCategory)

make :: [String] -> [Tok]
make = map (concatMap escape . markSpace)

glue :: [String] -> String
glue = concatMap (unmarkSpace . unescape)

fromFile :: String -> [Tok]
fromFile = lines

toFile :: [Tok] -> String
toFile = unlines
