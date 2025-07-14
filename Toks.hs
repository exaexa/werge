module Toks where

import Data.Char
import Data.Function
import Data.List

escape '\\' = "\\\\"
escape '\n' = "\\n"
escape x = pure x

unescape [] = []
unescape ('\\':'\\':xs) = '\\' : unescape xs
unescape ('\\':'n':xs) = '\n' : unescape xs
unescape ('\\':_) = error "bad escape?"
unescape (x:xs) = x : unescape xs

markSpace [] = error "wat"
markSpace s@(c:_)
  | isSpace c = '.' : s
  | otherwise = '|' : s

unmarkSpace ('.':s) = s
unmarkSpace ('|':s) = s
unmarkSpace x = error "unwat"

space ('.':_) = True
space _ = False

joinSpaces [] = []
joinSpaces (a@('.':as):xs) =
  case joinSpaces xs of
    (('.':bs):xs') -> ('.' : (as ++ bs)) : xs'
    xs' -> a : xs'
joinSpaces (x:xs) = x : joinSpaces xs

splitCategory = make . groupBy ((==) `on` generalCategory)

simpleCategory c
  | isSpace c = 0
  | isAlpha c = 1
  | isNumber c = 2
  | otherwise = 3

splitSimple = make . groupBy ((==) `on` simpleCategory)

make = map (concatMap escape . markSpace)

glue :: [String] -> String
glue = concatMap (unmarkSpace . unescape)

fromFile = lines

toFile = unlines
