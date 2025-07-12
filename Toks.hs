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
unmarkSpace _ = error "wat"

split =
  unlines
    . map (concatMap escape . markSpace)
    . groupBy ((==) `on` generalCategory)

glueToks = concatMap (unmarkSpace . unescape)

glue = glueToks . lines
