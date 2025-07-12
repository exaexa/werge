{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Bool
import Data.Foldable
import Opts
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import qualified Toks

import Debug.Trace

-- TODO: the diff w
rundiff f1 f2 out = do
  st <-
    withFile out WriteMode $ \oh ->
      withCreateProcess
        (proc
           "diff" -- TODO: from WERGE_DIFF env
           [ "--text"
           , "--new-line-format=+%L"
           , "--old-line-format=-%L"
           , "--unchanged-line-format= %L"
           , f1
           , f2
           ])
          {std_in = NoStream, std_out = UseHandle oh, std_err = Inherit} $ \_ _ _ ->
        waitForProcess
  when (st == ExitFailure 2) $ error "fatal: diff failed"
  unless (st `elem` [ExitSuccess, ExitFailure 1])
    $ error "diff failed for unknown reason (is GNU diffutils installed?)"

data Op
  = Del
  | Keep
  | Add
  deriving (Show, Eq)

pdiff path = map go . lines <$> readFile path
  where
    go [] = error "empty line from diff"
    go ('-':s) = (Del, s)
    go (' ':s) = (Keep, s)
    go ('+':s) = (Add, s)

data Merged
  = Ok [String]
  | Conflict [String] [String] [String]
  deriving (Show)

align :: [(Op, String)] -> [(Op, String)] -> [Merged]
align [] [] = []
align ((Keep, m):ms) ((Keep, y):ys)
  | m == y = Ok [m] : align ms ys
align ((Del, m):ms) ((Del, y):ys)
  | m == y = Conflict [] [m] [] : align ms ys
align ((Del, m):ms) ((Keep, y):ys)
  | m == y = Conflict [] [m] [m] : align ms ys
align ((Keep, m):ms) ((Del, y):ys)
  | m == y = Conflict [m] [m] [] : align ms ys
align ((Add, m):ms) ys = Conflict [m] [] [] : align ms ys
align ms ((Add, y):ys) = Conflict [] [] [y] : align ms ys
align _ _ = error "fatal: diffs do not align"

-- TODO this is quadratic, call regroup first and case it
regroup :: [Merged] -> [Merged]
regroup [] = []
regroup (Ok []:xs) = regroup xs
regroup (Ok a:Ok b:xs) = regroup (Ok (a ++ b) : xs)
regroup (Conflict [] [] []:xs) = regroup xs
regroup (Conflict m1 o1 y1:Conflict m2 o2 y2:xs) =
  regroup (Conflict (m1 ++ m2) (o1 ++ o2) (y1 ++ y2) : xs)
regroup (x:xs) = x : regroup xs

expand :: Int -> [Merged] -> [Merged]
expand n = go
  where
    go [] = []
    go (Conflict m1 o1 y1:Ok a:Conflict m2 o2 y2:xs)
      | length a <= n =
        go $ Conflict (m1 ++ a ++ m2) (o1 ++ a ++ o2) (y1 ++ a ++ y2) : xs
    go (x:xs) = x : go xs

zeal = id -- TODO

resolve _ c@(Conflict m o y)
  | m == o && o == y = Ok o
  | m == o = Ok y
  | o == y = Ok m
  | m == y = Ok m
resolve cfg x = x

merge cfg@Config {..} ms ys =
  regroup
    . map (resolve cfg)
    . bool id zeal cfgZealous
    . expand cfgContext
    . regroup
    $ align ms ys

format :: Config -> [Merged] -> IO Bool
format Config {..} = go False
  where
    go c [] = pure c
    go c (Ok x:xs) = do
      putStr (Toks.glueToks x)
      go c xs
    go c (Conflict m o y:xs) = do
      putStr
        $ mconcat
            [ cfgLabelStart
            , Toks.glueToks m
            , cfgLabelMyOld
            , Toks.glueToks o
            , cfgLabelOldYour
            , Toks.glueToks y
            , cfgLabelEnd
            ]
      go True xs

runCmd CmdDiff3 {..} cfg =
  withSystemTempDirectory "werge-diff3" $ \workdir -> do
    let [fMy, fOld, fYour, fdMy, fdYour] =
          map (workdir </>) ["my", "old", "your", "mydiff", "yourdiff"]
    for_ [(d3my, fMy), (d3old, fOld), (d3your, fYour)] $ \(path, tmp) ->
      readFile path >>= writeFile tmp . Toks.split
    rundiff fOld fMy fdMy
    rundiff fOld fYour fdYour
    conflicted <- merge cfg <$> pdiff fdMy <*> pdiff fdYour >>= format cfg
    if conflicted
      then exitWith (ExitFailure 1)
      else exitSuccess
runCmd _ _ = error "not implemented yet"

main :: IO ()
main = catch go bad
  where
    go = do
      (cfg, cmd) <- parseOpts
      runCmd cmd cfg
    bad e = do
      hPutStrLn stderr $ "fatal: " ++ displayException (e :: IOException)
      exitWith (ExitFailure 2)
