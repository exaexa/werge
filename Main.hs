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
  when (st == ExitFailure 2) $ error "diff failed"
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
align _ _ = error "diffs do not align"

regroup :: [Merged] -> [Merged]
regroup [] = []
regroup (Ok []:xs) = regroup xs
regroup (x@(Ok a):xs) =
  case regroup xs of
    (Ok b:xs') -> Ok (a ++ b) : xs'
    xs' -> x : xs'
regroup (Conflict [] [] []:xs) = regroup xs
regroup (x@(Conflict m1 o1 y1):xs) =
  case regroup xs of
    (Conflict m2 o2 y2:xs') -> Conflict (m1 ++ m2) (o1 ++ o2) (y1 ++ y2) : xs'
    xs' -> x : xs'
regroup (x:xs) = x : regroup xs

expand :: Int -> [Merged] -> [Merged]
expand n = go
  where
    go [] = []
    go (x@(Conflict m1 o1 y1):xs) =
      case go xs of
        (Ok a:Conflict m2 o2 y2:xs')
          | length a <= n ->
            Conflict (m1 ++ a ++ m2) (o1 ++ a ++ o2) (y1 ++ a ++ y2) : xs'
        xs' -> x : xs'
    go (x:xs) = x : go xs

zeal (Conflict m o y) =
  before' ++ (Conflict (reverse m'') o (reverse y'') : after')
  where
    ((m', y'), before) = pops m y
    ((m'', y''), rafter) = pops (reverse m') (reverse y')
    before' =
      case before of
        [] -> []
        xs -> [Ok xs]
    after' =
      case rafter of
        [] -> []
        xs -> [Ok $ reverse xs]
    pops (m:ms) (y:ys)
      | m == y = (m :) <$> pops ms ys
    pops ms ys = ((ms, ys), [])
zeal x = [x]

resolve cfg@Config {..} c@(Conflict m o y)
  | all Toks.space (concat [m, o, y]) && cfgSpaces /= SpacesNormal =
    resolveSpace cfg c
  | m == o && o == y = Ok o
  | m == o && cfgResolveSeparate = Ok y
  | o == y && cfgResolveSeparate = Ok m
  | m == y && cfgResolveOverlaps = Ok m
resolve _ x = x

-- TODO: there might be a bit of interplay between the spaces handling and
-- separate/overlapped conflict resolution -- e.g., what if someone wants to
-- merge overlapping edits in text but separate edits in spaces? At this point
-- that might be ignorable.
resolveSpace Config {..} c@(Conflict m o y)
  | m == o && o == y = Ok o
  | otherwise =
    case cfgSpaces of
      SpacesConflict -> c
      SpacesMy -> Ok m
      SpacesOld -> Ok o
      SpacesYour -> Ok y
      _ -> error "spaces resolution failed"

resolveSpaces _ x = x

merge cfg@Config {..} ms ys =
  regroup
    . map (resolve cfg)
    . regroup
    . bool id (concatMap zeal) cfgZealous
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
      readFile path >>= writeFile tmp . Toks.split -- TODO cfg
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
