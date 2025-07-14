{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Bool
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Traversable
import Opts
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

import qualified Toks

{-
 - interface to other programs
 -}
diffProg = fromMaybe "diff" <$> lookupEnv "WERGE_DIFF"

gitProg = fromMaybe "git" <$> lookupEnv "WERGE_GIT"

bracketFile path mode = bracket (openFile path mode) hClose

rundiff f1 f2 out = do
  diff <- diffProg
  st <-
    bracketFile out WriteMode $ \oh ->
      withCreateProcess
        (proc
           diff
           [ "--text"
           , "--new-line-format=+%L"
           , "--old-line-format=-%L"
           , "--unchanged-line-format= %L"
           , f1
           , f2
           ])
          {std_in = NoStream, std_out = UseHandle oh} $ \_ _ _ -> waitForProcess
  when (st == ExitFailure 2) $ error "diff failed"
  unless (st `elem` [ExitSuccess, ExitFailure 1])
    $ error "diff failed for unknown reason (is GNU diffutils installed?)"

gitRepoRelRoot = do
  git <- gitProg
  (path, st) <-
    withCreateProcess
      (proc git ["rev-parse", "--show-cdup"])
        {std_in = NoStream, std_out = CreatePipe} $ \_ (Just oh) _ p ->
      (,) <$> hGetContents' oh <*> waitForProcess p
  unless (st == ExitSuccess) $ error "git failed"
  let [p] = lines path
  pure p

gitUnmerged = do
  git <- gitProg
  (paths, st) <-
    withCreateProcess
      (proc git ["status", "--porcelain=v1"])
        {std_in = NoStream, std_out = CreatePipe} $ \_ (Just oh) _ p ->
      (,)
        <$> (map (drop 3) . filter ("UU " `isPrefixOf`) . lines
               <$> hGetContents' oh)
        <*> waitForProcess p
  unless (st == ExitSuccess) $ error "git failed"
  pure paths

gitCheckoutMOY cfg u my old your = do
  git <- gitProg
  (paths, st) <-
    withCreateProcess
      (proc git ["ls-files", "--unmerged", "--", u])
        {std_in = NoStream, std_out = CreatePipe} $ \_ (Just oh) _ p ->
      (,)
        <$> (sortOn snd
               . map ((\[a, b] -> (a, b)) . take 2 . drop 1 . words)
               . lines
               <$> hGetContents' oh)
        <*> waitForProcess p
  unless (st == ExitSuccess) $ error "git failed"
  let co (hash, _) path = do
        st <-
          withCreateProcess
            (proc "git" ["cat-file", "blob", hash])
              {std_in = NoStream, std_out = CreatePipe} $ \_ (Just ho) _ p -> do
            hSplitToFile cfg ho path
            waitForProcess p
        unless (st == ExitSuccess) . error
          $ "failed checking out " ++ u ++ " from blob " ++ hash
  case paths of
    [(_, "1"), (_, "2"), (_, "3")] ->
      zipWithM co paths [old, my, your] >> pure ()
    _ -> error $ "bad data from ls-files for unmerged " ++ u

gitAdd path = do
  git <- gitProg
  st <- rawSystem git ["add", "--", path]
  unless (st == ExitSuccess) $ error "git-add failed"

{-
 - configurable splitting
 -}
hSplitToFile cfg h path =
  case cfgTokenizer cfg of
    TokenizeCharCategory -> internal Toks.splitCategory
    TokenizeCharCategorySimple -> internal Toks.splitSimple
    TokenizeFilter cmd -> do
      st <-
        bracketFile path WriteMode $ \ho ->
          withCreateProcess
            (shell cmd) {std_in = UseHandle h, std_out = UseHandle ho} $ \_ _ _ ->
            waitForProcess
      unless (st == ExitSuccess) $ error "tokenize filter failed"
  where
    internal s = hGetContents h >>= writeFile path . Toks.toFile . s

{-
 - merge algorithms
 -}
data Op
  = Del
  | Keep
  | Add
  deriving (Show, Eq)

pdiff path = map go . lines <$> readFile path
  where
    go ('-':s) = (Del, s)
    go (' ':s) = (Keep, s)
    go ('+':s) = (Add, s)
    go [] = error "unexpected output from diff"

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
--
-- Also, conflicts that are made out of an ignorable space change and a
-- mergeable non-space change now cause conflicts because the spaces are no
-- longer truly separable/alignable here. Ideally some part of the space
-- merging should be done at alignment (e.g., fake all spaces to cause them to
-- align well). Also it might be necessary to group space-tokens together
-- (newline and indent are now 2 space tokens, which won't ever merge with a
-- single space)
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

{-
 - front-end
 -}
format :: Config -> Handle -> [Merged] -> IO Bool
format Config {..} h = go False
  where
    go c [] = pure c
    go c (Ok x:xs) = do
      hPutStr h (Toks.glue x)
      go c xs
    go c (Conflict m o y:xs) = do
      hPutStr h
        $ mconcat
            [ cfgLabelStart
            , Toks.glue m
            , cfgLabelMyOld
            , Toks.glue o
            , cfgLabelOldYour
            , Toks.glue y
            , cfgLabelEnd
            ]
      go True xs

runCmd CmdDiff3 {..} cfg =
  withSystemTempDirectory "werge-diff3" $ \workdir -> do
    let [fMy, fOld, fYour, fdMy, fdYour] =
          map (workdir </>) ["my", "old", "your", "mydiff", "yourdiff"]
    for_ [(d3my, fMy), (d3old, fOld), (d3your, fYour)] $ \(path, tmp) ->
      bracketFile path ReadMode $ \h -> hSplitToFile cfg h tmp
    rundiff fOld fMy fdMy
    rundiff fOld fYour fdYour
    conflicted <-
      merge cfg <$> pdiff fdMy <*> pdiff fdYour >>= format cfg stdout
    if conflicted
      then exitWith (ExitFailure 1)
      else exitSuccess
runCmd CmdGitMerge {..} cfg = do
  relroot <- gitRepoRelRoot
  unmerged <-
    case gmFiles of
      Nothing -> map (relroot </>) <$> gitUnmerged
      Just fs -> pure fs
  conflicts <-
    for unmerged $ \u ->
      withSystemTempDirectory "werge-git" $ \workdir -> do
        let [fMy, fOld, fYour, fdMy, fdYour] =
              map (workdir </>) ["my", "old", "your", "mydiff", "yourdiff"]
        gitCheckoutMOY cfg u fMy fOld fYour
        rundiff fOld fMy fdMy
        rundiff fOld fYour fdYour
        readFile u >>= writeFile (u ++ ".werge-backup")
        conflict <-
          bracketFile u WriteMode $ \h ->
            merge cfg <$> pdiff fdMy <*> pdiff fdYour >>= format cfg h
        unless conflict $ when gmDoAdd $ gitAdd u
        pure conflict
  if or conflicts
    then exitWith (ExitFailure 1)
    else exitSuccess

main :: IO ()
main = catch go bad
  where
    go = do
      (cfg, cmd) <- parseOpts
      runCmd cmd cfg
    bad e = do
      hPutStrLn stderr $ "fatal: " ++ displayException (e :: IOException)
      exitWith (ExitFailure 2)
