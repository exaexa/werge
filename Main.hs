{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Bool
import Data.Foldable
import Data.Function
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
import Toks (Tok)

import Debug.Trace

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
 -
 - TODO this should probably enforce joinSpaces?
 - or have joinSpaces as configurable? (probably best, default true)
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

pdiff :: FilePath -> IO [(Op, Tok)]
pdiff path = map go . lines <$> readFile path
  where
    go ('-':s) = (Del, s)
    go (' ':s) = (Keep, s)
    go ('+':s) = (Add, s)
    go [] = error "unexpected output from diff"

data Merged
  = Ok [String]
  | Replace [String] [String]
  | Conflict [String] [String] [String]
  deriving (Show)

isKeepTok (Keep, _) = True
isKeepTok _ = False

isDelTok (Del, _) = True
isDelTok _ = False

chunks :: [(Op, String)] -> [Merged]
chunks [] = []
chunks xs@((Keep, _):_) =
  let (oks, ys) = span isKeepTok xs
   in Ok (map snd oks) : chunks ys
chunks xs =
  let (reps, ys) = break isKeepTok xs
   in uncurry (Replace `on` map snd) (partition isDelTok reps) : chunks ys

align1 as [] = ([], as, [])
align1 [] bs = ([], [], bs)
align1 (a:as) (b:bs)
  | a == b
  , (xs, as', bs') <- align1 as bs = (a : xs, as', bs')
align1 _ _ = error "chunks do not align"

align :: [Merged] -> [Merged] -> [Merged]
align m y = connect $ slice m y
  where
    erase x = Replace x []
    nemap _ [] = []
    nemap f xs = [f xs]
    slice (Ok m:ms) (Ok y:ys) =
      let (ok, m', y') = align1 m y
       in (Ok ok, Ok ok) : slice (nemap Ok m' ++ ms) (nemap Ok y' ++ ys)
    slice (Replace m mr:ms) (Ok y:ys) =
      let (ok, m', y') = align1 m y
       in (Replace ok mr, Ok ok)
            : slice (nemap erase m' ++ ms) (nemap Ok y' ++ ys)
    slice (Ok m:ms) (Replace y yr:ys) =
      let (ok, m', y') = align1 m y
       in (Ok ok, Replace ok yr)
            : slice (nemap Ok m' ++ ms) (nemap erase y' ++ ys)
    slice (Replace m mr:ms) (Replace y yr:ys) =
      let (ok, m', y') = align1 m y
       in (Replace ok mr, Replace ok yr)
            : slice (nemap erase m' ++ ms) (nemap erase y' ++ ys)
    slice [Replace [] mr] [] = [(Replace [] mr, Ok [])]
    slice [] [Replace [] yr] = [(Ok [], Replace [] yr)]
    slice [] [] = []
    slice _ _ = error "unacceptable chunks"
    coFlag (Ok _) = False
    coFlag (Replace _ _) = True
    coSig (a, b) = (coFlag a, coFlag b)
    coConn' (a, b) (a', b') = (a && a') || (b && b')
    coConn = coConn' `on` coSig
    coGroup [] = []
    coGroup (x:xs) =
      case coGroup xs of
        xs'@(ys@(y:_):yss)
          | coConn x y -> (x : ys) : yss
        xs' -> [x] : xs'
    connect = map confl . coGroup
    toCon (Ok m, Ok _) = Ok m
    toCon (Ok o, Replace _ y) = Conflict o o y
    toCon (Replace o m, Ok _) = Conflict m o o
    toCon (Replace o m, Replace _ y) = Conflict m o y
    confl = foldr cappend (Ok []) . map toCon
    cappend (Ok x) (Ok o) = Ok (x ++ o)
    cappend (Ok x) (Conflict m o y) = error "align consistency check fail" -- Conflict (x++m) (x++o) (x++y)
    cappend (Conflict m o y) (Ok x) = Conflict (m ++ x) (o ++ x) (y ++ x)
    cappend (Conflict m o y) (Conflict m' o' y') =
      Conflict (m ++ m') (o ++ o') (y ++ y')

regroup :: [Merged] -> [Merged]
regroup [] = []
regroup (Ok []:xs) = regroup xs
regroup (x@(Ok a):xs) =
  case regroup xs of
    (Ok b:xs') -> Ok (a ++ b) : xs'
    xs' -> x : xs'
regroup (x:xs) = x : regroup xs

zeal Config {..} (Conflict m o y) =
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
      | SpacesMy <- cfgSpaces
      , Toks.space m
      , Toks.space y = (m :) <$> pops ms ys
      | SpacesYour <- cfgSpaces
      , Toks.space m
      , Toks.space y = (y :) <$> pops ms ys
    pops ms ys = ((ms, ys), [])
zeal _ x = [x]

resolveSpace Config {..} c@(Conflict m o y)
  | not (all Toks.space $ concat [m, o, y])
      || cfgSpaces `elem` [SpacesNormal, SpacesConflict] = c
  | m == o && o == y = Ok o
  | otherwise =
    case cfgSpaces of
      SpacesMy -> Ok m
      SpacesOld -> Ok o
      SpacesYour -> Ok y
      _ -> error $ "spaces resolution error " ++ show cfgSpaces
resolveSpace _ x = x

expand :: Int -> [Merged] -> [Merged]
expand n = go
  where
    go [] = []
    go (x@(Conflict m1 o1 y1):xs) =
      case go xs of
        (Conflict m2 o2 y2:xs') ->
          Conflict (m1 ++ m2) (o1 ++ o2) (y1 ++ y2) : xs'
        (Ok a:Conflict m2 o2 y2:xs')
          | length a <= n ->
            Conflict (m1 ++ a ++ m2) (o1 ++ a ++ o2) (y1 ++ a ++ y2) : xs'
        xs' -> x : xs'
    go (x:xs) = x : go xs

resolve cfg@Config {..} c@(Conflict m o y)
  | cfgSpaces /= SpacesNormal && all Toks.space (concat [m, o, y]) =
    resolveSpace cfg c
  | m == o && o == y = Ok o
  | m == o && cfgResolveSeparate = Ok y
  | o == y && cfgResolveSeparate = Ok m
  | m == y && cfgResolveOverlaps = Ok m
resolve _ x = x

merge cfg@Config {..} ms ys =
  regroup
    . map (resolve cfg)
    . expand cfgContext
    . regroup
    . map (resolveSpace cfg)
    . bool id (concatMap $ zeal cfg) cfgZealous
    . regroup
    $ align (chunks ms) (chunks ys)

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
