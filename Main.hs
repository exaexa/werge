{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Bool
import Data.Foldable
import Data.Function
import Data.List
import Data.Traversable
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp

import Opts
import Progs
import qualified Toks
import Toks (Tok)

import Debug.Trace

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
    go _ = error "unexpected output from diff"

data Merged
  = Ok [String]
  | Replace [String] [String]
  | Conflict [String] [String] [String]
  deriving (Show)

pmerge :: FilePath -> IO [Merged]
pmerge path = go . lines <$> readFile path
  where
    go [] = []
    go xs@(x:_)
      | Toks.tok x = goOk xs
      | otherwise = goC0 xs
    eat = span Toks.tok
    goOk xs =
      let (a, xs') = eat xs
       in Ok a : go xs'
    goC0 ("<<<<<<<":xs) =
      let (m, xs') = eat xs
       in goC1 m xs'
    goC0 (x:_) = error $ "unexpected token: " ++ x
    goC0 [] = error "unexpected end"
    goC1 m ("|||||||":xs) =
      let (o, xs') = eat xs
       in goC2 m o xs'
    goC1 _ (x:_) = error $ "unexpected token: " ++ x
    goC1 _ [] = error "unexpected end"
    goC2 m o ("=======":xs) =
      let (y, xs') = eat xs
       in goC3 m o y xs'
    goC2 _ _ (x:_) = error $ "unexpected token: " ++ x
    goC2 _ _ [] = error "unexpected end"
    goC3 m o y (">>>>>>>":xs) = Conflict m o y : go xs
    goC3 _ _ _ (x:_) = error $ "unexpected token: " ++ x
    goC3 _ _ _ [] = error "unexpected end"

isKeepTok :: (Op, String) -> Bool
isKeepTok (Keep, _) = True
isKeepTok _ = False

isDelTok :: (Op, String) -> Bool
isDelTok (Del, _) = True
isDelTok _ = False

-- TODO: Diff output is not necessarily deterministic; we could make the chunk
-- sequences more unique by rolling them to front (or back), possibly enabling
-- more conflict resolution and preventing mismerges.
--
-- Example: " a " can be made out of " {+a +}" or "{+ a+} "
chunks :: [(Op, String)] -> [Merged]
chunks [] = []
chunks xs@((Keep, _):_) =
  let (oks, ys) = span isKeepTok xs
   in Ok (map snd oks) : chunks ys
chunks xs =
  let (reps, ys) = break isKeepTok xs
   in uncurry (Replace `on` map snd) (partition isDelTok reps) : chunks ys

align1 :: Eq a => [a] -> [a] -> ([a], [a], [a])
align1 as [] = ([], as, [])
align1 [] bs = ([], [], bs)
align1 (a:as) (b:bs)
  | a == b
  , (xs, as', bs') <- align1 as bs = (a : xs, as', bs')
align1 _ _ = error "chunks do not align"

align :: [Merged] -> [Merged] -> [Merged]
align m0 y0 = connect $ slice m0 y0
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
    coFlag _ = error "flagging unacceptable chunks"
    coSig (a, b) = (coFlag a, coFlag b)
    coConn' (a, b) (a', b') = (a && a') || (b && b')
    coConn = coConn' `on` coSig
    coGroup [] = []
    coGroup (x:xs) =
      case coGroup xs of
        (ys@(y:_):yss)
          | coConn x y -> (x : ys) : yss
        xs' -> [x] : xs'
    connect = map confl . coGroup
    toCon (Ok m, Ok _) = Ok m
    toCon (Ok o, Replace _ y) = Conflict o o y
    toCon (Replace o m, Ok _) = Conflict m o o
    toCon (Replace o m, Replace _ y) = Conflict m o y
    toCon _ = error "converting unacceptable chunks"
    confl = foldr coAppend (Ok []) . map toCon
    coAppend (Ok x) (Ok o) = Ok (x ++ o)
    coAppend (Ok _) (Conflict _ _ _) = error "align consistency check fail" -- Conflict (x++m) (x++o) (x++y)
    coAppend (Conflict m o y) (Ok x) = Conflict (m ++ x) (o ++ x) (y ++ x)
    coAppend (Conflict m o y) (Conflict m' o' y') =
      Conflict (m ++ m') (o ++ o') (y ++ y')
    coAppend _ _ = error "appending unacceptable chunks"

regroup :: [Merged] -> [Merged]
regroup [] = []
regroup (Ok []:xs) = regroup xs
regroup (x@(Ok a):xs) =
  case regroup xs of
    (Ok b:xs') -> Ok (a ++ b) : xs'
    xs' -> x : xs'
regroup (x:xs) = x : regroup xs

zeal :: Config -> Merged -> [Merged]
zeal Config {..} (Conflict m0 o0 y0) =
  before' ++ (Conflict (reverse m'') o0 (reverse y'') : after')
  where
    ((m', y'), before) = pops m0 y0
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
      | cfgSpaceRetain == ResolveMy
      , Toks.space m
      , Toks.space y = (m :) <$> pops ms ys
      | cfgSpaceRetain == ResolveYour
      , Toks.space m
      , Toks.space y = (y :) <$> pops ms ys
    pops ms ys = ((ms, ys), [])
zeal _ x = [x]

resolveSpace :: Config -> Merged -> Merged
resolveSpace Config {..} c@(Conflict m o y)
  | not (all Toks.space $ concat [m, o, y]) = c
  | m == o && o == y = Ok o
  | cfgSpaceRetain == ResolveMy = Ok m
  | cfgSpaceRetain == ResolveOld = Ok o
  | cfgSpaceRetain == ResolveYour = Ok y
  | cfgSpaceResolution == SpaceNormal = c
  | cmResolveSeparate cfgSpaceConflicts && m == o = Ok y
  | cmResolveSeparate cfgSpaceConflicts && o == y = Ok m
  | cmResolveOverlaps cfgSpaceConflicts && m == y = Ok m
  | SpaceSpecial r <- cfgSpaceResolution =
    case r of
      ResolveMy -> Ok m
      ResolveOld -> Ok o
      ResolveYour -> Ok y
      ResolveKeep -> c
resolveSpace _ x = x

expand :: Int -> [Merged] -> [Merged]
expand n = go
  where
    go [] = []
    go (x@(Conflict m1 o1 y1):xs) =
      case go xs of
        (Conflict m2 o2 y2:xs')
          | n > 0 -> Conflict (m1 ++ m2) (o1 ++ o2) (y1 ++ y2) : xs'
        (Ok a:Conflict m2 o2 y2:xs')
          | length a < n ->
            Conflict (m1 ++ a ++ m2) (o1 ++ a ++ o2) (y1 ++ a ++ y2) : xs'
        xs' -> x : xs'
    go (x@(Replace o1 n1):xs) =
      case go xs of
        (Replace o2 n2:xs')
          | n > 0 -> Replace (o1 ++ o2) (n1 ++ n2) : xs'
        (Ok a:Replace o2 n2:xs')
          | length a < n -> Replace (o1 ++ a ++ o2) (n1 ++ a ++ n2) : xs'
        xs' -> x : xs'
    go (x:xs) = x : go xs

resolve :: Config -> Merged -> Merged
resolve cfg@Config {..} c@(Conflict m o y)
  | cfgSpaceResolution /= SpaceNormal
  , all Toks.space (concat [m, o, y]) = resolveSpace cfg c
  | m == o && o == y = Ok o
  | cmResolveSeparate cfgConflicts && m == o = Ok y
  | cmResolveSeparate cfgConflicts && o == y = Ok m
  | cmResolveOverlaps cfgConflicts && m == y = Ok m
  | otherwise =
    case cfgResolution of
      ResolveMy -> Ok m
      ResolveOld -> Ok o
      ResolveYour -> Ok y
      ResolveKeep -> c
resolve _ x = x

merge :: Config -> [(Op, String)] -> [(Op, String)] -> [Merged]
merge cfg@Config {..} ms ys =
  regroup
    . map (resolve cfg)
    . expand cfgContext
    . regroup
    . map (resolveSpace cfg)
    . bool id (concatMap $ zeal cfg) cfgZealous
    . regroup
    $ align (chunks ms) (chunks ys)

diff Config {..} = expand cfgContext . chunks

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
    go _ (Conflict m o y:xs) = do
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
    go _ (Replace o n:xs) = do
      hPutStr h
        $ mconcat
            [cfgLabelStart, Toks.glue o, cfgLabelDiff, Toks.glue n, cfgLabelEnd]
      go True xs

runCmd :: Command -> Config -> IO ()
runCmd CmdDiff3 {..} cfg =
  withSystemTempDirectory "werge-diff3" $ \workdir -> do
    let [fMy, fOld, fYour, fdMy, fdYour] =
          map (workdir </>) ["my", "old", "your", "mydiff", "yourdiff"]
    for_ [(d3my, fMy), (d3old, fOld), (d3your, fYour)] $ \(path, tmp) ->
      bracketFile path ReadMode $ \h -> hSplitToFile cfg h tmp
    runDiff fOld fMy fdMy
    runDiff fOld fYour fdYour
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
        runDiff fOld fMy fdMy
        runDiff fOld fYour fdYour
        readFile u >>= writeFile (u ++ ".werge-backup")
        conflict <-
          bracketFile u WriteMode $ \h ->
            merge cfg <$> pdiff fdMy <*> pdiff fdYour >>= format cfg h
        unless conflict $ when gmDoAdd $ gitAdd u
        pure conflict
  if or conflicts
    then exitWith (ExitFailure 1)
    else exitSuccess
runCmd CmdDiff {..} cfg = do
  withSystemTempDirectory "werge-diff" $ \workdir -> do
    let [fOld, fYour, fDiff] = map (workdir </>) ["old", "your", "diff"]
    for_ [(diffOld, fOld), (diffYour, fYour)] $ \(path, tmp) ->
      bracketFile path ReadMode $ \h -> hSplitToFile cfg h tmp
    conflicted <-
      case diffUnified of
        Just u -> do
          c <- runDiffRaw u fOld fYour fDiff
          readFile fDiff >>= putStr . unlines . drop 2 . lines
          pure c
        Nothing -> do
          runDiff fOld fYour fDiff
          pdiff fDiff >>= format cfg stdout . diff cfg
    if conflicted
      then exitWith (ExitFailure 1)
      else exitSuccess
runCmd CmdPatch {..} cfg = do
  withSystemTempDirectory "werge-patch" $ \workdir -> do
    let f = workdir </> "file"
    bracketFile patchMy ReadMode $ \h -> hSplitToFile cfg h f
    _ <- runPatch f stdin
    conflicted <- pmerge f >>= format cfg stdout -- TODO try to resolve more?
    if conflicted
      then exitWith (ExitFailure 1)
      else exitSuccess
runCmd CmdBreak cfg = hSplit cfg stdin stdout
runCmd CmdGlue _ = getContents >>= putStr . Toks.glue . Toks.fromFile

main :: IO ()
main = catch go bad
  where
    go = parseOpts >>= uncurry (flip runCmd)
    bad e = do
      hPutStrLn stderr $ "fatal: " ++ displayException (e :: IOException)
      exitWith (ExitFailure 2)
