module Progs where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import System.Process

import Opts
import qualified Toks

bracketFile :: FilePath -> IOMode -> (Handle -> IO c) -> IO c
bracketFile path mode = bracket (openFile path mode) hClose

{-
 - interface to gnu diff
 -}
diffProg :: IO String
diffProg = fromMaybe "diff" <$> lookupEnv "WERGE_DIFF"

patchProg :: IO String
patchProg = fromMaybe "patch" <$> lookupEnv "WERGE_PATCH"

runDiff :: FilePath -> FilePath -> FilePath -> IO ()
runDiff f1 f2 out = do
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

runDiffRaw :: Int -> FilePath -> FilePath -> FilePath -> IO Bool
runDiffRaw u f1 f2 out = do
  diff <- diffProg
  st <-
    bracketFile out WriteMode $ \oh ->
      withCreateProcess
        (proc diff ["--text", "--unified=" ++ show u, f1, f2])
          {std_in = NoStream, std_out = UseHandle oh} $ \_ _ _ -> waitForProcess
  unless (st `elem` [ExitSuccess, ExitFailure 1]) $ error "diff failed"
  pure (st /= ExitSuccess) -- report if diff thinks that the files differed

runPatch :: FilePath -> Handle -> IO Bool
runPatch f hi = do
  patch <- patchProg
  st <-
    withCreateProcess
      (proc patch ["--silent", "--batch", "--merge=diff3", f])
        {std_in = UseHandle hi} $ \_ _ _ -> waitForProcess
  unless (st `elem` [ExitSuccess, ExitFailure 1]) $ error "patch failed"
  pure (st /= ExitSuccess) -- report if patch thinks that stuff has failed

{-
 - interface to git
 -}
gitProg :: IO String
gitProg = fromMaybe "git" <$> lookupEnv "WERGE_GIT"

gitRepoRelRoot :: IO FilePath
gitRepoRelRoot = do
  git <- gitProg
  (path, st) <-
    withCreateProcess
      (proc git ["rev-parse", "--show-cdup"])
        {std_in = NoStream, std_out = CreatePipe} $ \_ (Just oh) _ p ->
      (,) <$> hGetContents' oh <*> waitForProcess p
  unless (st == ExitSuccess) $ error "git failed"
  case lines path of
    [p] -> pure p
    _ -> fail "bad git-rev-parse output"

gitUnmerged :: IO [FilePath]
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

gitCheckoutMOY ::
     Config -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
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
        st' <-
          withCreateProcess
            (proc "git" ["cat-file", "blob", hash])
              {std_in = NoStream, std_out = CreatePipe} $ \_ (Just ho) _ p -> do
            hSplitToFile cfg ho path
            waitForProcess p
        unless (st' == ExitSuccess) . error
          $ "failed checking out " ++ u ++ " from blob " ++ hash
  case paths of
    [(_, "1"), (_, "2"), (_, "3")] ->
      zipWithM co paths [old, my, your] >> pure ()
    _ -> error $ "bad data from ls-files for unmerged " ++ u

gitAdd :: FilePath -> IO ()
gitAdd path = do
  git <- gitProg
  st <- rawSystem git ["add", "--", path]
  unless (st == ExitSuccess) $ error "git-add failed"

{-
 - interface to external tokenizers
 -
 - TODO this might probably enforce joinSpaces?
 - or have joinSpaces as configurable? (probably best, default true)
 -}
hSplit :: Config -> Handle -> Handle -> IO ()
hSplit cfg hi ho =
  case cfgTokenizer cfg of
    TokenizeCharCategory -> internal Toks.splitCategory
    TokenizeCharCategorySimple -> internal Toks.splitSimple
    TokenizeFilter fltr -> do
      st <-
        withCreateProcess
          (shell fltr) {std_in = UseHandle ho, std_out = UseHandle ho} $ \_ _ _ ->
          waitForProcess
      unless (st == ExitSuccess) $ error "tokenize filter failed"
  where
    internal s = hGetContents hi >>= hPutStr ho . Toks.toFile . s

hSplitToFile :: Config -> Handle -> FilePath -> IO ()
hSplitToFile cfg hi path = bracketFile path WriteMode $ hSplit cfg hi
