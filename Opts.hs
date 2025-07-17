{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Opts where

import Data.Bool
import Data.List
import Data.Maybe
import Data.Version (showVersion)
import Options.Applicative
import Paths_werge (version)

data Tokenizer
  = TokenizeFilter String
  | TokenizeCharCategory
  | TokenizeCharCategorySimple
  deriving (Show)

tokenizer =
  asum
    [ TokenizeFilter
        <$> strOption
              (long "tok-filter"
                 <> short 'F'
                 <> metavar "FILTER"
                 <> help "External program to separate the text to tokens")
    , flag'
        TokenizeCharCategorySimple
        (long "simple-tokens"
           <> short 'i'
           <> help
                "Use wider character class to separate the tokens (results in larger tokens and ignores case)")
    , flag'
        TokenizeCharCategory
        (long "full-tokens"
           <> short 'I'
           <> help
                "Separate characters by all known character classes (default)")
    , pure TokenizeCharCategory
    ]

data ConflictMask = ConflictMask
  { cmResolveOverlaps :: Bool
  , cmResolveSeparate :: Bool
  } deriving (Show)

conflictMask label objs = do
  cmResolveOverlaps' <-
    fmap not . switch
      $ long (label ++ "-overlaps")
          <> help ("Never resolve overlapping changes in " ++ objs)
  cmResolveSeparate' <-
    fmap not . switch
      $ long (label ++ "-separate")
          <> help
               ("Never resolve separate (non-overlapping) changes in " ++ objs)
  cmAll <-
    fmap not . switch
      $ long (label ++ "-all") <> help ("Never resolve any changes in " ++ objs)
  pure
    ConflictMask
      { cmResolveSeparate = cmResolveSeparate' && cmAll
      , cmResolveOverlaps = cmResolveOverlaps' && cmAll
      }

data Resolution
  = ResolveKeep
  | ResolveMy
  | ResolveOld
  | ResolveYour
  deriving (Show, Eq)

resolutionMode x
  | x `isPrefixOf` "keep" = Right ResolveKeep
  | x `isPrefixOf` "my" = Right ResolveMy
  | x `isPrefixOf` "old" = Right ResolveOld
  | x `isPrefixOf` "your" = Right ResolveYour
  | otherwise =
    Left
      $ "Could not parse value `"
          ++ x
          ++ "', use one of `keep', `my', `old', and `your'"

data SpaceResolution
  = SpaceNormal
  | SpaceSpecial Resolution
  deriving (Show, Eq)

spaceMode x
  | x `isPrefixOf` "normal" = Right SpaceNormal
  | Right y <- resolutionMode x = Right (SpaceSpecial y)
  | otherwise =
    Left
      $ "Could not parse value `"
          ++ x
          ++ "', use one of `normal', `keep', `my', `old', and `your'"

data Config = Config
  { cfgTokenizer :: Tokenizer
  , cfgZealous :: Bool
  , cfgSpaceRetain :: Resolution
  , cfgSpaceResolution :: SpaceResolution
  , cfgSpaceConflicts :: ConflictMask
  , cfgContext :: Int
  , cfgResolution :: Resolution
  , cfgConflicts :: ConflictMask
  , cfgLabelStart :: String
  , cfgLabelMyOld :: String
  , cfgLabelOldYour :: String
  , cfgLabelEnd :: String
  } deriving (Show)

config = do
  cfgTokenizer <- tokenizer
  cfgZealous <-
    asum
      [ flag' False $ long "no-zeal" <> help "avoid zealous mode (default)"
      , flag' True
          $ long "zeal"
              <> short 'z'
              <> help
                   "Try to zealously minify conflicts, potentially resolving them"
      , pure False
      ]
  cfgSpaceRetain <-
    option (eitherReader resolutionMode)
      $ long "space"
          <> short 'S'
          <> metavar "(keep|my|old|your)"
          <> help
               "Retain spacing from a selected version, or keep all space changes for merging (default: keep)"
          <> value ResolveKeep
  cfgSpaceResolution <-
    asum
      [ flag' (SpaceSpecial ResolveKeep)
          $ short 's'
              <> help
                   "Shortcut for `--resolve-space keep' (this separates space-only conflicts, enabling better automated resolution)"
      , option (eitherReader spaceMode)
          $ long "resolve-space"
              <> metavar ("(normal|keep|my|old|your)")
              <> value SpaceNormal
              <> help
                   "Resolve conflicts in space-only tokens separately, and either keep unresolved conflicts, or resolve in favor of a given version; `normal' resolves the spaces together with other tokens, ignoring choices in --resolve-space-* (default: normal)"
      ]
  cfgSpaceConflicts <- conflictMask "conflict-space" "space-only tokens"
  cfgContext <-
    option auto
      $ long "expand-context"
          <> short 'C'
          <> metavar "N"
          <> value 1
          <> showDefault
          <> help
               "Consider changes that are at most N tokens apart to be a single change. Zero may cause bad resolutions of near conflicting edits"
  cfgResolution <-
    option (eitherReader resolutionMode)
      $ long "resolve"
          <> metavar "(keep|my|old|your)"
          <> value ResolveKeep
          <> help
               "Resolve general conflicts in favor of a given version, or keep the conflicts (default: keep)"
  cfgConflicts <- conflictMask "conflict" "general tokens"
  color <-
    flag False True
      $ long "color"
          <> short 'G'
          <> help
               "Use shorter, gaily colored output markers by default (requires ANSI color support; good for terminals or `less -R')"
  labelStart <-
    optional . strOption
      $ long "label-start"
          <> metavar "\"<<<<<\""
          <> help "Label for beginning of the conflict"
  labelMyOld <-
    optional . strOption
      $ long "label-mo"
          <> metavar "\"|||||\""
          <> help "Separator of local edits and original"
  labelOldYour <-
    optional . strOption
      $ long "label-oy"
          <> metavar "\"=====\""
          <> help "Separator of original and other people's edits"
  labelEnd <-
    optional . strOption
      $ long "label-end"
          <> metavar "\">>>>>\""
          <> help "Label for end of the conflict"
  pure
    Config
      { cfgLabelStart =
          bool "<<<<<" "\ESC[1;37m<\ESC[0;31m" color `fromMaybe` labelStart
      , cfgLabelMyOld =
          bool "|||||" "\ESC[1;37m|\ESC[1;30m" color `fromMaybe` labelMyOld
      , cfgLabelOldYour =
          bool "=====" "\ESC[1;37m=\ESC[0;32m" color `fromMaybe` labelOldYour
      , cfgLabelEnd =
          bool ">>>>>" "\ESC[1;37m>\ESC[0m" color `fromMaybe` labelEnd
      , ..
      }

data Command
  = CmdDiff3
      { d3my :: FilePath
      , d3old :: FilePath
      , d3your :: FilePath
      }
  | CmdGitMerge
      { gmFiles :: Maybe [FilePath]
      , gmDoAdd :: Bool
      }
  deriving (Show)

cmdDiff3 = do
  d3my <- strArgument $ metavar "MYFILE" <> help "Version with local edits"
  d3old <- strArgument $ metavar "OLDFILE" <> help "Original file version"
  d3your <-
    strArgument $ metavar "YOURFILE" <> help "Version with other people's edits"
  pure CmdDiff3 {..}

cmdGitMerge = do
  gmFiles <-
    asum
      [ fmap Just . some
          $ strArgument
          $ metavar "UNMERGED"
              <> help "Unmerged git file (can be specified repeatedly)"
      , flag'
          Nothing
          (long "unmerged"
             <> short 'u'
             <> help "Process all files marked as unmerged by git")
      ]
  gmDoAdd <-
    asum
      [ flag'
          True
          (long "add"
             <> short 'a'
             <> help "Run `git add' for fully merged files")
      , flag' False (long "no-add" <> help "Prevent running `git add'")
      , pure False
      ]
  pure CmdGitMerge {..}

-- TODO have some option to output the (partially merged) my/old/your files so
-- that folks can continue with external program or so (such as meld)
cmd =
  hsubparser
    $ mconcat
        [ command "merge"
            $ info cmdDiff3
            $ progDesc "diff3-style merge of two changesets"
        , command "git"
            $ info cmdGitMerge
            $ progDesc "Automerge unmerged files in git conflict"
        ]

parseOpts :: IO (Config, Command)
parseOpts =
  customExecParser (prefs helpShowGlobals)
    $ info
        (liftA2 (,) config cmd
           <**> helper
           <**> simpleVersioner (showVersion version))
        (fullDesc
           <> header
                "werge -- blanks-friendly mergetool for tiny interdwindled changes"
           <> footer "werge is a free software, use it accordingly.")
