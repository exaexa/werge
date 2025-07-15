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
                 <> help "external program to separate the text to tokens")
    , flag'
        TokenizeCharCategorySimple
        (long "simple-tokens"
           <> short 'i'
           <> help
                "use wider character class to separate the tokens (results in larger tokens and ignores case)")
    , flag'
        TokenizeCharCategory
        (long "full-tokens"
           <> short 'I'
           <> help
                "separate characters by all known character classes (default)")
    , pure TokenizeCharCategory
    ]

data Spaces
  = SpacesNormal
  | SpacesConflict
  | SpacesMy
  | SpacesOld
  | SpacesYour
  deriving (Show, Eq)

spaceMode x
  | x `isPrefixOf` "normal" = Right SpacesNormal
  | x `isPrefixOf` "conflict" = Right SpacesConflict
  | x `isPrefixOf` "my" = Right SpacesMy
  | x `isPrefixOf` "old" = Right SpacesOld
  | x `isPrefixOf` "your" = Right SpacesYour
  | otherwise =
    Left
      $ "could not parse value `"
          ++ x
          ++ "', use one of `normal', `conflict', `my', `old', and `your'"

data Config = Config
  { cfgTokenizer :: Tokenizer
  , cfgSpaces :: Spaces
  , cfgContext :: Int
  , cfgZealous :: Bool
  , cfgLabelStart :: String
  , cfgLabelMyOld :: String
  , cfgLabelOldYour :: String
  , cfgLabelEnd :: String
  , cfgResolveSpaces :: Bool
  , cfgResolveOverlaps :: Bool
  , cfgResolveSeparate :: Bool
  } deriving (Show)

config = do
  cfgTokenizer <- tokenizer
  cfgSpaces <-
    option (eitherReader spaceMode)
      $ long "spaces"
          <> short 's'
          <> metavar "(normal|conflict|my|old|your)"
          <> help
               "mode of merging the space-only changes; instead of usual resolution one may choose to always conflict or to default the space from the source files (default: normal)"
          <> value SpacesNormal
  cfgContext <-
    option auto
      $ long "expand-context"
          <> short 'C'
          <> metavar "N"
          <> value 1
          <> showDefault
          <> help
               "Consider changes that are at most N tokens apart to be a single change. Zero may cause bad resolutions of near conflicting edits."
  cfgZealous <-
    asum
      [ flag' False $ long "no-zeal" <> help "avoid zealous mode (default)"
      , flag' True
          $ long "zeal"
              <> short 'z'
              <> help
                   "try to zealously minify conflicts, potentially resolving them"
      , pure False
      ]
  color <-
    flag False True
      $ long "color"
          <> short 'G'
          <> help
               "use shorter, gaily colored output markers by default (requires ANSI color support; good for terminals or `less -R')"
  labelStart <-
    optional . strOption
      $ long "label-start"
          <> metavar "\"<<<<<\""
          <> help "label for beginning of the conflict"
  labelMyOld <-
    optional . strOption
      $ long "label-mo"
          <> metavar "\"|||||\""
          <> help "separator of local edits and original"
  labelOldYour <-
    optional . strOption
      $ long "label-oy"
          <> metavar "\"=====\""
          <> help "separator of original and other people's edits"
  labelEnd <-
    optional . strOption
      $ long "label-end"
          <> metavar "\">>>>>\""
          <> help "label for end of the conflict"
  cfgResolveOverlaps <-
    fmap not . switch
      $ long "conflict-overlaps" <> help "do not resolve overlapping changes"
  cfgResolveSeparate <-
    fmap not . switch
      $ long "conflict-separate"
          <> help "do not resolve separate (non-overlapping) changes"
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
  d3my <- strArgument $ metavar "MYFILE" <> help "version with local edits"
  d3old <- strArgument $ metavar "OLDFILE" <> help "original file version"
  d3your <-
    strArgument $ metavar "YOURFILE" <> help "version with other people's edits"
  pure CmdDiff3 {..}

cmdGitMerge = do
  gmFiles <-
    asum
      [ fmap Just . some
          $ strArgument
          $ metavar "UNMERGED"
              <> help "unmerged git file (can be specified repeatedly)"
      , flag'
          Nothing
          (long "unmerged"
             <> short 'u'
             <> help "process all files marked as unmerged by git")
      ]
  gmDoAdd <-
    asum
      [ flag'
          True
          (long "add"
             <> short 'a'
             <> help "run `git add' for fully merged files")
      , flag' False (long "no-add" <> help "prevent running `git add'")
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
            $ progDesc "automerge unmerged files in git conflict"
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
           <> (footer $ "werge is a free software, use it accordingly."))
