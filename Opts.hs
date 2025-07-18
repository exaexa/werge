{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Opts where

import Data.Bool
import Data.List
import Data.Maybe
import OptEnvConf
import Paths_werge (version)

data Tokenizer
  = TokenizeFilter String
  | TokenizeCharCategory
  | TokenizeCharCategorySimple
  deriving (Show)

tokenizer :: Parser Tokenizer
tokenizer =
  withShownDefault (const "full tokens") TokenizeCharCategory
    $ choice
        [ TokenizeFilter
            <$> strOption
                  [ long "tok-filter"
                  , short 'F'
                  , metavar "FILTER"
                  , help "External program to separate the text to tokens"
                  ]
        , setting
            [ switch TokenizeCharCategorySimple
            , long "simple-tokens"
            , short 'i'
            , help
                "Use wider character class to separate the tokens (results in larger tokens and ignores case)"
            ]
        , setting
            [ switch TokenizeCharCategory
            , long "full-tokens"
            , short 'I'
            , help "Separate characters by all known character classes"
            ]
        ]

data ConflictMask = ConflictMask
  { cmResolveOverlaps :: Bool
  , cmResolveSeparate :: Bool
  } deriving (Show)

conflictMask :: String -> String -> Parser ConflictMask
conflictMask label objs = do
  cmResolveOverlaps' <-
    not
      <$> setting
            [ switch True
            , long (label ++ "-overlaps")
            , help ("Never resolve overlapping changes in " ++ objs)
            ]
  cmResolveSeparate' <-
    not
      <$> setting
            [ switch True
            , long (label ++ "-separate")
            , help
                ("Never resolve separate (non-overlapping) changes in " ++ objs)
            ]
  cmAll <-
    not
      <$> setting
            [ switch True
            , long (label ++ "-all")
            , help ("Never resolve any changes in " ++ objs)
            ]
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

resolutionMode :: String -> Either String Resolution
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

spaceMode :: String -> Either String SpaceResolution
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
  , cfgLabelDiff :: String
  , cfgLabelOldYour :: String
  , cfgLabelEnd :: String
  } deriving (Show)

config :: Parser Config
config = do
  cfgTokenizer <- tokenizer
  cfgZealous <-
    withShownDefault (const "not zealous") False
      $ choice
          [ setting [switch False, long "no-zeal", help "avoid zealous mode"]
          , setting
              [ switch True
              , long "zeal"
              , short 'z'
              , help
                  "Try to zealously minify conflicts, potentially resolving them"
              ]
          ]
  cfgSpaceRetain <-
    setting
      [ option
      , reader (eitherReader resolutionMode)
      , long "space"
      , short 'S'
      , metavar "(keep|my|old|your)"
      , help
          "Retain spacing from a selected version, or keep all space changes for merging (default: keep)"
      , value ResolveKeep
      ]
  cfgSpaceResolution <-
    choice
      [ setting
          [ switch (SpaceSpecial ResolveKeep)
          , short 's'
          , help
              "Shortcut for `--resolve-space keep' (this separates space-only conflicts, enabling better automated resolution)"
          ]
      , setting
          [ option
          , reader (eitherReader spaceMode)
          , long "resolve-space"
          , metavar ("(normal|keep|my|old|your)")
          , value SpaceNormal
          , help
              "Resolve conflicts in space-only tokens separately, and either keep unresolved conflicts, or resolve in favor of a given version; `normal' resolves the spaces together with other tokens, ignoring choices in --resolve-space-* (default: normal)"
          ]
      ]
  cfgSpaceConflicts <- conflictMask "conflict-space" "space-only tokens"
  cfgContext <-
    setting
      [ option
      , reader auto
      , long "expand-context"
      , short 'C'
      , metavar "N"
      , value 2
      , help
          "Consider changes that are at less than N tokens apart to be a single change; 0 turns off conflict expansion, 1 may cause bad resolutions of near conflicting edits"
      ]
          --, showDefault
  cfgResolution <-
    setting
      [ option
      , reader (eitherReader resolutionMode)
      , long "resolve"
      , metavar "(keep|my|old|your)"
      , value ResolveKeep
      , help
          "Resolve general conflicts in favor of a given version, or keep the conflicts (default: keep)"
      ]
  cfgConflicts <- conflictMask "conflict" "general tokens"
  color <-
    setting
      [ switch True
      , value False
      , long "color"
      , short 'G'
      , help
          "Use shorter, gaily colored output markers by default (requires ANSI color support; good for terminals or `less -R')"
      ]
  labelStart <-
    optional
      $ setting
          [ option
          , reader str
          , long "label-start"
          , metavar "\"<<<<<\""
          , help "Label for beginning of the conflict"
          ]
  labelMyOld <-
    optional
      $ setting
          [ option
          , reader str
          , long "label-mo"
          , metavar "\"|||||\""
          , help "Separator of local edits and original"
          ]
  labelDiff <-
    optional
      $ setting
          [ option
          , reader str
          , long "label-diff"
          , metavar "\"|||||\""
          , help "Separator for old and new version"
          ]
  labelOldYour <-
    optional
      $ setting
          [ option
          , reader str
          , long "label-oy"
          , metavar "\"=====\""
          , help "Separator of original and other people's edits"
          ]
  labelEnd <-
    optional
      $ setting
          [ option
          , reader str
          , long "label-end"
          , metavar "\">>>>>\""
          , help "Label for end of the conflict"
          ]
  pure
    Config
      { cfgLabelStart =
          bool "<<<<<" "\ESC[1;37m<\ESC[0;31m" color `fromMaybe` labelStart
      , cfgLabelMyOld =
          bool "|||||" "\ESC[1;37m|\ESC[1;30m" color `fromMaybe` labelMyOld
      , cfgLabelDiff =
          bool "|||||" "\ESC[1;37m|\ESC[0;32m" color `fromMaybe` labelDiff
      , cfgLabelOldYour =
          bool "=====" "\ESC[1;37m=\ESC[0;32m" color `fromMaybe` labelOldYour
      , cfgLabelEnd =
          bool ">>>>>" "\ESC[1;37m>\ESC[0m" color `fromMaybe` labelEnd
      , ..
      }

data Cmd
  = CmdDiff3
      { d3my :: FilePath
      , d3old :: FilePath
      , d3your :: FilePath
      }
  | CmdGitMerge
      { gmFiles :: Maybe [FilePath]
      , gmDoAdd :: Bool
      }
  | CmdDiff
      { diffOld :: FilePath
      , diffNew :: FilePath
      }
  deriving (Show)

cmdDiff3 :: Parser Cmd
cmdDiff3 = do
  d3my <- setting [argument, reader str, metavar "MYFILE", help "Version with local edits"]
  d3old <- setting [argument, reader str, metavar "OLDFILE", help "Original file version"]
  d3your <-
    setting
      [argument, reader str, metavar "YOURFILE", help "Version with other people's edits"]
  pure CmdDiff3 {..}

cmdGitMerge :: Parser Cmd
cmdGitMerge = do
  gmFiles <-
    choice
      [ fmap Just . some
          $ setting
              [ argument
              , reader str
              , metavar "UNMERGED"
              , help
                  "Unmerged file tracked by git (can be specified repeatedly)"
              ]
      , setting
          [ switch Nothing
          , long "unmerged"
          , short 'u'
          , help "Process all files marked as unmerged by git"
          ]
      ]
  gmDoAdd <- -- TODO yesNoSwitch
    withShownDefault (const "does not git-add") False
      $ choice
          [ setting
              [ switch True
              , long "add"
              , short 'a'
              , help "Run `git add' for fully merged files"
              ]
          , setting
              [switch False, long "no-add", help "Prevent running `git add'"]
          ]
  pure CmdGitMerge {..}

cmdDiff :: Parser Cmd
cmdDiff = do
  diffOld <- setting [argument, reader str, metavar "OLDFILE", help "Original file version"]
  diffNew <-
    setting [argument, reader str, metavar "NEWFILE", help "File version with changes"]
  pure CmdDiff {..}

-- TODO have some option to output the (partially merged) my/old/your files so
-- that folks can continue with external program or so (such as meld)
cmd :: Parser Cmd
cmd =
  commands
    [ command "merge" "diff3-style merge of two changesets" cmdDiff3
    , command "git" "Automerge unmerged files in git conflict" cmdGitMerge
    , command "diff" "Highlight differences between two files" cmdDiff
    ]

parseOpts :: IO (Config, Cmd)
parseOpts =
  runParser
    version
    "werge -- blanks-friendly mergetool for tiny interdwindled changes"
    $ liftA2 (,) config cmd
