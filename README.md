
# werge (merge weird stuff)

This is a partial work-alike of `diff3` and `git merge` and other merge-y tools
that is capable of

- merging token-size changes instead of line-size ones
- largely ignoring changes in blank characters

These properties are great for several use-cases:

- merging free-flowing text changes (such as in TeX) irrespective of line breaks
  etc,
- merging of change sets that use different code formatters
- minimizing the conflict size of tiny changes to a few characters, making them
  easier to resolve

## Demo

Original (`old` file):
```
Roses are red. Violets are blue.
Patch is quite hard. I cannot rhyme.
```

Local changes (`my` file):
```
Roses are red. Violets are blue.
Patching is hard. I still cannot rhyme.
```

Remote changes (`your` file):
```
Roses are red.
Violets are blue.
Patch is quite hard.
I cannot do verses.
```

Token-merged version with `werge merge my orig your` (conflicts on the space
change that is too close to the disappearing "still" token):
```
Roses are red.
Violets are blue.
Patching is hard.<<<<< I still||||| I=====
I>>>>> cannot do verses.
```
(NOTE: option `-G` gives nicely colored output that is much easier to read.)

Token-merged version with separate space resolution using `-s` (conflicts get
fixed separately):
```
Roses are red.
Violets are blue.
Patching is hard.
I still cannot do verses.
```

A harder-conflicting file (`theirs`):
```
Roses are red.
Violets are blue.
Merging is quite hard.
I cannot do verses.
```

`werge merge mine orig theirs -s` highlights the actual unmergeable change:
```
Roses are red.
Violets are blue.
<<<<<Patching|||||Patch=====Merging>>>>> is hard.
I still cannot do verses.
```

## How does it work?

- Instead of lines, the files are torn to small tokens (words, spaces, symbols,
  ...) and these are diffed and merged individually.
- Some tokens are marked as spaces by the tokenizer, which allows the merge
  algorithm to be (selectively) more zealous when resolving conflicts on these.

Compared to e.g. `difftastic`, `mergiraf` and similar tools, **`werge` is
completely oblivious about the actual file structure** and works on any file
type. This choice trades off some merge quality for (a lot of) complexity.

Tokenizers are simple, implementable as linear scanners that print separate
tokens on individual lines that are prefixed with a space mark (`.` for space
and `|` for non-space), and also escape newlines and backslashes. A default
tokenization of string "hello \ world" with a new line at the end is listed
below (note the invisible space on the lines with dots):

```
|hello
. 
|\\
. 
|world
.\n
```

Users may supply any tokenizer via option `-F`, e.g. this script makes
line-size tokens (reproducing the usual line merges):

```py
#!/usr/bin/env python3
import sys
for l in sys.stdin.readlines():
    if len(l)==0: continue
    if l[-1]=='\n':
        print('|'+l[:-1].replace('\\','\\\\')+'\\n')
    else:
        print('|'+l.replace('\\','\\\\'))
```

## Installation

```sh
cabal install
```

Running of `werge` requires a working installation of `diff` compatible
with the one from [GNU diffutils](https://www.gnu.org/software/diffutils/). You
may set up a path to such `diff` (or a wrapper script) via environment variable
`WERGE_DIFF`.

## Use with `git`

`werge` can automatically process files that are marked in `git` as merge
conflicts:

```sh
$ git merge somebranch
$ werge git -ua
```

Options `-ua` (`--unmerged --add`) find all files that are marked as unmerged,
tries to merge them token-by-token, and if the merge is successful with current
settings it runs `git add` on them. The current changes in the files are
replaced by the merged (or partially merged) state; backups are written
automatically to `filename.werge-backup`.

## Current `--help` and features

```
werge -- blanks-friendly mergetool for tiny interdwindled changes

Usage: werge [(-F|--tok-filter FILTER) | (-i|--simple-tokens) | 
               (-I|--full-tokens)] [--no-zeal | (-z|--zeal)] 
             [-S|--space (keep|my|old|your)] 
             [-s | --resolve-space (normal|keep|my|old|your)] 
             [--conflict-space-overlaps] [--conflict-space-separate] 
             [--conflict-space-all] [-C|--expand-context N] 
             [--resolve (keep|my|old|your)] [--conflict-overlaps] 
             [--conflict-separate] [--conflict-all] [-G|--color] 
             [--label-start "<<<<<"] [--label-mo "|||||"] [--label-diff "|||||"]
             [--label-oy "====="] [--label-end ">>>>>"] COMMAND

Available options:
  -F,--tok-filter FILTER   External program to separate the text to tokens
  -i,--simple-tokens       Use wider character class to separate the tokens
                           (results in larger tokens and ignores case)
  -I,--full-tokens         Separate characters by all known character classes
                           (default)
  --no-zeal                avoid zealous mode (default)
  -z,--zeal                Try to zealously minify conflicts, potentially
                           resolving them
  -S,--space (keep|my|old|your)
                           Retain spacing from a selected version, or keep all
                           space changes for merging (default: keep)
  -s                       Shortcut for `--resolve-space keep' (this separates
                           space-only conflicts, enabling better automated
                           resolution)
  --resolve-space (normal|keep|my|old|your)
                           Resolve conflicts in space-only tokens separately,
                           and either keep unresolved conflicts, or resolve in
                           favor of a given version; `normal' resolves the
                           spaces together with other tokens, ignoring choices
                           in --resolve-space-* (default: normal)
  --conflict-space-overlaps
                           Never resolve overlapping changes in space-only
                           tokens
  --conflict-space-separate
                           Never resolve separate (non-overlapping) changes in
                           space-only tokens
  --conflict-space-all     Never resolve any changes in space-only tokens
  -C,--expand-context N    Consider changes that are at less than N tokens apart
                           to be a single change; 0 turns off conflict
                           expansion, 1 may cause bad resolutions of near
                           conflicting edits (default: 2)
  --resolve (keep|my|old|your)
                           Resolve general conflicts in favor of a given
                           version, or keep the conflicts (default: keep)
  --conflict-overlaps      Never resolve overlapping changes in general tokens
  --conflict-separate      Never resolve separate (non-overlapping) changes in
                           general tokens
  --conflict-all           Never resolve any changes in general tokens
  -G,--color               Use shorter, gaily colored output markers by default
                           (requires ANSI color support; good for terminals or
                           `less -R')
  --label-start "<<<<<"    Label for beginning of the conflict
  --label-mo "|||||"       Separator of local edits and original
  --label-diff "|||||"     Separator for old and new version
  --label-oy "====="       Separator of original and other people's edits
  --label-end ">>>>>"      Label for end of the conflict
  -h,--help                Show this help text
  --version                Show version information

Available commands:
  merge                    diff3-style merge of two changesets
  git                      Automerge unmerged files in git conflict
  diff                     Find differences between two files
  patch                    Apply a patch from `diff' to file
  break                    Break text to tokens
  glue                     Glue tokens back to text

werge is a free software, use it accordingly.
```

#### Manual merging
```
Usage: werge merge MYFILE OLDFILE YOURFILE

  diff3-style merge of two changesets

Available options:
  MYFILE                   Version with local edits
  OLDFILE                  Original file version
  YOURFILE                 Version with other people's edits
  -h,--help                Show this help text
```

#### Git interoperability
```
Usage: werge git (UNMERGED | (-u|--unmerged)) [(-a|--add) | --no-add]

  Automerge unmerged files in git conflict

Available options:
  UNMERGED                 Unmerged file tracked by git (can be specified
                           repeatedly)
  -u,--unmerged            Process all files marked as unmerged by git
  -a,--add                 Run `git add' for fully merged files
  --no-add                 Prevent running `git add'
  -h,--help                Show this help text
```

#### Finding differences
```
Usage: werge diff OLDFILE YOURFILE 
                  [(-u|--unified) | (-U|--unified-size ARG) | (-m|--merge)]

  Find differences between two files

Available options:
  OLDFILE                  Original file version
  YOURFILE                 File version with changes
  -u,--unified             Produce unified-diff-like output for `patch' with
                           default context size (20)
  -U,--unified-size ARG    Produce unified diff with this context size
  -m,--merge               Highlight the differences as with `merge' (default)
  -h,--help                Show this help text
```

#### Patching files in place
```
Usage: werge patch MYFILE

  Apply a patch from `diff' to file

Available options:
  MYFILE                   File to be modified
  -h,--help                Show this help text
```

#### Converting between files and tokens

```
Usage: werge break 

  Break text to tokens
```

```
Usage: werge glue 

  Glue tokens back to text
```
