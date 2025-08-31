
# werge (merge weird stuff)

This is a partial work-alike of `diff3`, `patch`, `git merge` and other merge-y
tools that is capable of:

- merging token-size changes (words, identifiers, sentences) instead of
  line-size ones
- merging changes in blank characters separately or ignoring them altogether

These properties are great for several use-cases:

- combining changes in free-flowing text (such as in TeX or Markdown),
  irrespectively of changed line breaks, paragraph breaking and justification,
  etc.
- merging of code formatted with different code formatters
- minimizing the conflict size of tiny changes to a few characters, making them
  easier to resolve

Separate `diff`&`patch` functionality is provided too for sending
token-granularity patches. (The patches are similar to what `git diff
--word-diff` produces, but can be applied to files.)

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

Technically, the ideas are similar to
[`spiff`](http://hpux.connect.org.uk/hppd/hpux/Text/spiff-1.0/) or `git diff
--word-diff`.  Other tools exist such as
[`difftastic`](https://difftastic.wilfred.me.uk/) and
[`mergiraf`](https://mergiraf.org/) that are aware of the file structure (i.e.,
the actual syntax _tree_) that can be used to improve output.  Compared to
these, **`werge` is completely oblivious about the actual file structure**, and
thus works quite well on any file type.  This choice trades off some diff&merge
quality for (a lot of) complexity.

Tokenizers in `werge` are simple, implementable as linear scanners that print
separate tokens on individual lines that are prefixed with a space mark (`.`
for space and `/` for non-space), and escape newlines and backslashes. A
default tokenization of string "hello \ world" with a new line at the end is
listed below (note the invisible space on the lines with dots):

```
/hello
. 
/\\
. 
/world
.\n
```

### Custom tokenizers

Users may supply any tokenizer via option `-F`. The script below produces
line-size tokens for demonstration (in turn, `werge` will do the usual line
merges), and can be used e.g. via `-F ./tokenize.py`:

```py
#!/usr/bin/env python3
import sys
for l in sys.stdin.readlines():
    if len(l)==0: continue
    if l[-1]=='\n':
        print('/'+l[:-1].replace('\\','\\\\')+'\\n')
    else:
        print('/'+l.replace('\\','\\\\'))
```

### History

I previously made an attempt to solve this in `adiff` software, which failed
because the approach was too complex. Before that, the issue was tackled by
Arek Antoniewicz on MFF CUNI, who used regex-edged DFAs (REDFAs) to construct
user-specifiable tokenizers in a pretty cool way.

## Installation

```sh
cabal install
```

Running of `werge` requires a working installation of `diff` compatible
with the one from [GNU diffutils](https://www.gnu.org/software/diffutils/). You
may set up a path to such `diff` (or a wrapper script) via environment variable
`WERGE_DIFF`.

## Integration with `git`

### Automerging conflicts

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

Optionally, you can specify exact files to be automerged. That is useful for
cases when only some of the conflicting files should be processed by `werge`:

```sh
$ werge git my/conflicting/file.txt
```

Support for merging complex types of changes (deletes, directory moves,
symlinks, ...) via this interface is currently limited. `werge` can be used as
a mergetool or a merge driver to ameliorate that.

### Use as `git difftool` and `git mergetool`

The `git` config below allows direct use of `werge` as `git difftool -t werge`
and `git mergetool -t werge`:
```ini
[difftool "werge"]
	cmd = werge diff -G $LOCAL $REMOTE
[mergetool "werge"]
	cmd = werge merge $LOCAL $BASE $REMOTE > $MERGED
	trustExitCode = true

# variant for separate resolution of space (solves more conflicts):
[mergetool "spacewerge"]
	cmd = werge merge -s $LOCAL $BASE $REMOTE > $MERGED
	trustExitCode = true
```

One issue with `git` mergetools is that they are supposed to be interactive,
and thus `git` assumes them to always produce a completely merged, conflictless
result. In turn, the auto-merging with `git mergetool -t werge` fails with
conflicts, `git` assumes a complete failure and restores the original version
from the backup. To enable a more useful behavior, use `werge` as a merge
driver (see below).

### Use as a `git` merge driver

Add this to your git config:
```ini
[merge "werge"]
	name = werge
	driver = werge merge %A %O %B > %P
	recursive = binary
```

Then, specify that the "werge" driver should be used for certain files in your
repository's `.gitattributes`:
```
*.md merge=werge
*.tex merge=werge
# ... etc
```

With this in place, `git merge` will automatically run `werge` to merge the
marked files in the repository. On conflict, you will have the files marked
with the usual (werge's usual) conflict markers, and you will be able to
resolve them just as with the normal merging workflow.

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
Usage: werge patch (MYFILE | (-f|--format)) [-p|--patch PATCH]

  Modify a file using a patch from `diff'

Available options:
  MYFILE                   File to be patched
  -f,--format              Do not patch anything, only format the patch using
                           conflict marks on joined tokens
  -p,--patch PATCH         File with the patch (default: stdin)
  -h,--help                Show this help text
```

#### Converting between files and tokens

Both commands work as plain stdin-to-stdout filters:

```
Usage: werge break 

  Break text to tokens
```

```
Usage: werge glue 

  Glue tokens back to text
```
