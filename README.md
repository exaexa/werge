
# werge (merge weird stuff)

This is a partial work-alike of `diff3` and `git merge` and other merge-y tools that is capable of

- merging token-size changes instead of line-size ones
- largely ignoring changes in blank characters

These properties are great for several use-cases:

- merging free-flowing text changes (such as in TeX) irrespective of linebreaks
  etc,
- merging of changesets that use different code formatters
- minimizing the conflict size of tiny changes to a few characters, making them
  easier to resolve

Better docs is WIP

## Installation

```sh
cabal install
```

## Help & features

```
werge -- blanks-friendly mergetool for tiny interdwindled changes

Usage: werge [(-F|--tok-filter FILTER) | (-i|--simple-tokens) | 
               (-I|--full-tokens)] [-s|--spaces (normal|conflict|my|old|your)] 
             [-C|--expand-context N] [--no-zeal | (-z|--zeal)] 
             [--label-start STRING] [--label-mo STRING] [--label-oy STRING] 
             [--label-end STRING] [--conflict-overlaps] [--conflict-separate]
             COMMAND

Available options:
  -F,--tok-filter FILTER   external program to separate the text to tokens
  -i,--simple-tokens       use wider character class to separate the tokens
                           (results in larger tokens and ignores case)
  -I,--full-tokens         separate characters by all known character classes
                           (default)
  -s,--spaces (normal|conflict|my|old|your)
                           mode of merging the space-only changes; instead of
                           usual resolution one may choose to always conflict or
                           to default the space from the source files (default:
                           normal)
  -C,--expand-context N    Consider changes that are at most N tokens apart to
                           be a single change. Zero may cause bad resolutions of
                           near conflicting edits. (default: 1)
  --no-zeal                avoid zealous mode (default)
  -z,--zeal                try to zealously minify conflicts, potentially
                           resolving them
  --label-start STRING     label for beginning of the conflict
                           (default: "<<<<<")
  --label-mo STRING        separator of local edits and original
                           (default: "|||||")
  --label-oy STRING        separator of original and other people's edits
                           (default: "=====")
  --label-end STRING       label for end of the conflict (default: ">>>>>")
  --conflict-overlaps      do not resolve overlapping changes
  --conflict-separate      do not resolve separate (non-overlapping) changes
  -h,--help                Show this help text
  --version                Show version information

Available commands:
  merge                    diff3-style merge of two changesets
  git                      automerge unmerged files in git conflict

werge is a free software, use it accordingly.
```
