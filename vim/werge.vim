" Vim syntax file
" Language: werge
" Maintainer: Mirek Kratochvil
" Last Change: Oct 14, 2025
" Version: 1
" URL: https://github.com/exaexa/werge

" quit if a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" syntax for the werge files
syntax region wergeHunk start=/<<<<</ end=/>>>>>/ contains=wergeRm,wergeDiffAdd,wergeConflictOrigAdd
syntax region wergeRm  start=/<<<<</hs=e+1  end=/|||||/he=s-1,me=s-1 contained
syntax match wergeDiffAdd /|||||\([^>=]\|>\{1,4\}>\@!\|=\{1,4\}=\@!\)*>>>>>/ms=s+5,me=e-5 contained
syntax match wergeConflictOrigAdd /|||||\([^=>]\|=\{1,4\}=\@!\|>\{1,4\}>\@!\)*=====\([^=>]\|=\{1,4\}=\@!\|>\{1,4\}>\@!\)*>>>>>/me=e-5 contained contains=wergeConflictOrig,wergeconflictAdd
syntax region wergeConflictOrig start=/|||||/hs=e+1  end=/=====/he=s-1,me=s-1 contained
syntax region wergeConflictAdd start=/=====/hs=e+1  end=/>>>>>/he=s-1,me=s-1 contained

" color specification
highlight default link wergeHunk Comment
highlight default link wergeRm Removed
highlight default link wergeDiffAdd Added
highlight default link wergeConflictOrigAdd Comment
highlight default link wergeConflictOrig Changed
highlight default link wergeConflictAdd Added

let b:current_syntax = "werge"
