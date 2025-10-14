
syntax region wergeDiffChange start=/<<<<</ end=/>>>>>/ contains=wergeDiffRm,wergeDiffAdd
syntax region wergeDiffRm  start=/<<<<</hs=e+1  end=/|||||/he=b-1,me=b-1 contained
syntax region wergeDiffAdd start=/|||||/hs=e+1  end=/>>>>>/he=b-1,me=b-1 contained

highlight default link wergeDiffChange Comment
highlight default link wergeDiffRm Removed
highlight default link wergeDiffAdd Added
