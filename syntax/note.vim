syn match cptest00 /\".\{-}\"/
syn match cptest01 /'.\{-}'/
syn match cptest02 /\d\{4}-\d\{2}-\d\{2}\(\s*\d\{2}:\d\{2}:\d\{2}\)\{0,1}/
syn match cptest03 /\*.\{-}\*/
syn match cptest04 /@/ 
"syn match cptest05 /@[^[:space:]]\+/
"syn match cptest06 /^.*@done.*$/ contains=concealtest00
syn match cptest07 /^[^[:blank:]][^{]\+/
syn match cptest08 "\(http\|ftp\)://[^[:space:]]\+"
"syn match cptest09 /_[^[:blank:]]\{1,}_/

syn match ConcealMarker /{{{\d\{}\|}}}\d\{}/ conceal

syn match Subnote /^\(\t\+\)▸\_.\{-}\_^\1/ contains=ConcealMarker,DoneSubnote,cptest00,cptest01,cptest02,cptest03,cptest04,cptest08
syn match DoneSubnote /^\(\t\+\)▸.*@done\_.\{-}\(\_^\1▸\)\@=/

syn sync fromstart

hi link cptest00 Constant
hi link cptest01 PreProc
hi link cptest02 Comment
hi link cptest03 Special
hi link cptest04 Special
""hi link cptest05 Special
hi link cptest06 Comment
hi link cptest07 Type
hi link cptest08 Statement
"hi link cptest09 Statement
hi link ConcealMarker Comment

hi link DoneSubnote Comment
