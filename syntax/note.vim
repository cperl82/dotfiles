syn keyword Todo TODO FIXME XXX

syn match LineContinuation /\\$/
syn match DoubleQuote /"\_[[:print:]]\{-}"/
syn match SingleQuote /'[^[:space:]]\{-}'/
syn match DateTime /\d\{4}-\d\{2}-\d\{2}\(\s*\d\{2}:\d\{2}:\d\{2}\)\{0,1}/
syn match Starred /\*.\{-}\*/
syn match AtSymbol /@\s/
syn match URL "\(http\|https\|ftp\)://[^[:space:]]\+"
syn match Context /\s\+@[[:alnum:]-]\+/
syn match ProjHeader /^[^[:space:]].\+:/

syn region TaskDone start=/^\s\+￭.*@[Dd]one/ end=/^\s\{-}\(\_^\s\+￭\|\_^[^[:space:]].\+:\|\%$\)\@=/
syn region NoteDone start=/^\s\+▾.*@[Dd]one/ end=/^\s\{-}\(\_^\s\+▾\|\_^\s\+￭\|\_^[^[:space:]].\+:\|\%$\)\@=/

syn sync fromstart

hi link LineContinuation Todo
hi link DoubleQuote PreProc
hi link SingleQuote Constant
hi link DateTime Comment
hi link Starred Special
hi link AtSymbol Special
hi link URL Statement
hi link Context Special
hi link ProjHeader Type
hi link TaskDone Comment
hi link NoteDone Comment
