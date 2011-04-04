syn match DoubleQuote /"[[:print:]]\{-1,}"/ contained
syn match SingleQuote /'[[:print:]]\{-1,}'/ contained
syn match DateTime /\d\{4}-\d\{2}-\d\{2}\(\s*\d\{2}:\d\{2}:\d\{2}\)\{0,1}/ contained
syn match Starred /\*.\{-}\*/ contained
syn match AtSymbol /@\s/ contained
syn match URL "\(http\|ftp\)://[^[:space:]]\+" contained
syn match Context /@\w\+/ contained

syn region Note start=/^\s\+▾/ end=/^\s\{-}\(\_^\s\+▾\|\_^\s\+￭\|\_^[^[:space:]].\+:\|\%$\)\@=/ contains=Context,DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL contained
syn region Task start=/^\s\+￭/ end=/^\s\{-}\(\_^\s\+￭\|\_^[^[:space:]].\+:\|\%$\)\@=/ contains=Note,NoteDone,Context,DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL contained
syn region Proj start=/^[^[:space:]].\+:/ end=/^\s\{-}\(\_^[^[:space:]].\+:\|\%$\)\@=/ contains=Task,Note,TaskDone,NoteDone
syn region TaskDone start=/^\s\+￭.*@[Dd]one/ end=/^\s\{-}\(\_^\s\+￭\|\_^[^[:space:]].\+:\|\%$\)\@=/ contained
syn region NoteDone start=/^\s\+▾.*@[Dd]one/ end=/^\s\{-}\(\_^\s\+▾\|\_^\s\+￭\|\_^[^[:space:]].\+:\|\%$\)\@=/ contained

syn sync fromstart

hi link DoubleQuote Constant
hi link SingleQuote PreProc
hi link DateTime Comment
hi link Starred Special
hi link AtSymbol Special
hi link URL Statement
hi link Context Special
hi link Proj Type
hi link TaskDone Comment
hi link NoteDone Comment

