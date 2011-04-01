syn match DoubleQuote /"[[:print:]]\{-1,}"/ contained
syn match SingleQuote /'[[:print:]]\{-1,}'/ contained
syn match DateTime /\d\{4}-\d\{2}-\d\{2}\(\s*\d\{2}:\d\{2}:\d\{2}\)\{0,1}/ contained
syn match Starred /\*.\{-}\*/ contained
syn match AtSymbol /@/ contained
syn match URL "\(http\|ftp\)://[^[:space:]]\+" contained
syn match Context /@\w\+/ contained

"syn match NotePlain /^\s\+▾\_.\{-}\(\_^\s\+▾\|\_^\s\+￭\|^[^\s]\+:\|\%$\)\@=/ transparent contains=ConcealFoldMarker
"syn match Note /^\s\+▾\_.\{-}\(\_^\s\+▾\|\_^\s\+￭\|^[^\s]\+:\|\%$\)\@=/ contains=ConcealFoldMarker,DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL transparent
"syn match NoteDone /^\s\+▾.*@[Dd]one\_.\{-}\(\_^\s\+▾\|\_^\s\+￭\|^[^\s]\+:\|\%$\)\@=/ contains=ConcealFoldMarker,Done
"syn match Task /^\s\+￭\_.\{-}\(\_^\s\+￭\|^[^\s]\+:\|\%$\)\@=/ contains=ConcealFoldMarker,Note,NoteDone,Context,DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL
"syn match TaskDone /^\s\+￭.*@[Dd]one\_.\{-}\(\_^\s\+￭\|^[^\s]\+:\|\%$\)\@=/ contains=ConcealFoldMarker,NotePlain,NoteDone,Done
"syn match Project /^[^\s]\+:\_.\{-}\(\_^[^\s]\|\%$\)\@=/ contains=ConcealFoldMarker,Task,TaskDone,Note,NoteDone
"syn match ConcealFoldMarker /{{{\d\{}\|}}}\d\{}/ conceal

syn region Note start=/^\s\+▾/ end=/^\s\{-}\(\_^\s\+▾\|\_^\s\+￭\|\_^[^[:space:]].\+:\|\%$\)\@=/ contains=Context,DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL contained transparent
syn region Task start=/^\s\+￭/ end=/^\s\{-}\(\_^\s\+￭\|\_^[^[:space:]].\+:\|\%$\)\@=/ contains=Note,Context,DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL contained transparent
syn region Proj start=/^[^[:space:]].\+:/ end=/^\s\{-}\(\_^[^[:space:]].\+:\|\%$\)\@=/


hi link DoubleQuote Constant
hi link SingleQuote PreProc
hi link DateTime Comment
hi link Starred Special
hi link AtSymbol Special
hi link URL Statement
hi link Context Special
hi link Proj Type


