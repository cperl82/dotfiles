syn match DoubleQuote /"[[:print:]]\{-1,}"/
syn match SingleQuote /'[[:print:]]\{-1,}'/
syn match DateTime /\d\{4}-\d\{2}-\d\{2}\(\s*\d\{2}:\d\{2}:\d\{2}\)\{0,1}/
syn match Starred /\*.\{-}\*/
syn match AtSymbol /@/ 
syn match URL "\(http\|ftp\)://[^[:space:]]\+"
syn match Context /@\w\+/
syn match Done /@[Dd]one/

syn match NotePlain /^\t\+▾\_.\{-}\(\_^\t\+▾\|\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold transparent contains=ConcealFoldMarker
syn match Note /^\t\+▾\_.\{-}\(\_^\t\+▾\|\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold contains=ConcealFoldMarker,DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL transparent
syn match NoteDone /^\t\+▾.*@[Dd]one\_.\{-}\(\_^\t\+▾\|\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold contains=ConcealFoldMarker,Done

syn match Task /^\t\+￭\_.\{-}\(\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold contains=ConcealFoldMarker,Note,NoteDone,Context,DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL
syn match TaskDone /^\t\+￭.*@[Dd]one\_.\{-}\(\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold contains=ConcealFoldMarker,NotePlain,NoteDone,Done

syn match Project /^[^\t]\+:\_.\{-}\(\_^[^\t]\|\%$\)\@=/ fold contains=ConcealFoldMarker,Task,TaskDone,Note,NoteDone

syn match ConcealFoldMarker /{{{\d\{}\|}}}\d\{}/ conceal

syn sync fromstart

hi link Project Type
hi link DoubleQuote Constant
hi link SingleQuote PreProc
hi link DateTime Comment
hi link Starred Special
hi link AtSymbol Special
hi link URL Statement
hi link Context Special
hi link Done Special

hi link ConcealFoldMarker Comment

hi link NoteDone Comment
hi link TaskDone Comment
