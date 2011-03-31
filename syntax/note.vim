syn match DoubleQuote /"\w\+"/
syn match SingleQuote /'\w\+'/
syn match DateTime /\d\{4}-\d\{2}-\d\{2}\(\s*\d\{2}:\d\{2}:\d\{2}\)\{0,1}/
syn match Starred /\*.\{-}\*/
syn match AtSymbol /@/ 
syn match URL "\(http\|ftp\)://[^[:space:]]\+"
syn match Context /@\w\+/
syn match Done /@[Dd]one/

syn match NotePlain /^\t\+▸\_.\{-}\(\_^\t\+▸\|\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold transparent
syn match Note /^\t\+▸\_.\{-}\(\_^\t\+▸\|\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold contains=DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL transparent
syn match NoteDone /^\t\+▸.*@[Dd]one\_.\{-}\(\_^\t\+▸\|\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold contains=Done

syn match Task /^\t\+￭\_.\{-}\(\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold contains=Note,NoteDone,Context
syn match TaskDone /^\t\+￭.*@[Dd]one\_.\{-}\(\_^\t\+￭\|^[^\t]\+:\|\%$\)\@=/ fold contains=NotePlain,NoteDone,Done

syn match Project /^[^\t]\+:\_.\{-}\(\_^[^\t]\|\%$\)\@=/ fold contains=Task,TaskDone,Note,NoteDone

"syn match ConcealMarker /{{{\d\{}\|}}}\d\{}/ conceal

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

"hi link ConcealMarker Comment

hi link NoteDone Comment
hi link TaskDone Comment
