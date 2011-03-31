syn match Project /^[^[:blank:]][^{]\+:/
syn match DoubleQuote /\".\{-}\"/
syn match SingleQuote /'.\{-}'/
syn match DateTime /\d\{4}-\d\{2}-\d\{2}\(\s*\d\{2}:\d\{2}:\d\{2}\)\{0,1}/
syn match Starred /\*.\{-}\*/
syn match AtSymbol /@/ 
syn match URL "\(http\|ftp\)://[^[:space:]]\+"

syn match Note /^\t\+▸\_.\{-}\(\_^\t\+▸\|\_^\t\+￭\|\%$\)\@=/ fold contains=DoubleQuote,SingleQuote,DateTime,Starred,AtSymbol,URL contained transparent
syn match NoteDone /^\t\+▸.*@[Dd]one\_.\{-}\(\_^\t\+▸\|\_^\t\+￭\|\%$\)\@=/ fold
syn match Task /^\t\+￭\_.\{-}\(\_^\t\+￭\|\%$\)\@=/ fold contains=Note,NoteDone
syn match TaskDone /^\t\+￭.*@[Dd]one\_.\{-}\(\_^\t\+￭\|\%$\)\@=/ fold contains=Note,NoteDone

"syn match ConcealMarker /{{{\d\{}\|}}}\d\{}/ conceal

syn sync fromstart

hi link Project Type
hi link DoubleQuote Constant
hi link SingleQuote PreProc
hi link DateTime Comment
hi link Starred Special
hi link AtSymbol Special
hi link URL Statement

"hi link ConcealMarker Comment

hi link NoteDone Comment
hi link TaskDone Comment
