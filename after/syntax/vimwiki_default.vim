syn match DoubleQuote /"\_[[:print:]]\{-}"/
syn match TripleQuote /"""\_.\{-}"""/
syn match SingleQuote /'[^[:space:]]\{-}'/
syn match HashMark /#.*$/
syn match DateTime /\d\{4}-\d\{2}-\d\{2}\(\s*\d\{2}:\d\{2}:\d\{2}\)\{0,1}/

hi link TripleQuote PreProc
hi link DoubleQuote PreProc
hi link SingleQuote Constant
hi link HashMark Comment
hi link DateTime Comment
