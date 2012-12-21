syn match CPDoubleQuote /"\_[[:print:]]\{-}"/
syn match CPTripleQuote /"""\_.\{-}"""/
syn match CPSingleQuote /'[^[:space:]]\{-}'/
syn match CPHashMark /\s\+#.*$/
syn match CPContext /\(^\|\s\+\)@[^[:space:]]\+/
syn match CPBlockQuote /^s*>/
syn match CPDateTime /\d\{4}-\d\{2}-\d\{2}\(|\d\{2}:\d\{2}:\d\{2}\)\{0,1}/
syn match CPNumberBox /\[\d\+\]/

hi link CPDoubleQuote Type
hi link CPTripleQuote Type
hi link CPSingleQuote Constant
hi link CPHashMark Comment
hi link CPContext Directory
hi link CPBlockQuote NonText
hi link CPDateTime Directory
hi link CPNumberBox Directory
