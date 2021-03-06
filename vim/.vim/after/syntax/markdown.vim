syn match CPDoubleQuote /"\_[[:print:]]\{-}"/
syn match CPTripleQuote /"""\_.\{-}"""/
syn match CPSingleQuote /'[^[:space:]]\{-}'/
syn match CPHashMark    /\s\+#.*$/
syn match CPContext     /\(^\|\s\+\)@[^[:space:]]\+/
syn match CPBlockQuote  /^s*>/
syn match CPDateTime    /\d\{4}-\d\{2}-\d\{2}\(T\d\{2}:\d\{2}:\d\{2}\)\{0,1}/
syn match CPNumberBox   /\[\d\+\]/
syn match CPGmailHash   /[0-9a-f]\{16}/

hi link CPDoubleQuote Type
hi link CPTripleQuote Type
hi link CPSingleQuote Constant
hi link CPHashMark    Comment
hi link CPContext     Directory
hi link CPBlockQuote  NonText
hi link CPDateTime    PreProc
hi link CPNumberBox   Directory
hi link CPGmailHash   Underlined

hi link markdownItalic Constant
