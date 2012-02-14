syn match CPDoubleQuote /"\_[[:print:]]\{-}"/
syn match CPTripleQuote /"""\_.\{-}"""/
syn match CPSingleQuote /'[^[:space:]]\{-}'/
syn match CPHashMark /\s\+#.*$/
syn match CPContext /\(^\|\s\+\)@[^[:space:]]\+/
syn match CPBlockQuote /^s*>/

hi link CPTripleQuote PreProc
hi link CPDoubleQuote Type
hi link CPSingleQuote Constant
hi link CPHashMark Comment
hi link CPContext Comment
hi link CPBlockQuote NonText
