syn match CPDoubleQuote /"\_[[:print:]]\{-}"/
syn match CPTripleQuote /"""\_.\{-}"""/
syn match CPSingleQuote /'[^[:space:]]\{-}'/
syn match CPHashMark /#.*$/
syn match CPContext /@[^[:space:]]\+/

hi link CPTripleQuote PreProc
hi link CPDoubleQuote PreProc
hi link CPSingleQuote Constant
hi link CPHashMark Comment
hi link CPContext Comment
