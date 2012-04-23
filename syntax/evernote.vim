syn keyword Todo TODO FIXME XXX NOTE

syn match CPDoubleQuote /"\_[[:print:]]\{-}"/
syn match CPTripleQuote /"""\_.\{-}"""/
syn match CPSingleQuote /'[^[:space:]]\{-}'/
syn match CPHashMark /\s\+#.*$/
syn match CPContext /\(^\|\s\+\)@[^[:space:]]\+/
syn match CPBlockQuote /^s*>/
syn match CPUrl `\S\{-}://[^[:space:]]\+`

hi link CPDoubleQuote Type
hi link CPTripleQuote Type
hi link CPSingleQuote Constant
hi link CPHashMark Comment
hi link CPContext Directory
hi link CPBlockQuote NonText
hi link CPUrl Type
