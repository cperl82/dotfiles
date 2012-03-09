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

" 2012-02-14
" Direct override of what is defined in bundles/vimwiki/syntax/vimwiki.vim.  I
" use the markdown syntax.  That means headings are represented with '#'.
" However, I do not want # characters that appear indented to be headers.  For
" example, I'd like to be able to include snippets of shell code (with
" {{{shell) that has comments in it, and not have it interfere with the
" headers.  Forcing headers to begin at the first column is a small compromise
" in my opinion.
" For a concreate example, see Cassandra.wiki
if g:vimwiki_symH
  "" symmetric
  let g:vimwiki_rxH1 = '^'.g:vimwiki_rxH.'\{1}[^'.g:vimwiki_rxH.']\+.*[^'.g:vimwiki_rxH.']\+'.g:vimwiki_rxH.'\{1}\s*$'
  let g:vimwiki_rxH2 = '^'.g:vimwiki_rxH.'\{2}[^'.g:vimwiki_rxH.']\+.*[^'.g:vimwiki_rxH.']\+'.g:vimwiki_rxH.'\{2}\s*$'
  let g:vimwiki_rxH3 = '^'.g:vimwiki_rxH.'\{3}[^'.g:vimwiki_rxH.']\+.*[^'.g:vimwiki_rxH.']\+'.g:vimwiki_rxH.'\{3}\s*$'
  let g:vimwiki_rxH4 = '^'.g:vimwiki_rxH.'\{4}[^'.g:vimwiki_rxH.']\+.*[^'.g:vimwiki_rxH.']\+'.g:vimwiki_rxH.'\{4}\s*$'
  let g:vimwiki_rxH5 = '^'.g:vimwiki_rxH.'\{5}[^'.g:vimwiki_rxH.']\+.*[^'.g:vimwiki_rxH.']\+'.g:vimwiki_rxH.'\{5}\s*$'
  let g:vimwiki_rxH6 = '^'.g:vimwiki_rxH.'\{6}[^'.g:vimwiki_rxH.']\+.*[^'.g:vimwiki_rxH.']\+'.g:vimwiki_rxH.'\{6}\s*$'
  let g:vimwiki_rxHeader = '^\('.g:vimwiki_rxH.'\{1,6}\)\zs[^'.g:vimwiki_rxH.']\+.*[^'.g:vimwiki_rxH.']\+\ze\1\s*$'
else
  "" asymm
  let g:vimwiki_rxH1 = '^'.g:vimwiki_rxH.'\{1}[^'.g:vimwiki_rxH.']\+.*'
  let g:vimwiki_rxH2 = '^'.g:vimwiki_rxH.'\{2}[^'.g:vimwiki_rxH.']\+.*'
  let g:vimwiki_rxH3 = '^'.g:vimwiki_rxH.'\{3}[^'.g:vimwiki_rxH.']\+.*'
  let g:vimwiki_rxH4 = '^'.g:vimwiki_rxH.'\{4}[^'.g:vimwiki_rxH.']\+.*'
  let g:vimwiki_rxH5 = '^'.g:vimwiki_rxH.'\{5}[^'.g:vimwiki_rxH.']\+.*'
  let g:vimwiki_rxH6 = '^'.g:vimwiki_rxH.'\{6}[^'.g:vimwiki_rxH.']\+.*'
  let g:vimwiki_rxHeader = '^\('.g:vimwiki_rxH.'\{1,6}\)\zs[^'.g:vimwiki_rxH.']\+.*\ze'
endif
