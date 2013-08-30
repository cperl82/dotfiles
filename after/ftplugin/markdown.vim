if has("folding") && exists("g:markdown_folding")
  setlocal foldexpr=MarkdownFold()
  setlocal foldmethod=expr
  let b:undo_ftplugin .= " foldexpr< foldmethod<"

  function! MarkdownFold()
    let line = getline(v:lnum)

    " Regular headers
    let depth = match(line, '\(^#\+\)\@<=\( .*$\)\@=')
    if depth > 0
      return ">" . depth
    endif

    " Setext style headings
    let nextline = getline(v:lnum + 1)
    if (line =~ '^.\+$') && (nextline =~ '^=\+$')
      return ">1"
    endif

    if (line =~ '^.\+$') && (nextline =~ '^-\+$')
      return ">2"
    endif

    return "="
  endfunction
endif

setlocal et ts=2 sts=2 sw=2 tw=79
setlocal clipboard=unnamed
inoremap <buffer> <Leader>T <C-r>=strftime("%Y-%m-%d")<CR>
inoremap <buffer> <Leader>t <C-r>=strftime("%Y-%m-%dT%H:%M:%S")<CR>
nnoremap <Space> za

function! MarkdownText()
	let line = getline(v:foldstart)
	let size = (v:foldend - v:foldstart) + 1

        " Special calculation of multibyte characters
	"         " :help strlen
	let linelen = strlen(substitute(line, '.', 'x', 'g'))

	" calculate the number of spaces needed to reach the edge of the
	" window
	let spaces = ""
	let nspaces = winwidth(0) - &foldcolumn - (&number ? &numberwidth : 0) - linelen - len(size) - len(" lines") - 2
	for i in range(nspaces)
		let spaces .= " "
	endfor
	return line . spaces . size . " lines"
endfunction
setlocal foldtext=MarkdownText()

function! GmailLinkToClipboard(hash)
	let @+ = printf("https://mail.google.com/mail/u/0/#all/%s", a:hash)
endfunction
nnoremap <silent> <LocalLeader>cg :call GmailLinkToClipboard("<C-R>=expand("<cword>")<CR>")<CR>
