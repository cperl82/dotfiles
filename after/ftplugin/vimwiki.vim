imap <buffer> <Leader>t <C-r>=strftime("%Y-%m-%d")<CR>
imap <buffer> <Leader>T <C-r>=strftime("%Y-%m-%d\|%H:%M:%S")<CR>
imap <buffer> <Leader>v ￭ 
imap <buffer> <Leader>b ▾ 

setlocal et tw=79 sw=2 ts=2 softtabstop=2 formatoptions+=t formatoptions-=l concealcursor=n

" 2012-02-14
" Make it so that writing a bulleted list like:
" - some text that is long enough to wrap
"   actually wrap to the proper indent
setlocal comments+=fb:- formatoptions+=cq
setlocal fdm=marker foldminlines=0 fdc=0
setlocal foldtext=MyFoldText()

function! MyFoldText()
	let line = getline(v:foldstart)
	let line = substitute(line, '{{{\d\+\s\{,1}', '', '')
	let line = substitute(line, '\t', '    ', 'g')
	let line = substitute(line, '▾', '▸', '')
	let size = (v:foldend - v:foldstart) + 1

	" Special calculation of multibyte characters
	" :help strlen
	let linelen = strlen(substitute(line, '.', 'x', 'g'))

	" calculate the number of "."'s needed to reach the edge of the window
	let dots = ""
	let ndots = winwidth(0) - &foldcolumn - (&number ? &numberwidth : 0) - linelen - len(size) - len(" lines") - 3
	for i in range(ndots)
		let dots .= "."
	endfor

	return line . dots . " " . size . " lines"
endfunction
