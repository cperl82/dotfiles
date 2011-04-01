imap <buffer> <Leader>t <C-r>=strftime("%Y-%m-%d")<CR>
imap <buffer> <Leader>T <C-r>=strftime("%Y-%m-%d %H:%M:%S")<CR>
imap <buffer> <Leader>v ￭  {{{2<Left><Left><Left><Left><Left>
imap <buffer> <Leader>b ▾  {{{3<Left><Left><Left><Left><Left>

setlocal tw=90 sw=4 ts=4 softtabstop=4 formatoptions+=t formatoptions-=l
setlocal foldmethod=marker foldminlines=0 foldcolumn=2

setlocal foldtext=MyFoldText()
function! MyFoldText()
	let line = getline(v:foldstart)
	let line = substitute(line, '{{{\d\+', '', '')
	let line = substitute(line, '\t', '    ', 'g')
	let line = substitute(line, '▾', '▸', '')
	let size = (v:foldend - v:foldstart) + 1
	
	" Special calculation of multibyte characters
	" :help strlen
	let linelen = strlen(substitute(line, '.', 'x', 'g'))

	" calculate the number of "."'s needed to reach the edge of the window
	let dots = ""
	let ndots = winwidth(0) - &foldcolumn - (&number ? &numberwidth : 0) - linelen - len(size) - len(" lines") - 2
	for i in range(ndots)
		let dots .= "."
	endfor

	return line . dots . size . " lines"
endfunction
