imap <buffer> <Leader>t <C-r>=strftime("%Y-%m-%d")<CR>
imap <buffer> <Leader>T <C-r>=strftime("%Y-%m-%d %H:%M:%S")<CR>
imap <buffer> <Leader>v ￭  {{{2<Left><Left><Left><Left><Left>
imap <buffer> <Leader>b ▸  {{{3<Left><Left><Left><Left><Left>

setlocal tw=90 sw=4 ts=4 softtabstop=4 formatoptions+=t formatoptions-=l
setlocal foldmethod=marker foldminlines=0 foldcolumn=2

setlocal foldtext=MyFoldText()
function! MyFoldText()
	let line = getline(v:foldstart)
	let size = (v:foldend - v:foldstart) + 1
	let line = substitute(line, '{{{\d\+', '', '')
	let spaces = ""

	" Calculate the appropriate number of leading spaces so the folded
	" line matches up with the real text underneath it
	for i in range((len(v:folddashes)-1) * (&ts-1))
		let spaces .= " "
	endfor

	" calculate the number of "."'s needed to reach the edge of the window
	let dots = ""
	let ndots = winwidth(0) - &foldcolumn - (&number ? &numberwidth : 0) - len(spaces) - len(line) - len(size) - len(" lines") 
	for i in range(ndots)
		let dots .= "."
	endfor

	return spaces . line . dots . size . " lines"
endfunction
