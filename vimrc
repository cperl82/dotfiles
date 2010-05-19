filetype plugin indent on
filetype plugin on
syntax on


set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set autoindent
set laststatus=2
set number
set incsearch

" Turn on search highlighting
set hlsearch

" colorscheme asmdev
" colorscheme norwaytoday
colorscheme ir_black

" 2010-05-12
" Override the highlight settings for NERD_tree.  This is mainly because
" ir_black colorscheme looks terrible with treeRO linked to WarningMsg
hi link treeRO Normal

" 2010-05-17
" Playing around with trying to implement my own tabline
function! MyTabLabel(n)
	let buflist = tabpagebuflist(a:n)
	let winnr = tabpagewinnr(a:n)
	let filename = bufname(buflist[winnr - 1])
	if filename == ""
		let filename = "[No Name]"
	else
		let filename = fnamemodify(filename, ":~")
	endif
	return filename
endfunction

function! MyTabLine()
	let s = ''
	for i in range(tabpagenr('$'))
		if i+1 == tabpagenr()
			let s .= '%#TabLineSel#'
		else
			let s .= '%#TabLine#'
		endif

		let s .= ' ' . (i+1) . ' %' . (i+1) . 'T'
		let s .= '%{MyTabLabel(' . (i+1) . ')} '
		let s .= '%#TabLine#|'
	endfor

	if tabpagenr('$') > 1
		let s .= '%=%#TabLine#%999Xclose'
	endif


	return s
endfunction
set tabline=%!MyTabLine()

" 2010-05-18
" Playing around with moving tabs
" tab numbers run from 1 to n
function! MoveTabLeft()
	let current = tabpagenr()
	if current == 1
		let current = tabpagenr('$') + 1
	endif
	execute "tabmove" (current-2)
endfunction
function! MoveTabRight()
	let current = tabpagenr()
	if current == tabpagenr('$')
		let current = 0
	endif
	execute "tabmove" current
endfunction
map <silent> <C-Left> :call MoveTabLeft()<CR>
map <silent> <C-Right> :call MoveTabRight()<CR>

" Map Control-Shift-left and Control-Shift-right
" to moving left and right through the open tabs
map <silent> <C-h> :tabp<CR>
map <silent> <C-l> :tabn<CR>

