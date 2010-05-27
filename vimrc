" Enable file type detection, all three modes
" detection, indent, plugin.  See :help filetype
filetype plugin indent on

" Turn on syntax highlighting, using defaults
" See :help syntax
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
		let filename = fnamemodify(filename, ":.")
	endif
	return filename
endfunction

function! MyTabLine()
	let tabList = []
	for i in range(1, tabpagenr('$'))
		let s = ' ' . i . ' %' . i . 'T' . '%{MyTabLabel(' . i . ')}' . ' '
		call add(tabList, s)
		if i == tabpagenr()
			let tabList[i-1] = '%#TabLineSel#' . tabList[i-1] . '%#TabLine#'
		else
			let tabList[i-1] = '%#TabLine#' . tabList[i-1]
		endif
	endfor
	return join(tabList, '|')
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
" Map Control-Left and Control-Right
" to dragging a tab left or right
map <silent> <C-Left> :call MoveTabLeft()<CR>
map <silent> <C-Right> :call MoveTabRight()<CR>

" Map Control-h and Control-l
" to moving left and right through the open tabs
map <silent> <C-h> :tabp<CR>
map <silent> <C-l> :tabn<CR>

" Deal with stupid terminal.app weirdness
if $TERM_PROGRAM == "Apple_Terminal"
	map [5D <C-Left>
	map [5C <C-Right>
endif

