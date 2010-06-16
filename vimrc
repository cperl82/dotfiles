" Enable file type detection, all three modes
" detection, indent, plugin.  See :help filetype
filetype plugin indent on

" Turn on syntax highlighting, using defaults
" See :help syntax
syntax on


set laststatus=2
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set autoindent
set number
set numberwidth=4
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

function! CreateTabLine()
	return CreateTabLineRange(1, tabpagenr('$'))
endfunction

" Function that takes parameters i, j for the start and
" end tab to display
function! CreateTabLineRange(j, k)
	let tabDict = {}
	for i in range(a:j, a:k)
		let s = ' ' . i . ' %' . i . 'T' . '%{MyTabLabel(' . i . ')}' . ' '
		let tabDict[i] = s
		if i == tabpagenr()
			let tabDict[i] = '%#TabLineSel#' . tabDict[i] . '%#TabLine#'
		endif
	endfor
	let tabList = []
	for key in sort(keys(tabDict))
		call add(tabList, tabDict[key])
	endfor
	return '%#TabLine#' . join(tabList, '|')
endfunction
set tabline=%!CreateTabLine()

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
