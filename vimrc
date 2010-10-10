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

" Set the tabline to our custom function
set tabline=%!DrawTabLine()

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
nmap <silent> <C-Left> :call MoveTabLeft()<CR>
nmap <silent> <C-Right> :call MoveTabRight()<CR>

" Map Control-h and Control-l
" to moving left and right through the open tabs
nmap <silent> <C-h> :tabp<CR>
nmap <silent> <C-l> :tabn<CR>

" Map Alt-1 (at least on my mac) such that it opens the quick fix window
" and then prepares a vimgrep for me w/o jumping to the first
" match it finds
:nmap <ESC>1 :copen<CR>:vimgrep ##j **/*<Left><Left><Left><Left><Left><Left><Left>

" 2010-09-24
" Added newer python syntax highlighting script
" Enable all the syntax options in it (.vim/syntax/python.vim)
let python_highlight_all = 1
let python_highlight_indent_errors = 0
let python_highlight_space_errors = 0

" 2010-10-03
" Make shortcuts for jumping directly to a specific tab
" We're mapping <Leader> (which defaults to \) followed by {number} (where
" number is in the range 1-9) to jump to that numbered tab
for i in range(1,9)
	execute "nmap <silent> <Leader>" . i . " :tabnext " . i . "<CR>"
endfor
