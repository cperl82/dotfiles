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
" Function returns a dict with 2 keys.
" label: the actual tab label
" len: length of the label contained in the key label
function! CreateTabLabelObj(n)
	let tmp = {}
	let buflist = tabpagebuflist(a:n)
	let winnr = tabpagewinnr(a:n)
	let filename = bufname(buflist[winnr - 1])
	if filename == ""
		let filename = "[No Name]"
	else
		let filename = fnamemodify(filename, ":.")
	endif
	let tmp.label = ' ' . a:n . ' ' . filename . ' '
	let tmp.len = len(tmp.label)
	if tabpagenr() == a:n
		let tmp.label = '%#TabLineSel#' . tmp.label . '%#TabLine#'
	else
		let tmp.label = '%#TabLine#' . tmp.label
	endif
	return tmp
endfunction

function! CreateTabLine()
	if ! exists("s:anchor")
		" If we don't have a tab position to anchor to the left hand
		" corner, just set it to the first tab
		let s:anchor = 1
	endif

	let curTab = tabpagenr()
	let totTab = tabpagenr('$')

	if curTab < s:anchor
		" if the current tab is less than the anchor, set the anchor
		" to the current tab.  This will happen when we move left
		let s:anchor = curTab
	endif

	" Build our tabline from anchor to the end, but make sure we dont go
	" over the width of the screen
	let tmp = BuildTabList(s:anchor, totTab)
	
	" Now check to see if our current tab made it on the list of displayed
	" tabs, and if not, move the anchor one to the right and try again
	while curTab > (s:anchor + len(tmp) - 1)
		let s:anchor = s:anchor + 1
		let tmp = BuildTabList(s:anchor, totTab)
	endwhile

	" Add a check to see if there are more tabs than we could
	" display, and if so, put a '>' at the very far right hand side
	let s:tabline = join(tmp, '|')	
	if (s:anchor + len(tmp) - 1) < totTab
		let s:tabline = s:tabline . '%=%999\>'
	endif
	" Finally, return the list of tab labels as a string separated by |
	return s:tabline
endfunction

function! BuildTabList(start, end)
	let tmp = []
	let width = 0
	let columns = &columns
	for i in range(a:start, a:end)
		" Here we are accounting for the fact that all the tab labels
		" in the tab list will be joined together with some separator
		" (we're assuming it will be one character) and therefore that
		" separator contributes to the total width
		if (i > a:start) && (i < a:end)
			let width = width + 1
		endif
		let tabObj = CreateTabLabelObj(i)
		let width = width + tabObj.len
		if width > columns
			break
		endif
		call add(tmp, tabObj.label)
	endfor
	return tmp
endfunction

" Set the tabline to our custom function
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
