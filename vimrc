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

" Messing around with OO for tabline
" Function TestTabLine
function! TestTabLine()
	let t = g:TabLine.new()
	return t
endfunction

" Class TabLine
let g:TabLine = {}
function! g:TabLine.new() dict
	" Do everything to build our current state
	let obj = copy(self)
	" Note that index into self.tabs is tabnr - 1
	let obj.tabs = []
	for i in range(1, tabpagenr('$')) 
		let tab = g:Tab.new(i)
		let obj.tabs += [ tab ]
	endfor
	" Figure out the width we have to work with
	let obj.screenwidth = &columns
	" Set the separator we're using
	let obj.labelseparator = '|'

	" Identified the selected tab
	" selectedtab is the current tab page number
	" selectedtabidx is the index into obj.tabs for the selected tab
	let obj.selectedtab = tabpagenr()

	return obj
endfunction

" Class Tab
let g:Tab = {}
function! g:Tab.new(number) dict
	let obj = copy(self)
	let obj.number = a:number
	let buflist    = tabpagebuflist(obj.number)
	let winnr      = tabpagewinnr(obj.number)
	let buffer     = buflist[winnr - 1]
	let name       = bufname(buffer)

	" Set the name
	if name == ""
		let obj.name = "[No Name]"
	else
		let obj.name = fnamemodify(name, ":.")
	endif

	" Determine if the buffer in the active window of
	" this tab is modified
	let obj.modified = getbufvar(buffer, "&modified")

	" Set the label
	if obj.modified
		let obj.label = "+" . obj.number . " " . obj.name . " "
	else
		let obj.label = " " . obj.number . " " . obj.name . " "
	endif

	" Set the display label, can be modified by client code
	let obj.displaylabel = obj.label

	" Determine if we are the selected tab or not
	if obj.number == tabpagenr()
		let obj.selected = 1
	else
		let obj.selected = 0 
	endif
	return obj
endfunction

function! g:Tab.getDisplayLabel() dict
	if self.selected
		let pre  = '%#TabLineSel#'
		let post = '%#TabLine#'
	else
		let pre  = ""
		let post = ""
	endif
	return pre . self.displaylabel . post
endfunction

function! g:Tab.setDisplayLabel(str) dict
	let self.displaylabel = a:str
endfunction




" Function to create a tab object that represents a tagpage
function! CreateTabObj(n)
	let tabObj   = {}
	let buflist  = tabpagebuflist(a:n)
	let winnr    = tabpagewinnr(a:n)
	let buffer   = buflist[winnr - 1]
	let filename = bufname(buffer)
	if filename == ""
		let filename = "[No Name]"
	else
		let filename = fnamemodify(filename, ":.")
	endif
	let modified = getbufvar(buffer, "&modified")
	if modified
		let pre = " +"
	else
		let pre = " "
	endif
	let tabObj.label = pre . a:n . ' ' . filename . ' '
	let tabObj.labelLen = len(tabObj.label)
	let tabObj.sep = '|'
	let tabObj.sepLen = len(tabObj.sep)
	let tabObj.partial = 0
	" If this is the current tab, set the highlight to make it selected
	if tabpagenr() == a:n
		let tabObj.labelHighlight = '%#TabLineSel#'
		let tabObj.sepHighlight =   '%#TabLine#'
		let tabObj.resetHighlight = '%#TabLine#'
		let tabObj.current = 1
	else
		let tabObj.labelHighlight = '%#TabLine#'
		let tabObj.sepHighlight =   '%#TabLine#'
		let tabObj.resetHighlight = '%#TabLine#'
		let tabObj.current = 0
	endif
	return tabObj
endfunction

function! CreateTabLine()
	if ! exists("s:anchor")
		" If we don't have a tab position to anchor to the left hand
		" corner, just set it to the first tab
		let s:anchor = 1
	endif

	let s:curTab = tabpagenr()
	let s:totTab = tabpagenr('$')

	if s:curTab < s:anchor
		" if the current tab is less than the anchor, set the anchor
		" to the current tab.  This will happen when we move left
		let s:anchor = s:curTab
	endif

	" Build our tabline from anchor to the end, but make sure we dont go
	" over the width of the screen
	let s:tmp = BuildTabList(s:anchor, s:totTab)
	
	" Now check to see if our current tab made it on the list of displayed
	" tabs, and if not, move the anchor one to the right and try again
	" OR
	" Check that the final tab displayed is not current AND partial,
	" and if it is, move the anchor once more
	while (s:curTab > (s:anchor + len(s:tmp) - 1)) || (s:tmp[-1].current && s:tmp[-1].partial)
		let s:anchor = s:anchor + 1
		let s:tmp = BuildTabList(s:anchor, s:totTab)
	endwhile

	" Add a check to see if there are more tabs than we could
	" display, and if so, put a '>' at the very far right hand side
	let s:tabline = ''
	for s:tabObj in s:tmp
		let s:tabline = s:tabline . s:tabObj.sepHighlight . s:tabObj.sep . s:tabObj.labelHighlight . s:tabObj.label . s:tabObj.resetHighlight
	endfor
	if (s:anchor + len(s:tmp) - 1) < s:totTab
		let s:tabline = s:tabline . '%=%999\>'
	endif
	" Finally, return the list of tab labels as a string separated by |
	return s:tabline
endfunction

function! BuildTabList(start, end)
	let s:tabList = []
	let s:width = 0
	" set the number of columns we have to work with to the number of
	" columns available minus 1, to account for the potential '>' added if
	" there are more tabs than can fit
	let s:columns = &columns - 1
	for i in range(a:start, a:end)
		let s:tabObj = CreateTabObj(i)
		" We don't want a separator for the first tab in the tab list
		if i == a:start
			let s:tabObj.sep = ''
		endif	
		let s:width = s:width + s:tabObj.sepLen + s:tabObj.labelLen
		if s:width > s:columns
			let s:overage = s:width - s:columns
			let s:room = s:tabObj.labelLen - s:overage - 1
			if s:room > 0
				let s:tabObj.label = strpart(s:tabObj.label, 0, s:room-3) . '...'
				let s:tabObj.partial = 1
				call add(s:tabList, s:tabObj)
			endif
			break
		endif
		call add(s:tabList, s:tabObj)
	endfor
	return s:tabList
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
let g:python_highlight_all = 1
let g:python_highlight_ident_errors = 0
