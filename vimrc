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
	return t.getString()
endfunction

" Class TabLine
let g:TabLine = {}
function! g:TabLine.new() dict
	
	" Create a new object for returning
	let obj = copy(self)

	" Note that index into self.tabs is tabnr - 1
	let obj.tabs = []
	for i in range(1, tabpagenr('$')) 
		let tab = g:Tab.new(i)
		let obj.tabs += [ tab ]
	endfor
	" Identify the selected tab
	let obj.selectedtab = tabpagenr()

	" Initialize the tab number used for anchoring.
	let obj.anchor  = 1
	
	" Testing
	let obj.ts = g:TabString.new()
	call obj.ts.setAnchor(g:TabString.ANCHORLEFT)
	" Here we need to determine if we moved left or right and other rules
	" for changing the anchoring.  If we change from left to right, call
	" setAnchor and reverse self.tabs before calling build.  If we change
	" from right to left, setAnchor to the left and leave self.tabs alone
	call obj.build(obj.anchor)
	echo "Anchor is " . obj.anchor
	return obj
endfunction

function! g:TabLine.getString() dict
	return self.ts.getString()
endfunction

function! g:TabLine.build(startnr) dict
	" Build the tab string from tab number startnr.  Keep going until the
	" selected tab is fully displayed
	let startnr  = a:startnr
	let startidx = startnr - 1
	let endidx   = len(self.tabs) - 1
	let curridx  = startidx
	let stidx    = self.selectedtab - 1
	call self.ts.clear()
	while curridx <= endidx
		let tab = self.tabs[curridx]
		let return = self.ts.concatTab(tab)
		if return == 0 || return == 1
			" TabString is full
			if stidx >= curridx
				call self.ts.clear()
				let startidx += 1
				let curridx = startidx
				continue
			else
				if curridx != endidx
					call self.ts.setMoreTabsMarker()
				endif
				break
			endif
		endif
		let curridx += 1
	endwhile
	let anchornr = startidx + 1
	let self.anchor = anchornr
endfunction

" Class TabString
let g:TabString = {}
let g:TabString.ANCHORNONE  = 0
let g:TabString.ANCHORLEFT  = 1
let g:TabString.ANCHORRIGHT = 2

function! g:TabString.new() dict
	let obj = copy(self)
	" Save space on either side for < or > if neccessary
	let obj.width = &columns - 1 
	let obj.remaining = obj.width
	let obj.separator = '|'
	let obj.anchor = g:TabString.ANCHORNONE
	let obj.string = ""
	return obj
endfunction

function! g:TabString.setAnchor(x) dict
	let self.anchor = a:x
endfunction

function! g:TabString.clear() dict
	let self.string = ""
	let self.remaining = self.width
endfunction

function! g:TabString.clearAndAnchor(x) dict
	call self.clear()
	call self.setAnchor(a:x)
endfunction

function! g:TabString.concatTab(tab) dict
	let tab = a:tab
	if self.string == ""
		let separator = ""
	else
		let separator = self.separator
	endif
	if self.anchor == g:TabString.ANCHORLEFT
		if self.remaining == 0
			return 0
		elseif len(tab.label) + len(separator) > self.remaining
			let tmp = strpart(tab.label, 0, self.remaining-len(separator))
			let tmp = strpart(tmp, 0, len(tmp) - 3) . "..."
			let tmp = strpart(tmp, 0, self.remaining-len(separator))
			let self.string .= separator . tab.getHighlightPre() . tmp . tab.getHighlightPost()
			let self.remaining = 0
			return 1
		else
			let self.string .= separator . tab.getLabel()
			let self.remaining -= len(tab.label) 
			let self.remaining -= len(separator)
			return 2
		endif
	elseif self.anchor == g:TabString.ANCHORRIGHT
		if self.remaining == 0
			return 0
		elseif len(tab.label) + len(separator) > self.remaining
			let tmp = strpart(tab.label, len(tab.label)-(self.remaining-len(separator)), len(tab.label))
			let tmp = "..." . strpart(tmp, 3, len(tmp))
			" Add fix like you have above for cases were
			" self.remaining is a very low number like 1, 2 or 3
			let self.string = tab.getHighlightPre() . tmp . tab.getHighlightPost() . separator . self.string
			let self.remaining = 0
			return 1
		else
			let self.string = tab.getLabel() . separator . self.string
			let self.remaining -= len(tab.label)
			let self.remaining -= len(separator)
			return 2
		endif
	else
		throw "You cannot call concatTab without having anchored the TabString object"
endfunction

function! g:TabString.setMoreTabsMarker() dict
	if self.anchor == g:TabString.ANCHORLEFT
		let self.string .= '>'
	elseif self.anchor == g:TabString.ANCHORRIGHT
		let self.string = '<' . self.string
	else
		throw "You cannot call setMoreTabsMarker without having anchored the TabString object"
	endif
endfunction

function! g:TabString.getString() dict
	return '%#Tabline#' . self.string
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
		let obj.label = " +" . obj.number . " " . obj.name . " "
	else
		let obj.label =  " " . obj.number . " " . obj.name . " "
	endif

	" Determine if we are the selected tab or not
	if obj.number == tabpagenr()
		let obj.selected = 1
	else
		let obj.selected = 0 
	endif

	" Setup the highligh pre and post
	if obj.selected
		let obj.highlightPre  = '%#TabLineSel#'
		let obj.highlightPost = '%#TabLine#'
	else
		let obj.highlightPre  = ""
		let obj.highlightPost = ""
	endif
	return obj
endfunction

function! g:Tab.getLabel() dict
	return self.highlightPre . self.label . self.highlightPost
endfunction

function! g:Tab.getHighlightPre() dict
	return self.highlightPre
endfunction

function! g:Tab.getHighlightPost() dict
	return self.highlightPost
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
