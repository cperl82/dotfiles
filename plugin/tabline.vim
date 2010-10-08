" Vim global plugin for generating a different tabline than the default vim
" tabline when using vim in non gui mode.
" Maintainer: Chris Perl <chris.perl@gmail.com>

" Function CreateTabLine 
function! CreateTabLine()
	let t = s:TabLine.new()
	return t.getString()
endfunction

" Class TabLine {{{1
let s:TabLine = {}
" Function: TabLine.new {{{2
function! s:TabLine.new() dict
	" Setup initial state for the first time through
	if ! exists("self.marker")
		let self.marker = 1
	endif

	if ! exists("self.direction")
		let self.direction = s:TabString.ANCHORLEFT
	endif

	if ! exists("self.previousTabs")
		let self.previousTabs = []
	endif

	" default to our previous state
	let marker = self.marker
	let direction = self.direction
	
	" Create a new object for returning
	let obj = copy(self)

	" NOTE: The 0 index into self.tabs is invalid.  This way a tab number
	" and its index into self.tabs is the same
	let obj.tabs = []
	let obj.tabs += [ "INVALIDTABINDEX" ]
	for i in range(1, tabpagenr('$')) 
		let tab = s:Tab.new(i)
		let obj.tabs += [ tab ]
	endfor

	" Identify the selected tab
	let obj.selectedtab = tabpagenr()

	let obj.ts = s:TabString.new()
	if len(obj.tabs) == len(obj.previousTabs)
		" No tabs have been added or subtracted
		if obj.movedLeft()
			" echo "Moved Left"
			let prevStateTab = obj.previousTabs[obj.selectedtab]
			if prevStateTab.isNotDisplayed() || prevStateTab.isPartiallyDisplayed()
				let marker = obj.selectedtab
				let direction = s:TabString.ANCHORLEFT
			endif
			" If the above doesn't apply, we stick with the
			" previous state
		elseif obj.movedRight()
			" echo "Moved Right"
			let prevStateTab = obj.previousTabs[obj.selectedtab]
			if prevStateTab.isNotDisplayed() || prevStateTab.isPartiallyDisplayed()
				let marker = obj.selectedtab
				let direction = s:TabString.ANCHORRIGHT
			endif
			" If the above doesn't apply, we stick with the
			" previous state
		else
			" Just stick with the previous state
		endif
		call obj.ts.build(obj.tabs, marker, direction)
	else
		" The number of tabs has changed.  We either added a tab or
		" removed a tab.
		
		" First, just try to build the tab string with the previous
		" state
		call obj.ts.build(obj.tabs, marker, direction)
		" Now check to see that the selected tab was fully displayed
		" and if it wasnt, then we need to see what direction we moved
		" and rebuild the string
		let tab = obj.tabs[obj.selectedtab]
		if tab.isNotDisplayed() || tab.isPartiallyDisplayed()
			call obj.ts.clear()
			let marker = obj.selectedtab
			if obj.movedLeft()
				let direction = s:TabString.ANCHORLEFT
			elseif obj.movedRight()
				let direction = s:TabString.ANCHORRIGHT
			else
				throw "Looks like we created a new tab, but didn't move in any direction"
			endif
			call obj.ts.build(obj.tabs, marker, direction)
		endif
	endif
	" Save our state for the next time
	let self.direction = direction
	let self.marker = marker
	let self.previousTabs = obj.tabs
	return obj
endfunction

" Function: TabLine.getString {{{2
function! s:TabLine.getString() dict
	return self.ts.getString()
endfunction

" Function: TabLine.selectedTabFromTabs {{{2
function! s:TabLine.selectedTabFromTabs(tabs) dict
	let tabs = a:tabs
	for tabnr in range(1, len(tabs)-1)
		let tab = tabs[tabnr]
		if tab.selected
			return tabnr
		endif
	endfor
	return -1
endfunction

" Function: TabLine.movedLeft {{{2
function! s:TabLine.movedLeft() dict
	let previousSelectedTabnr = self.selectedTabFromTabs(self.previousTabs)
	if previousSelectedTabnr == -1
		return 0
	elseif self.selectedtab < previousSelectedTabnr
		return 1
	else
		return 0
	endif
endfunction

" Function: TabLine.movedRight {{{2
function! s:TabLine.movedRight() dict
	let previousSelectedTabnr = self.selectedTabFromTabs(self.previousTabs)
	if previousSelectedTabnr == -1
		return 0
	elseif self.selectedtab > previousSelectedTabnr
		return 1
	else
		return 0
	endif
endfunction

" Class TabString {{{1
let s:TabString = {}
let s:TabString.ANCHORLEFT        = 0
let s:TabString.ANCHORRIGHT       = 1
let s:TabString.FITFULL           = 10
let s:TabString.FITPART           = 11
let s:TabString.FITNONE           = 12
let s:TabString.FITFULLOUTOFSPACE = 13
" Function: TabString.new {{{2
function! s:TabString.new() dict
	let obj = copy(self)
	" Save space on either side for < or > if neccessary
	let obj.width = &columns - 2 
	let obj.remaining = obj.width
	let obj.separator = '|'
	let obj.string = ""
	let obj.pre  = " "
	let obj.post = " "
	return obj
endfunction

" Function: TabString.build {{{2
function! s:TabString.build(tabs, startnr, direction) dict
	" Build a string representation of the tab line from tabs starting at
	" startnr in direction
	if exists("self.tabs")
		throw "Build has already been called on this TabString object, call clear() before calling build again"
	endif

	let self.tabs      = a:tabs
	let self.direction = a:direction
	let self.startnr   = a:startnr

	if self.direction == s:TabString.ANCHORLEFT
		if self.startnr > 1
			call self.setMoreTabsMarkerLeft()
		endif
		let tabs = self.tabs[1:len(self.tabs)-1]
		let startidx = self.startnr - 1
	elseif self.direction == s:TabString.ANCHORRIGHT
		if self.startnr < len(self.tabs) - 1
			call self.setMoreTabsMarkerRight()
		endif
		let tabs = reverse(self.tabs[1:self.startnr])
		let startidx = 0
	else
		throw "Invalid direction given to TabString.build: " . direction
	endif

	let lastidx = len(tabs)-1
	for tabidx in range(startidx, lastidx)
		let tab = tabs[tabidx]
		let return = self.concatTab(tab)
		if return == s:TabString.FITNONE
			call tab.setNotDisplayed()
		elseif return == s:TabString.FITPART
			call tab.setPartiallyDisplayed()	
			if tabidx < lastidx
				if self.direction == s:TabString.ANCHORLEFT
					call self.setMoreTabsMarkerRight()
				else
					call self.setMoreTabsMarkerLeft()
				endif
			endif
		elseif return == s:TabString.FITFULLOUTOFSPACE
			call tab.setFullyDisplayed()
			if tabidx < lastidx
				if self.direction == s:TabString.ANCHORLEFT
					call self.setMoreTabsMarkerRight()
				else
					call self.setMoreTabsMarkerLeft()
				endif
			endif
		else
			call tab.setFullyDisplayed()
		endif
	endfor
endfunction

" Function: TabString.clear {{{2
function! s:TabString.clear() dict
	if ! exists("self.tabs")
		throw "TabString was never built, cannot be cleared"
	endif
	let self.string = ""
	let self.pre    = ""
	let self.post   = ""
	let self.remaining = self.width
	for tabnr in range(1, len(self.tabs)-1)
		let tab = self.tabs[tabnr]
		call tab.setNotDisplayed()
	endfor
	unlet self.tabs
	unlet self.direction
	unlet self.startnr
endfunction

" Function: TabString.concatTab {{{2
function! s:TabString.concatTab(tab) dict
	let tab = a:tab
	if self.string == ""
		let separator = ""
	else
		let separator = self.separator
	endif
	if self.direction == s:TabString.ANCHORLEFT
		if self.remaining == 0
			return s:TabString.FITNONE
		elseif len(tab.label) + len(separator) > self.remaining
			let tmp = strpart(tab.label, 0, self.remaining-len(separator))
			let tmp = strpart(tmp, 0, len(tmp) - 3) . "..."
			let tmp = strpart(tmp, 0, self.remaining-len(separator))
			let self.string .= separator . tab.getHighlightPre() . tmp . tab.getHighlightPost()
			let self.remaining = 0
			return s:TabString.FITPART
		else
			let self.string .= separator . tab.getLabel()
			let self.remaining -= len(tab.label) 
			let self.remaining -= len(separator)
			if self.remaining == 0
				return s:TabString.FITFULLOUTOFSPACE
			else
				return s:TabString.FITFULL
			endif
		endif
	elseif self.direction == s:TabString.ANCHORRIGHT
		if self.remaining == 0
			return s:TabString.FITNONE
		elseif len(tab.label) + len(separator) > self.remaining
			let tmp = strpart(tab.label, len(tab.label)-(self.remaining-len(separator)), len(tab.label))
			let tmp = "..." . strpart(tmp, 3, len(tmp))
			let tmp = strpart(tmp, len(tmp)-(self.remaining-len(separator)), len(tmp))
			let self.string = tab.getHighlightPre() . tmp . tab.getHighlightPost() . separator . self.string
			let self.remaining = 0
			return s:TabString.FITPART
		else
			let self.string = tab.getLabel() . separator . self.string
			let self.remaining -= len(tab.label)
			let self.remaining -= len(separator)
			if self.remaining == 0
				return s:TabString.FITFULLOUTOFSPACE
			else
				return s:TabString.FITFULL
			endif
		endif
	else
		throw "self.direction for TabString object does not appear to be properly set"
	endif
endfunction

" Function: TabString.setMoreTabsMarkerLeft {{{2
function! s:TabString.setMoreTabsMarkerLeft() dict
	let self.pre = "<"
endfunction

" Function: TabString.setMoreTabsMarkerRight {{{2
function! s:TabString.setMoreTabsMarkerRight() dict
	let self.post = ">"
endfunction

" Function: TabString.getString {{{2
function! s:TabString.getString() dict
	return '%#Tabline#' . self.pre . self.string . self.post
endfunction

" Class Tab {{{1
let s:Tab = {}
let s:Tab.DISPLAYNONE = 0
let s:Tab.DISPLAYPART = 1
let s:Tab.DISPLAYFULL = 2
" Function: Tab.new {{{2
function! s:Tab.new(number) dict
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
	" Initially the tab is not displayed
	let obj.displayed = s:Tab.DISPLAYNONE
	" Initially we are not marked as either the first or last tab
	let obj.firsttab  = 0
	let obj.lasttab   = 0
	return obj
endfunction

" Function: Tab.setNotDisplayed {{{2
function! s:Tab.setNotDisplayed() dict
	let self.displayed = s:Tab.DISPLAYNONE
endfunction

" Function: Tab.setPartiallyDisplayed {{{2
function! s:Tab.setPartiallyDisplayed() dict
	let self.displayed = s:Tab.DISPLAYPART
endfunction

" Function: Tab.setFullyDisplayed {{{2
function! s:Tab.setFullyDisplayed() dict
	let self.displayed = s:Tab.DISPLAYFULL
endfunction

" Function: Tab.isNotDisplayed {{{2
function! s:Tab.isNotDisplayed() dict
	if self.displayed == s:Tab.DISPLAYNONE
		return 1
	else
		return 0
	endif
endfunction

" Function: Tab.isPartiallyDisplayed {{{2
function! s:Tab.isPartiallyDisplayed() dict
	if self.displayed == s:Tab.DISPLAYPART
		return 1
	else
		return 0
	endif
endfunction

" Function: Tab.isFullyDisplayed {{{2
function! s:Tab.isFullyDisplayed() dict
	if self.displayed == s:Tab.DISPLAYFULL
		return 1
	else
		return 0
	endif
endfunction

" Function: Tab.getLabel {{{2
function! s:Tab.getLabel() dict
	return self.highlightPre . self.label . self.highlightPost
endfunction

" Function: Tab.getHighlightPre {{{2
function! s:Tab.getHighlightPre() dict
	return self.highlightPre
endfunction

" Function: Tab.getHighlightPost {{{2
function! s:Tab.getHighlightPost() dict
	return self.highlightPost
endfunction

