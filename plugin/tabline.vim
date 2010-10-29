" Vim global plugin for generating a different tabline than the default vim
" tabline when using vim in non gui mode.
" Maintainer: Chris Perl <chris.perl@gmail.com>

" Function: DrawTabLine 
function! DrawTabLine()
	if ! exists("g:cptl")
		let g:cptl = s:TabLine.new()
	endif
	call g:cptl.update()
	return g:cptl.getString()
endfunction

" Class TabLine {{{1
let s:TabLine = {}
" Function: TabLine.new {{{2
function! s:TabLine.new() dict
	" Create a new object for returning
	let obj = copy(self)

	" Set the prior state to invalid since we are just being
	" instantiated and have no prior state
	let obj.priorState = { "VALID": 0 }

	" Set a marker to indicate that we've just been created
	let obj.JUSTCREATED = 1
	
	" Instantiate a TabString object for creating the actual string that
	" is the tabline
	let obj.ts = s:TabString.new()
	return obj
endfunction

" Function: TabLine.update {{{2
function s:TabLine.update() dict
	if self.JUSTCREATED
		let self.JUSTCREATED = 0
		let self.marker = 1
		let self.direction = s:TabString.ANCHORLEFT
		let self.selectedtab = tabpagenr()
		call self.initTabs()
		call self.ts.build(self.tabs, self.marker, self.direction)
		return
	endif

	" We're updating, so save our current state
	call self.saveState()

	" Initialize the current set of tabs
	call self.initTabs()

	" Celar the TabString object for redrawing
	call self.ts.clear()

	" Identify the selected tab
	let self.selectedtab = tabpagenr()

	if len(self.tabs) == len(self.priorState.tabs)
		" No tabs have been added or subtracted
		call self.updateEqualTabs()
	elseif len(self.tabs) > len(self.priorState.tabs)
		" Added a new Tab
		call self.updateMoreTabs()
	else 
		" len(self.tabs) < len(self.priorState.tabs)
		call self.updateLessTabs()
	endif
endfunction

" Function: TabLine.updateEqualTabs {{{2
function s:TabLine.updateEqualTabs()
	if self.movedLeft()
		let tab = self.priorState.tabs[self.selectedtab]
		if ! tab.isFullyDisplayed()
			let self.marker = self.selectedtab
			let self.direction = s:TabString.ANCHORLEFT
		endif
		call self.ts.build(self.tabs, self.marker, self.direction)
	elseif self.movedRight()
		let tab = self.priorState.tabs[self.selectedtab]
		if ! tab.isFullyDisplayed()
			let self.marker = self.selectedtab
			let self.direction = s:TabString.ANCHORRIGHT
		endif
		call self.ts.build(self.tabs, self.marker, self.direction)
	else
		" TODO: This section needs more work.  The problems are due to
		" window resizing.  I try to check that things are good after
		" the first call with the prior state, but it doesn't always
		" work out perfect.
		
		" Just stick with the previous state
		let self.marker = self.priorState.marker
		let self.direction = self.priorState.direction
		" Try to build our tabline
		call self.ts.build(self.tabs, self.marker, self.direction)

		" Check it.  It could be jacked up due to window resizing
		let tab = self.tabs[self.selectedtab]
		if ! tab.isFullyDisplayed()
			let self.marker = self.selectedtab
			call self.switchDirection()
			call self.ts.clear()
			call self.ts.build(self.tabs, self.marker, self.direction)
		elseif (! self.ts.isFull()) && (self.direction == s:TabString.ANCHORRIGHT)
			let self.marker = 1
			let self.direction = s:TabString.ANCHORLEFT
			call self.ts.clear()
			call self.ts.build(self.tabs, self.marker, self.direction)
		endif
	endif
endfunction

" Function: TabLine.updateMoreTabs {{{2
function s:TabLine.updateMoreTabs()
	let tab = self.tabs[self.selectedtab]
	if self.direction == s:TabString.ANCHORLEFT
		" Attempt to build w/ old marker and direction
		call self.ts.build(self.tabs, self.marker, self.direction)
		if ! tab.isFullyDisplayed()
			if self.movedLeft()
				let self.marker = self.selectedtab
				call self.ts.clear()
				call self.ts.build(self.tabs, self.marker, self.direction)
			elseif self.movedRight()
				let self.marker = self.selectedtab
				let self.direction = s:TabString.ANCHORRIGHT
				call self.ts.clear()
				call self.ts.build(self.tabs, self.marker, self.direction)
			else
				throw "Tab added, but unable to determine if we moved left or right.  Currently ANCHORLEFT"
			endif
		endif
	elseif self.direction == s:TabString.ANCHORRIGHT
		" If we were anchored right, before building we need to check
		" the direction since a move left would mean that the new tab
		" was created before our prior selected tab, and would mean we
		" have to increment marker by one
		if self.movedLeft()
			let self.marker = self.marker + 1
			call self.ts.build(self.tabs, self.marker, self.direction)
			if ! tab.isFullyDisplayed()
				let self.marker = self.selectedtab
				let self.direction = s:TabString.ANCHORLEFT
				call self.ts.clear()
				call self.ts.build(self.tabs, self.marker, self.direction)
			endif
		elseif self.movedRight()
			call self.ts.build(self.tabs, self.marker, self.direction)
			if ! tab.isFullyDisplayed()
				let self.marker = self.selectedtab
				call self.ts.clear()
				call self.ts.build(self.tabs, self.marker, self.direction)
			endif
		else
			throw "Tab added, but unable to determine if we moved left or right.  Currently ANCHORRIGHT"
		endif
	else
		throw "TabLine does not appear to have a valid direction set: " . direction
	endif
endfunction

" Function: TabLine.updateLessTabs {{{2
function s:TabLine.updateLessTabs()
	" First attempt to build w/ our prior state
	let tab = self.tabs[self.selectedtab]
	call self.ts.build(self.tabs, self.marker, self.direction)
	if (! self.ts.isFull()) && (self.direction == s:TabString.ANCHORRIGHT)
		let self.marker = 1
		let self.direction = s:TabString.ANCHORLEFT
		call self.ts.clear()
		call self.ts.build(self.tabs, self.marker, self.direction)
	endif
endfunction

" Function: TabLine.saveState {{{2
function s:TabLine.saveState()
	unlet self.priorState
	let self.priorState = { "VALID": 1 }
	let self.priorState.tabs = deepcopy(self.tabs)
	let self.priorState.marker = copy(self.marker)
	let self.priorState.direction = copy(self.direction)
	let self.priorState.selectedtab = copy(self.selectedtab)
endfunction

" Function: TabLine.initTabs {{{2
function s:TabLine.initTabs()
	" NOTE: The 0 index into self.tabs is invalid.  This way a tab number
	" and its index into self.tabs is the same
	let self.tabs = []
	let self.tabs += [ "INVALIDTABINDEX" ]
	for i in range(1, tabpagenr('$')) 
		let tab = s:Tab.new(i)
		let self.tabs += [ tab ]
	endfor
endfunction

" Function: TabLine.getString {{{2
function! s:TabLine.getString() dict
	return self.ts.getString()
endfunction

" Function: TabLine.movedLeft {{{2
function! s:TabLine.movedLeft() dict
	" Checking for left movement is a little tricky because the selected
	" tab number could be the same if we added a tab before the previously
	" selected tab.
	if len(self.tabs) <= len(self.priorState.tabs)
		return self.selectedtab <  self.priorState.selectedtab ? 1 : 0
	else
		return self.selectedtab <= self.priorState.selectedtab ? 1 : 0
	endif
endfunction

" Function: TabLine.movedRight {{{2
function! s:TabLine.movedRight() dict
	 return self.selectedtab > self.priorState.selectedtab ? 1 : 0
endfunction

" Function: TabLine.switchDirection {{{2
function! s:TabLine.switchDirection() dict
	if self.direction == s:TabString.ANCHORLEFT
		let self.direction = s:TabString.ANCHORRIGHT
	else
		let self.direction = s:TabString.ANCHORLEFT
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
	let self.pre    = " "
	let self.post   = " "
	" We need to recalculate the width when we clear in case the window
	" has changed size
	let self.width  = &columns - 2
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

" Function: TabString.isFull {{{2
function! s:TabString.isFull() dict
	return self.remaining == 0 ? 1 : 0
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
		let obj.name = fnamemodify(name, ":~:.")
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

" vim: fdm=marker
