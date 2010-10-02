" Messing around with OO for tabline
" Function TestTabLine
function! TestTabLine()
	let t = g:TabLine.new()
	return t.getString()
endfunction

" Class TabLine
let g:TabLine = {}
let g:TabLine.BUILDFORWARD = 10
let g:TabLine.BUILDREVERSE = 11
function! g:TabLine.new() dict
	" Setup initial state for the first time through
	if ! exists("self.marker")
		let self.marker = 1
	endif

	if ! exists("self.direction")
		let self.direction = g:TabLine.BUILDFORWARD
	endif

	if ! exists("self.previousTabs")
		let self.previousTabs = []
	endif

	" default to our previous state
	let marker = self.marker
	let direction = self.direction
	
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

	" Here we need to determine if we moved left or right and other rules
	" for changing the anchoring.  
	let movedLeft  = obj.movedLeft()
	let movedRight = obj.movedRight()
	if movedLeft
		echo "Moved Left"
		let stidx = obj.selectedtab - 1
		let dt = obj.displayTypeFromTabIdxInTabs(stidx, self.previousTabs)
		if (dt == g:Tab.DISPLAYNONE) || (dt == g:Tab.DISPLAYPART)
			let marker = obj.selectedtab
			let direction = g:TabLine.BUILDFORWARD
			" echo "Changing build direction: Now LEFT to RIGHT"
		elseif dt == g:Tab.NOTFOUND
			" stidx is outside the range of our previous tabs
		" This logic is not correct
		"	let tab = self.previousTabs[stidx+1]
		"	if tab.firsttab
		"		let marker = obj.selectedtab
		"		let direction = g:TabLine.BUILDFORWARD
		"	endif
		endif
	elseif movedRight
		echo "Moved Right"
		let stidx = obj.selectedtab - 1
		let dt = obj.displayTypeFromTabIdxInTabs(stidx, self.previousTabs)
		if (dt == g:Tab.DISPLAYNONE) || (dt == g:Tab.DISPLAYPART)
			let marker = obj.selectedtab
			let direction = g:TabLine.BUILDREVERSE
			" echo "Changing build direction: Now RIGHT to LEFT"
		elseif dt == g:Tab.NOTFOUND
			" stidx is outside the range of our previous tabs
		" This logic is not correct
		"	let tab = self.previousTabs[stidx-1]
		"	if tab.lasttab
		"		let marker = obj.selectedtab
		"		let direction = g:TabLine.BUILDREVERSE
		"	endif
		endif
	endif
	call obj.build(direction, marker)
	" Save our state for the next time
	let self.direction = direction
	let self.marker = marker
	let self.previousTabs = obj.tabs
	return obj
endfunction

function! g:TabLine.getString() dict
	return self.ts.getString()
endfunction

function! g:TabLine.build(direction, startnr) dict
	" Build the tab string from tab number startnr in direction direction.  
	" Keep going until the selected tab is fully displayed
	let direction = a:direction
	let startnr   = a:startnr

	let  self.ts = g:TabString.new()
	call self.ts.clear()

	if direction == g:TabLine.BUILDFORWARD
		call self.ts.setAnchor(g:TabString.ANCHORLEFT)
		let tabs      = self.tabs
		let startidx  = startnr - 1
		let curridx   = startidx
		let stidx     = self.selectedtab - 1
		if startnr > 1
			call self.ts.setMoreTabsMarkerLeft()
		endif
	else
		call self.ts.setAnchor(g:TabString.ANCHORRIGHT)
		let tabs     = reverse(self.tabs[0:(startnr-1)])
		let startidx = 0
		let curridx  = startidx
		let stidx    = (len(tabs) - 1) - (self.selectedtab - 1)
		if startnr < len(self.tabs)
			call self.ts.setMoreTabsMarkerRight()
		endif
	endif
	let endidx = len(tabs) - 1

	while curridx <= endidx
		let tab = tabs[curridx]
		let return = self.ts.concatTab(tab)
		if return == g:TabString.FITNONE || return == g:TabString.FITPART || return == g:TabString.FITFULLOUTOFSPACE
			" TabString is full
			if stidx > curridx
				call self.ts.clear()
				let startidx += 1
				let curridx = startidx
				continue
			else
				if curridx < endidx
					if direction == g:TabLine.BUILDFORWARD
						call self.ts.setMoreTabsMarkerRight()
					else
						call self.ts.setMoreTabsMarkerLeft()
					endif
				endif
				break
			endif
		endif
		let curridx += 1
	endwhile
	if direction == g:TabLine.BUILDFORWARD
		let self.tabs[startidx].firsttab = 1
		let self.tabs[curridx].lasttab   = 1
	else
		let self.tabs[curridx].firsttab = 1
		let self.tabs[startidx].lasttab = 1
	endif
endfunction

function! g:TabLine.displayTypeFromTabIdxInTabs(idx, tabs) dict
	let idx  = a:idx
	let tabs = a:tabs
	if idx > len(tabs)-1 || idx < 0
		return g:Tab.NOTFOUND
	else
		return tabs[idx].displayed
	endif	
endfunction

function! g:TabLine.selectedTabIdxFromTabs(tabs) dict
	let tabs = a:tabs
	for tabidx in range(0, len(tabs)-1)
		let tab = tabs[tabidx]
		if tab.selected
			return tabidx
		endif
	endfor
	return -1
endfunction

function! g:TabLine.movedLeft() dict
	let stidx = self.selectedtab - 1
	let idx = self.selectedTabIdxFromTabs(self.previousTabs)
	if idx == -1
		return 0
	elseif stidx < idx
		return 1
	else
		return 0
	endif
endfunction

function! g:TabLine.movedRight() dict
	let stidx = self.selectedtab - 1
	let idx = self.selectedTabIdxFromTabs(self.previousTabs)
	if idx == -1
		return 0
	elseif stidx > idx
		return 1
	else
		return 0
	endif
endfunction

" Class TabString
let g:TabString = {}
let g:TabString.ANCHORNONE        = 0
let g:TabString.ANCHORLEFT        = 1
let g:TabString.ANCHORRIGHT       = 2
let g:TabString.FITFULL           = 10
let g:TabString.FITPART           = 11
let g:TabString.FITNONE           = 12
let g:TabString.FITFULLOUTOFSPACE = 13
function! g:TabString.new() dict
	let obj = copy(self)
	" Save space on either side for < or > if neccessary
	let obj.width = &columns - 2 
	let obj.remaining = obj.width
	let obj.separator = '|'
	let obj.anchor = g:TabString.ANCHORNONE
	let obj.string = ""
	let obj.pre  = " "
	let obj.post = " "
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
			return g:TabString.FITNONE
		elseif len(tab.label) + len(separator) > self.remaining
			let tmp = strpart(tab.label, 0, self.remaining-len(separator))
			let tmp = strpart(tmp, 0, len(tmp) - 3) . "..."
			let tmp = strpart(tmp, 0, self.remaining-len(separator))
			let self.string .= separator . tab.getHighlightPre() . tmp . tab.getHighlightPost()
			let self.remaining = 0
			call tab.setDisplayed(g:Tab.DISPLAYPART)
			return g:TabString.FITPART
		else
			let self.string .= separator . tab.getLabel()
			let self.remaining -= len(tab.label) 
			let self.remaining -= len(separator)
			call tab.setDisplayed(g:Tab.DISPLAYFULL)
			if self.remaining == 0
				return g:TabString.FITFULLOUTOFSPACE
			else
				return g:TabString.FITFULL
			endif
		endif
	elseif self.anchor == g:TabString.ANCHORRIGHT
		if self.remaining == 0
			return g:TabString.FITNONE
		elseif len(tab.label) + len(separator) > self.remaining
			let tmp = strpart(tab.label, len(tab.label)-(self.remaining-len(separator)), len(tab.label))
			let tmp = "..." . strpart(tmp, 3, len(tmp))
			let tmp = strpart(tmp, len(tmp)-(self.remaining-len(separator)), len(tmp))
			let self.string = tab.getHighlightPre() . tmp . tab.getHighlightPost() . separator . self.string
			let self.remaining = 0
			call tab.setDisplayed(g:Tab.DISPLAYPART)
			return g:TabString.FITPART
		else
			let self.string = tab.getLabel() . separator . self.string
			let self.remaining -= len(tab.label)
			let self.remaining -= len(separator)
			call tab.setDisplayed(g:Tab.DISPLAYFULL)
			if self.remaining == 0
				return g:TabString.FITFULLOUTOFSPACE
			else
				return g:TabString.FITFULL
			endif
		endif
	else
		throw "You cannot call concatTab without having anchored the TabString object"
endfunction

function! g:TabString.setMoreTabsMarkerLeft() dict
	let self.pre = "<"
endfunction

function! g:TabString.setMoreTabsMarkerRight() dict
	let self.post = ">"
endfunction

function! g:TabString.getString() dict
	return '%#Tabline#' . self.pre . self.string . self.post
endfunction

" Class Tab
let g:Tab = {}
let g:Tab.DISPLAYNONE = 0
let g:Tab.DISPLAYPART = 1
let g:Tab.DISPLAYFULL = 2
let g:Tab.NOTFOUND    = 10
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
	" Initially the tab is not displayed
	let obj.displayed = g:Tab.DISPLAYNONE
	" Initially we are not marked as either the first or last tab
	let obj.firsttab  = 0
	let obj.lasttab   = 0
	return obj
endfunction

function! g:Tab.setDisplayed(x) dict
	let self.displayed = a:x
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



