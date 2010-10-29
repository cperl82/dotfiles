" Intro blurb blah blah
" Maintainer: chris.perl@gmail.com

" Class: FuzzyFinder {{{1
let s:FuzzyFinder = {}
" Function: FuzzyFinder.new {{{2
function! s:FuzzyFinder.new() dict
	let obj = copy(self)
	let obj.prompt = "File >>> "
	let obj.selectedFile = ""
	let obj.walker = ""
	return obj
endfunction

" Function: FuzzyFinder.search {{{2
function! s:FuzzyFinder.search() dict
	" Reset the selected file
	let self.selectedFile = ""

	" Get a new walker
	let self.walker = s:NERDTreeWalker.new(b:NERDTreeRoot)

	" Initially we set the node to jump to when we are done to the
	" previously selected node in the tree
	let self.jumpNode = g:NERDTreeFileNode.GetSelected()
	if self.jumpNode == {}
		let self.jumpNode = b:NERDTreeRoot
	endif

	setlocal modifiable
	setlocal completeopt=menuone
	setlocal completefunc=Complete
	autocmd InsertLeave  <buffer> call Finish()
	autocmd CursorMovedI <buffer> call OnCursorMovedI()

	" TODO: Explain how this works, its kinda f'in confusing
	inoremap <buffer> <silent> <CR> <C-y><C-r>=FuzzyFinder.saveSelection() ? "" : ""<CR><ESC>

	"call setpos(".", [0, 2, len(self.prompt), 0])
	"call setline(2, self.prompt)
	call setpos(".", [0, line("w0"), len(self.prompt), 0])
	call setline(line("w0"), self.prompt)
	call feedkeys("A", 'n')
	call feedkeys("\<C-x>\<C-u>", 'n')
endfunction

" Function: FuzzyFinder.saveSelection {{{2
function! s:FuzzyFinder.saveSelection() dict
	let self.selectedFile = self.stripPrompt(getline("."))
endfunction

" Function: FuzzyFinder.stripPrompt {{{2
function! s:FuzzyFinder.stripPrompt(line) dict
	return substitute(a:line, self.prompt, "", "")
endfunction

" Function: FuzzyFinder.complete {{{2
function! s:FuzzyFinder.complete(findstart, base) dict
	if a:findstart == 1
	    return len(self.prompt)
	else
		call self.walker.reset()
		if a:base == ""
			return []
		else
			call complete_add(a:base)
			while self.walker.hasNext()
				let node = self.walker.next()
				let path = node.path.str()
				if path =~ a:base
					call complete_add({'word': path, 'abbr': fnamemodify(path, ":.")})
				endif
				if complete_check()
					break
				endif
			endwhile
			return []
		endif
	endif
endfunction

" Function: FuzzyFinder.finish {{{2
function! s:FuzzyFinder.finish() dict
	setlocal completefunc&
	setlocal completeopt&
	setlocal nomodifiable
	" Cant figure this out at the moment, keeps raising errors
	" iunmap <buffer> <CR>
	if self.selectedFile != ""
		echo "Handling selected file " . self.selectedFile
		" This is tricky because the NERDTree isnt necessarily open
		" call self.handleSelectedFile()
	endif
	call self.jumpNode.putCursorHere(0, 0)
	call NERDTreeRender()
endfunction

" Function: FuzzyFinder.handleSelectedFile {{{2
function! s:FuzzyFinder.handleSelectedFile()
	let path = g:NERDTreePath.New(self.selectedFile)
	echo path.str()
	let node = b:NERDTreeRoot.findNode(path)
	if node != {}
		let self.jumpNode = node
	endif
endfunction

" Function: FuzzyFinder.onCursorMovedI {{{2
function! s:FuzzyFinder.onCursorMovedI()
	" TODO: Check for correct positioning of the cursor, like fuf
	call feedkeys("\<C-x>\<C-u>", 'n')
endfunction

" Class: NERDTreeWalker {{{1
let s:NERDTreeWalker = {}
" Function: NERDTreeWalker.new {{{2
function! s:NERDTreeWalker.new(root) dict
	" TODO: Walking the tree has to be smarter.  Instead of opening the
	" entire tree and flattening it into a list on instantiation, this
	" object should yield a node when .next() is called, and only if
	" necessary should the next() method open directories as it walks the
	" tree.  This should make our use from FuzzyFinder coupled with
	" FuzzyFinder.complete's use of complete_add and complete_check much
	" more reasonable for large directories
	
	" TODO: Add some kind of assertion to make sure a:root is what we
	" expect it to be
	let obj = copy(self)
	" We have to store a copy of the tree as we manipulate the tree,
	" opening and closing nodes.  This would destroy the users current
	" tree view if we did not make a copy.
	let obj.root = deepcopy(a:root)
	let obj.currNode = obj.followLeft(obj.root)
	return obj
endfunction

" Function: NERDTreeWalker.followLeft {{{2
function! s:NERDTreeWalker.followLeft(node) dict
	" Take a node and follow it as far left down the tree as we can
	let node = a:node
	if ! node.path.isDirectory
		return node
	else
		" Must be a directory
		if ! node.isOpen
			call node.open()
		endif
		if node.getChildCount() > 0
			let leftmost = self.followLeft(node.getChildByIndex(0, 0))
			return leftmost
		else
			return node
		endif
endfunction

" Function: NERDTreeWalker.next {{{2
function! s:NERDTreeWalker.next() dict
	" Save self.currNode we're going to return this.  Then see if you have
	" any siblings at this level, if so, follow it left as far as you can.
	" If not, then walk back up the tree looking for a sibling
	let save = self.currNode
	let node = self.currNode
	let sibling = node.findSibling(1)
	while ( sibling == {} ) && ( node.parent != {} )
		let node = node.parent
		let sibling = node.findSibling(1)
	endwhile
	
	" We either found a sibling, or we are at the root
	" If we found a sibling, follow it left
	if sibling != {}
		let self.currNode = self.followLeft(sibling)	
	else
		" We're at the root
		let self.currNode = {}
	endif
	return save
endfunction

" Function: NERDTreeWalker.tmp {{{2
function! s:NERDTreeWalker.tmp() dict
	" Find our next sibling.  If we dont have one at this level, walk back
	" up the tree looking for one.
	" http://myarch.com/treeiter/traditways
	let root = self.currNode
	if root.path.isDirectory && ! root.isOpen
		call root.open()
	endif
	if root.path.isDirectory && root.getChildCount() > 0
		let node = root.children[0]
		while node != {}
			if node.path.isDirectory && ! node.isOpen
				call node.open()
			endif

			" echo "Visiting node " . node.path.str()
			let self.currNode = node
			if ( node.path.isDirectory ) && ( node.getChildCount() > 0 )
				" non-leaf node
				let node = node.children[0] 	
			else
				" leaf node (maybe directory, maybe not)
				while (node.findSibling(1) == {}) && (! node.path.equals(self.root.path))
					let node = node.parent
				endwhile
				let node = node.findSibling(1)
			endif
		endwhile
	endif
endfunction

" Function: NERDTreeWalker.hasNext {{{2
function! s:NERDTreeWalker.hasNext() dict
	return self.currNode != {} ? 1 : 0
endfunction
" Function: NERDTreeWalker.reset {{{2
function! s:NERDTreeWalker.reset() dict
	let self.currNode = self.followLeft(self.root)
endfunction

" Function: NERDTreeWalker.walk {{{2
function! s:NERDTreeWalker.walk(node) dict
	let node = a:node
	if ! node.path.isDirectory
		return [ node ]
	else
		" Make sure the node is opened
		if ! node.isOpen
			call node.open()
		endif

		let ret = []
		for c in node.children
			let tmp = self.walk(c)
			call extend(ret, tmp)
		endfor
		return ret
	endif
endfunction

" Init stuff {{{1
if ! exists("FuzzyFinder")
	let FuzzyFinder = s:FuzzyFinder.new()
	call NERDTreeAddKeyMap({'key': '<Leader>t', 'quickhelpText': 'Search NERDTree Quickly', 'callback': 'FuzzyFinder.search'})
else
	finish
endif

" Public Interfaces necessary for autocmd and completefunc {{{1
" Function: Complete {{{2
function! Complete(findstart, base)
	return g:FuzzyFinder.complete(a:findstart, a:base)
endfunction

" Function: Finish {{{2
function! Finish()
	return g:FuzzyFinder.finish()
endfunction

" Function: OnCursorMovedI {{{2
function! OnCursorMovedI()
	return g:FuzzyFinder.onCursorMovedI()
endfunction

" For debugging {{{1
let NERDTreeWalker = s:NERDTreeWalker

" vim: fdm=marker
