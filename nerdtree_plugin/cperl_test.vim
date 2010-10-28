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
			let ret = [ a:base ]
			while self.walker.hasNext()
				let path = self.walker.next()
				if path =~ a:base
					call add(ret, path)
				endif
			endwhile
		endif
		return ret
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
		" call self.handleSelectedFile()
	endif
	call self.jumpNode.putCursorHere(0, 0)
	call NERDTreeRender()
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
	" TODO: Implement pre, post, and inorder traversal selection
	" Not sure if it matters that its an n-ary tree.
	
	" TODO: Add some kind of assertion to make sure a:root is what we
	" expect it to be
	let obj = copy(self)
	let obj.root = deepcopy(a:root)

	" We have to open all the tree nodes as they are lazily populated
	" TODO: We leave the tree totally expanded, we have to fix that
	" call obj.root.openRecursively()

	" let obj.list = obj.walk(obj.root)
	let obj.list = split(glob("`find . -type f -print`"))
	let obj.idx = 0
	return obj
endfunction

" Function: NERDTreeWalker.next {{{2
function! s:NERDTreeWalker.next() dict
	if self.idx >= len(self.list)
		throw "Index too large.  Use .hasNext() to validate this method is safe to call."
	endif
	let ret = self.list[self.idx]
	let self.idx = self.idx + 1
	return ret
endfunction

" Function: NERDTreeWalker.hasNext {{{2
function! s:NERDTreeWalker.hasNext() dict
	return self.idx < len(self.list) ? 1 : 0
endfunction
" Function: NERDTreeWalker.reset {{{2
function! s:NERDTreeWalker.reset() dict
	let self.idx = 0
endfunction
" Function: NERDTreeWalker.walk {{{2
function! s:NERDTreeWalker.walk(node) dict
	let node = a:node
	if ! node.path.isDirectory
		return [ node ]
	else
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

" vim: fdm=marker
