" Intro blurb blah blah
" Maintainer: chris.perl@gmail.com

" Class: FuzzyFinder {{{1
let s:FuzzyFinder = {}
" Function: FuzzyFinder.new {{{2
function! s:FuzzyFinder.new() dict
	let obj = copy(self)
	let obj.prompt = "File >>> "
	let obj.selectedFile = ""
	return obj
endfunction

" Function: FuzzyFinder.search {{{2
function! s:FuzzyFinder.search() dict
	" Reset the selected file
	let self.selectedFile = ""

	" Check that we are on a selected node
	if b:NERDTreeRoot.GetSelected() == {}
		echo "Please select a node first"
		return
	endif

	" Get a new walker
	if ! exists("self.walker")
		let self.walker = s:Walker.new(b:NERDTreeRoot)
	endif

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
		if a:base == ""
			return []
		else
			call self.walker.reset()
			call complete_add(a:base)
			while self.walker.hasNext()
				let path = self.walker.next()
				let shortpath = fnamemodify(path, ":~:.")
				if shortpath =~ a:base
					call complete_add({'word': path, 'abbr': shortpath})
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
	call self.jumpNode.putCursorHere(0, 0)
	call NERDTreeRender()
	if self.selectedFile != ""
		call self.handleSelectedFile()
	endif
endfunction

" Function: FuzzyFinder.handleSelectedFile {{{2
function! s:FuzzyFinder.handleSelectedFile()
	execute "tabedit " . self.selectedFile
endfunction

" Function: FuzzyFinder.onCursorMovedI {{{2
function! s:FuzzyFinder.onCursorMovedI()
	" TODO: Check for correct positioning of the cursor, like fuf
	call feedkeys("\<C-x>\<C-u>", 'n')
endfunction

" Class: Walker {{{1
let s:Walker = {}
" Function: Walker.new {{{2
function! s:Walker.new(root) dict
	let obj = copy(self)
	let obj.idx = 0
	let selected = a:root.GetSelected()
	if selected == {}
		let obj.dir = ""
		let obj.files = []
	else
		let obj.dir = selected.path.str()
		echo "Building file list for dir: " . fnamemodify(obj.dir, ":~:.")
		let obj.files = split(glob(printf("`find %s -type f -print`", obj.dir)))
	endif
	return obj
endfunction


" Function: Walker.next {{{2
function! s:Walker.next() dict
	let idx = self.idx
	let self.idx += 1
	return self.files[idx]
endfunction

" Function: Walker.hasNext {{{2
function! s:Walker.hasNext() dict
	return self.idx < len(self.files) ? 1 : 0
endfunction

" Function: Walker.reset {{{2
function! s:Walker.reset() dict
	let self.idx = 0
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
let Walker = s:Walker

" vim: fdm=marker
