" Intro blurb blah blah
" Maintainer: chris.perl@gmail.com

" Init stuff {{{1
if exists("loaded_cperl_fuzzy_finder")
	finish
endif
let loaded_cperl_fuzzy_finder = 1
call NERDTreeAddKeyMap({'key': '<Leader>f', 'quickhelpText': 'Search Open Buffers Quickly', 'callback': 'Search'})

" Class: FuzzyFinder {{{1
let s:FuzzyFinder = {}
" Function: FuzzyFinder.new {{{2
function! s:FuzzyFinder.new() dict
	let obj = copy(self)
	let obj.prompt = "Buffer Search >>> "
	let obj.selectedFile = ""
	let obj.walker = s:Walker.new()
	return obj
endfunction

" Function: FuzzyFinder.search {{{2
function! s:FuzzyFinder.search() dict
	" Reset the selected file
	let self.selectedFile = ""

	setlocal modifiable
	setlocal completeopt=menuone
	setlocal completefunc=b:Complete
	autocmd InsertLeave  <buffer> call b:Finish()
	autocmd CursorMovedI <buffer> call b:OnCursorMovedI()

	" TODO: Explain how this works, its kinda f'in confusing
	inoremap <buffer> <silent> <CR> <C-y><C-r>=b:FuzzyFinder.saveSelection()<CR><ESC>

	let self.savedPos = getpos(".")
	call setpos(".", [0, line("w0"), len(self.prompt), 0])
	let self.savedLine = getline(".")
	call setline(line("w0"), self.prompt)
	call feedkeys("A\<C-x>\<C-u>", 'n')
endfunction

" Function: FuzzyFinder.saveSelection {{{2
function! s:FuzzyFinder.saveSelection() dict
	let self.selectedFile = self.stripPrompt(getline("."))
	return ""
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
				let bufname = self.walker.next()
				if bufname =~ a:base
					call complete_add({'word': bufname, 'abbr': bufname})
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
	" Turn off the autocmd's we defined on our way in so that the call to
	" setline wont invoke them
	autocmd! InsertLeave  <buffer>
	autocmd! CursorMovedI <buffer>
	iunmap <buffer> <CR>
	call setline(line("w0"), self.savedLine)
	call setpos(".", self.savedPos)
	setlocal completefunc&
	setlocal completeopt&
	setlocal nomodifiable
	if self.selectedFile != ""
		call self.handleSelectedFile()
	endif
endfunction

" Function: FuzzyFinder.handleSelectedFile {{{2
function! s:FuzzyFinder.handleSelectedFile()
	for i in range(1, tabpagenr('$'))
		for j in tabpagebuflist(i)
			if bufname(j) == self.selectedFile
				execute "tabnext " . i
				return
			endif
		endfor
	endfor
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
function! s:Walker.new() dict
	let obj = copy(self)
	let obj.idx = 0
	let obj.buflist = []
	return obj
endfunction

" Function: Walker.next {{{2
function! s:Walker.next() dict
	let idx = self.idx
	let self.idx += 1
	return self.buflist[idx]
endfunction

" Function: Walker.hasNext {{{2
function! s:Walker.hasNext() dict
	return self.idx < len(self.buflist) ? 1 : 0
endfunction

" Function: Walker.reset {{{2
function! s:Walker.reset() dict
	let self.idx = 0
	let self.buflist = []
	for i in range(1, bufnr('$'))
		if bufexists(i)
			call add(self.buflist, bufname(i))
		endif
	endfor
endfunction

" Public Interfaces necessary for autocmd and completefunc {{{1
" Function: Search {{{2
function! Search()
	if ! exists("b:FuzzyFinder")
		let b:FuzzyFinder = s:FuzzyFinder.new()
	endif
	return b:FuzzyFinder.search()
endfunction

" Function: Complete {{{2
function! b:Complete(findstart, base)
	return b:FuzzyFinder.complete(a:findstart, a:base)
endfunction

" Function: Finish {{{2
function! b:Finish()
	return b:FuzzyFinder.finish()
endfunction

" Function: OnCursorMovedI {{{2
function! b:OnCursorMovedI()
	return b:FuzzyFinder.onCursorMovedI()
endfunction

" For debugging {{{1
let Walker = s:Walker

" vim: fdm=marker
