if exists("g:loaded_nerdtree_cperl_test")
	finish
endif
let g:loaded_nerdtree_cperl_test = 1

"call NERDTreeAddMenuItem({'text': '(s)earch for a node', 'shortcut': 's', 'callback': 'Search'})
call NERDTreeAddKeyMap({'key': '<Leader>t', 'quickhelpText': 'Search NERDTree Quickly', 'callback': 'Search'})

function! s:walkTree(dirNode)
	"echo a:dirNode.path.str()
	if a:dirNode.path.isDirectory
	    for i in a:dirNode.children
	        "echo i.path.str()
	        if i.path.isDirectory
	            call s:walkTree(i)
	        endif
	    endfor
	endif
endfunction

function! Search()
	" reset where we store the filename
	unlet g:cperlline

	let s:savedNode = g:NERDTreeFileNode.GetSelected()
	if s:savedNode == {}
		let s:savedNode = b:NERDTreeRoot
	endif
	setlocal modifiable
	setlocal completefunc=Complete

	autocmd InsertLeave <buffer> call Finish()

	" TODO: Explain how this works, its kinda f'in confusing
	inoremap <buffer> <silent> <CR> <C-y><C-r>=SaveSelection() ? "" : ""<CR><ESC>

	"call setpos(".", [0, 2, 8, 0])
	"call setline(2, "File >> ")
	call setpos(".", [0, line("w0"), 8, 0])
	call setline(line("w0"), "File >> ")
	call feedkeys("A", 'n')
	call feedkeys("\<C-x>\<C-u>", 'n')
endfunction

function! SaveSelection()
	let g:cperlline = getline(".")
endfunction

function! Complete(findstart, base)
	if a:findstart == 1
	    return 8
	else
	    return [ "foo", "bar", "baz" ]
	endif
endfunction

function! Finish()
	setlocal completefunc&
	setlocal nomodifiable
	" Cant figure this out at the moment, keeps raising errors
	" iunmap <buffer> <CR>
	call s:savedNode.putCursorHere(0, 0)
	call NERDTreeRender()
endfunction

" Class: FuzzyFinder {{{1
let s:FuzzyFinder = {}

" Function: FuzzyFinder.new {{{2
function! s:FuzzyFinder.new() dict
endfunction

" Function: FuzzyFinder.search {{{2
function! s:FuzzyFinder.search() dict
endfunction

" Function: FuzzyFinder.saveSelection {{{2
function! s:FuzzyFinder.saveSelection()
endfunction

" Function: FuzzyFinder.complete {{{2
function! s:FuzzyFinder.complete(findstart, base) dict
endfunction

" Function: FuzzyFinder.cleanup {{{2
function! s:FuzzyFinder.cleanup()
endfunction

" Class: NERDTreeWalker {{{1
let s:NERDTreeWalker = {}

" Function: NERDTreeWalker.new {{{2
function! s:NERDTreeWalker.new() dict
	" TODO: Implement pre, post, and inorder traversal selection
endfunction

" Function: NERDTreeWalker.getFirst {{{2
function! s:NERDTreeWalker.getFirst()
endfunction

" Function: NERDTreeWalker.getNext {{{2
function! s:NERDTreeWalker.getNext()
endfunction

" vim: fdm=marker
