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
