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
	echo "Called Search()"	
	let s:savedNode = b:NERDTreeRoot.GetSelected()
	if s:savedNode == {}
		b:NERDTreeRoot.putCursorHere(0, 0)
		let s:savedNode = b:NERDTreeRoot
	endif
	setlocal modifiable
	setlocal completefunc=Complete
	autocmd InsertLeave <buffer> call Finish()
	call setpos(".", [0, 2, 8, 0])
	call setline(2, "File >> ")
	call feedkeys("A", 'n')
	call feedkeys("\<C-x>\<C-u>", 'n')
	" call s:walkTree(b:NERDTreeRoot)
	" call setline(1, saved_line)
	" setlocal nomodifiable
	" let path = getline(".")
	" let path = strpart(path, 8)
	" echo path
	" call NERDTreeRender()
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
	call s:savedNode.putCursorHere(0, 0)
	call NERDTreeRender()
endfunction
