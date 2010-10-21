if exists("g:loaded_nerdtree_cperl_test")
    finish
endif
let g:loaded_nerdtree_cperl_test = 1

call NERDTreeAddMenuItem({'text': '(s)earch for a node', 'shortcut': 's', 'callback': 'Search'})
set completefunc=Complete

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
    setlocal modifiable
    call setpos(".", [0, 1, 8, 0])
    let saved_line = getline(".")
    call setline(1, "File >> ")
    call feedkeys("A", 'n')
    call feedkeys("\<C-x>\<C-u>", 'n')
    " call s:walkTree(b:NERDTreeRoot)
    " call setline(1, saved_line)
    " setlocal nomodifiable
    " call NERDTreeRender()
endfunction

function! Complete(findstart, base)
    if a:findstart == 1
        return 8
    else
        return [ "foo", "bar", "baz" ]
    endif
endfunction
