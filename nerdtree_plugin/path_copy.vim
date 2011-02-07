" Intro blurb blah blah
" Maintainer: chris.perl@gmail.com
"
if exists("g:loaded_nerdtree_path_copy")
    finish
endif
let g:loaded_nerdtree_path_copy = 1

call NERDTreeAddKeyMap({'key': '<Leader>y', 'quickhelpText': 'Copy relative path of current node into the unamed register', 'callback': 'NERDTreeCopySelectedRelativePathToUReg', })

function! NERDTreeCopySelectedRelativePathToUReg()
	let t = g:NERDTreeFileNode.GetSelected()
	if t !=# {}
		call setreg('"', fnamemodify(t.path.str(), ':.'))
	else
		call setreg('"', '')
	endif
endfunction
