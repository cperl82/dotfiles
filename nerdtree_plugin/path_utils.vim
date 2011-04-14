" Intro blurb blah blah
" Maintainer: chris.perl@gmail.com
"
if exists("g:loaded_nerdtree_path_utils")
    finish
endif
let g:loaded_nerdtree_path_utils = 1

call NERDTreeAddKeyMap({
		\ 'key': '<Leader>y',
		\ 'quickhelpText': 'Copy relative path of current node into the unamed register',
		\ 'callback': 'NERDTreeCopySelectedRelativePathToUReg', })

call NERDTreeAddKeyMap({
		\ 'key': '<Leader>e',
		\ 'quickhelpText': 'Echo the full path of the selected node',
		\ 'callback': 'NERDTreeEchoSelectedFullPath', })

function! NERDTreeCopySelectedRelativePathToUReg()
	let t = g:NERDTreeFileNode.GetSelected()
	if t !=# {}
		call setreg('"', fnamemodify(t.path.str(), ':.'))
	else
		call setreg('"', '')
	endif
endfunction

function! NERDTreeEchoSelectedFullPath()
	let t = g:NERDTreeFileNode.GetSelected()
	if t !=# {}
		echo t.path.str()
	endif
endfunction
