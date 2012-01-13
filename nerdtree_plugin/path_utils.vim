" Intro blurb blah blah
" Maintainer: chris.perl@gmail.com
"
if exists("g:loaded_nerdtree_path_utils")
    finish
endif
let g:loaded_nerdtree_path_utils = 1

call NERDTreeAddKeyMap({
		\ 'key': ',y',
		\ 'scope': 'Node',
		\ 'quickhelpText': 'Copy relative path of current node into the unamed register',
		\ 'callback': 'NERDTreeCopyPathToURegRelative', })

call NERDTreeAddKeyMap({
		\ 'key': ',Y',
		\ 'scope': 'Node',
		\ 'quickhelpText': 'Copy absolute path of current node into the unamed register',
		\ 'callback': 'NERDTreeCopyPathToURegAbsolute', })

call NERDTreeAddKeyMap({
		\ 'key': ',e',
		\ 'scope': 'Node',
		\ 'quickhelpText': 'Echo the relative path of the selected node',
		\ 'callback': 'NERDTreeEchoPathRelative', })

call NERDTreeAddKeyMap({
		\ 'key': ',E',
		\ 'scope': 'Node',
		\ 'quickhelpText': 'Echo the absolute path of the selected node',
		\ 'callback': 'NERDTreeEchoPathAbsolute', })

call NERDTreeAddKeyMap({
		\ 'key': ',c',
		\ 'scope': 'Node',
		\ 'quickhelpText': 'Copy the relative path of the selected node to the clipboard',
		\ 'callback': 'NERDTreeCopyPathToClipboardRelative', })

call NERDTreeAddKeyMap({
		\ 'key': ',C',
		\ 'scope': 'Node',
		\ 'quickhelpText': 'Copy the relative path of the selected node to the clipboard',
		\ 'callback': 'NERDTreeCopyPathToClipboardAbsolute', })

function! NERDTreeCopyPathToURegRelative(node)
	call setreg('"', fnamemodify(a:node.path.str(), ':.'))
endfunction

function! NERDTreeCopyPathToURegAbsolute(node)
	call setreg('"', a:node.path.str())
endfunction

function! NERDTreeEchoPathRelative(node)
	echo fnamemodify(a:node.path.str(), ':.')
endfunction

function! NERDTreeEchoPathAbsolute(node)
	echo a:node.path.str()
endfunction

function! NERDTreeCopyPathToClipboardAbsolute(node)
	if has("macunix")
		exec printf("silent !echo -n %s | pbcopy", a:node.path.str())
	endif
endfunction

function! NERDTreeCopyPathToClipboardRelative(node)
	if has("macunix")
		exec printf("silent !echo -n %s | pbcopy", fnamemodify(a:node.path.str(), ':.'))
	endif
endfunction
