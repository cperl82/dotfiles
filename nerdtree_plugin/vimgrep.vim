" Intro blurb blah blah
" Maintainer: chris.perl@gmail.com
"
if exists("g:loaded_nerdtree_vimgrep")
    finish
endif
let g:loaded_nerdtree_vimgrep = 1

call NERDTreeAddKeyMap({
		\ 'key': '<Leader>s',
		\ 'quickhelpText': 'Setup an :lvimgrep search using the current node as a base',
		\ 'callback': 'NERDTreeVimGrep', })

function! NERDTreeVimGrep()
	let t = g:NERDTreeFileNode.GetSelected()
	let path = t.path.str()
	if t !=# {}
		call feedkeys(":lvimgrep //j " . path . "/**/*" . "\<S-Left>\<S-Left>\<Right>")
	endif
endfunction
