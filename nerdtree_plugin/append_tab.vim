" Intro blurb blah blah
" Maintainer: chris.perl@gmail.com
"
if exists("g:loaded_nerdtree_append_tab")
    finish
endif
let g:loaded_nerdtree_append_tab = 1

call NERDTreeAddKeyMap({'key': '<Leader>t', 'quickhelpText': 'open in new tab (append to tab list)',          'callback': 'NERDTreeAppendTab', })
call NERDTreeAddKeyMap({'key': '<Leader>T', 'quickhelpText': 'open in new tab silently (append to tab list)', 'callback': 'NERDTreeAppendTabSilent', })

function! NERDTreeAppendTab()
	let t = g:NERDTreeFileNode.GetSelected()
	if t !=# {}
		call t.openInNewTab({'makeLastTab': 1})
	endif
endfunction

function! NERDTreeAppendTabSilent()
	let t = g:NERDTreeFileNode.GetSelected()
	if t !=# {}
		call t.openInNewTab({'makeLastTab': 1, 'stayInCurrentTab': 1})
	endif
endfunction
