
augroup cipher
	"set verbose=9
	autocmd!
	autocmd BufEnter,VimEnter *.aes call s:OpenSSLEnter()
	autocmd BufReadPre,FileReadPre *.aes call s:OpenSSLReadPre()
	autocmd BufReadPost,FileReadPost *.aes call s:OpenSSLReadPost()
	autocmd BufWritePre,FileWritePre *.aes call s:OpenSSLWritePre()
	autocmd BufWritePost,FileWritePost *.aes call s:OpenSSLWritePost()
augroup END

function! s:OpenSSLEnter()
	setlocal noswapfile
	setlocal viminfo=""
	setlocal noshelltemp
endfunction

function! s:OpenSSLReadPre()
endfunction

function! s:OpenSSLReadPost()
	%!openssl aes-256-cbc -a -d
endfunction

function! s:OpenSSLWritePre()
	set cmdheight=3
	%!openssl aes-256-cbc -a
endfunction

function! s:OpenSSLWritePost()
	silent! undo
	set cmdheight&
	redraw!
endfunction
