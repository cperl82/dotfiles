imap <buffer> <Leader>t <C-r>=strftime("%Y-%m-%d")<CR>
imap <buffer> <Leader>T <C-r>=strftime("%Y-%m-%d\|%H:%M:%S")<CR>

setlocal et tw=79 sw=4 ts=4 softtabstop=4 formatoptions+=t formatoptions-=l concealcursor=n

" 2012-02-14
" Make it so that writing a bulleted list like:
" - some text that is long enough to wrap
"   actually wrap to the proper indent
setlocal comments+=fb:- formatoptions+=cq
