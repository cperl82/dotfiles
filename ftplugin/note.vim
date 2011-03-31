imap <buffer> <Leader>t <C-r>=strftime("%Y-%m-%d")<CR>
imap <buffer> <Leader>T <C-r>=strftime("%Y-%m-%d %H:%M:%S")<CR>
imap <buffer> <Leader>b ▸ 
imap <buffer> <Leader>v ￭ 

setlocal tw=90 sw=4 ts=4 softtabstop=4 formatoptions+=t formatoptions-=l
setlocal foldmethod=syntax foldminlines=0 foldcolumn=2
