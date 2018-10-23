" 2012-07-06 - For man page viewing want to be able to open and close folds
" with only one key press

" 2012-07-21 - Need to wrap these with a check for filetype == "man" because
" we explicitly call 'runtime! ftplugin/man.vim' in our vimrc, which kicks in
" the logic of the 'after' plugin (which is how we get sourced in the first
" place).  The long story short is that if we don't wrap this then it will get
" sourced on every invocation of vim and any buffer we go to edit will be
" nomodifiable
if &filetype == "man"
  nnoremap <CR> za
  nnoremap <Space> za
  setl readonly nomodifiable
endif
