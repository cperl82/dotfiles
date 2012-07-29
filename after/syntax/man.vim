syn region manTopLevelFold start='^\(NAME\|LIBRARY\|SYNOPSIS\|COPYRIGHT\|\%^\)\@!\u\u' end='^$\n\(\u\u\)\@=' fold contains=ALL
setl fdm=syntax
nnoremap <buffer> <Leader>ss :syn sync fromstart<CR>
