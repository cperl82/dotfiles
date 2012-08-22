syn region manTopLevelFold start='^\(NAME\s*$\|LIBRARY\|SYNOPSIS\|\%^\)\@!\u\+' end='\n\(\u\+\)\@=' fold contains=ALL
setl fdm=syntax
nnoremap <buffer> <Leader>ss :syn sync fromstart<CR>
