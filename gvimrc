"set textwidth=80
set autoindent
set laststatus=2
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set number
set hl
set incsearch
"set guifont=DejaVu\ Sans\ Mono\:h11.00
"set guifont=Envy\ Code\ R:h11.00
set guifont=Monaco\:h11.00
"colorscheme asmdev
colorscheme ir_black

set lines=60
set columns=130

set hlsearch

" 2010-05-12
" Override the highlight settings for NERD_tree.  This is mainly because
" ir_black colorscheme looks terrible with treeRO linked to WarningMsg
hi link treeRO Normal
