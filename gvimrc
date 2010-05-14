"set textwidth=80
set autoindent
set laststatus=2
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set number
set hl
set incsearch

" Do things differently for linux and macosx
if has("unix")
	set guifont=Monaco\ 8
	map <C-S-l> :tabn<CR>
	map <C-S-h> :tabp<CR>
	set tabpagemax=30

	" Set the gui tabs to a constant width
	if version >= 700
	    "set showtabline to show when more than one tab
	    set showtabline=1
	    "set tab labels to show at most 12 characters
	    set guitablabel=%-30.30t%M
	endif

elseif has("macunix")
	set guifont=Monaco\:h11.00
	"set guifont=DejaVu\ Sans\ Mono\:h11.00
	"set guifont=Envy\ Code\ R:h11.00
endif

"colorscheme asmdev
colorscheme ir_black

set lines=60
set columns=130

set hlsearch

" 2010-05-12
" Override the highlight settings for NERD_tree.  This is mainly because
" ir_black colorscheme looks terrible with treeRO linked to WarningMsg
hi link treeRO Normal
