" Do things differently for linux and macosx
if has("macunix")
	set guifont=Monaco\:h11.00
	"set guifont=DejaVu\ Sans\ Mono\:h11.00
	"set guifont=Envy\ Code\ R:h11.00

	" Turn off the toolbar
	set guioptions-=T

	" 2010-08-25
	" Override the highlight settings for FoldColumn with macvim as I can
	" barely read the default
	hi clear FoldColumn
	hi link FoldColumn Folded

elseif has("unix")
	set guifont=Monaco\ 8
	set tabpagemax=30

	" Set the gui tabs to a constant width
	if version >= 700
	    "set showtabline to show when more than one tab
	    set showtabline=1
	    "set tab labels to show at most 12 characters
	    set guitablabel=%-30.30t%M
	endif
endif

set lines=60
set columns=130
