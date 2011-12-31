" Do things differently for linux and macosx
if has("macunix")
	set guifont=Monaco\:h11.00
	"set guifont=DejaVu\ Sans\ Mono\:h11.00
	"set guifont=Envy\ Code\ R:h11.00

	" Turn off the toolbar
	set guioptions-=T

	" Turn off the left scrollbar
	set guioptions-=L

	" 2011-05-24
	" Set the gui colorscheme
	set bg=light
	colorscheme solarized

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

" 2011-12-27 
" Make the beeping stop!
" http://vim.wikia.com/wiki/Disable_beeping
" under the section "Disable beep and flash with gvimrc"
set noerrorbells visualbell t_vb=
