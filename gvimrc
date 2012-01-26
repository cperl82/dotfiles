" Do things differently for linux and macosx
if has("macunix")
	set guifont=Monaco\:h11.00
	"set guifont=DejaVu\ Sans\ Mono\:h11.00
	"set guifont=Envy\ Code\ R:h11.00

	" 2011-05-24
	" Set the gui colorscheme
	set bg=light
	colorscheme solarized

	" 2012-01-26
	" Fixing cmd line editing stuff in macvim
	" :he macmeta
	" :he gui-extras
	set macmeta
	" start of line
	cnoremap <C-a> <Home>
	" delete character under cursor
	cnoremap <C-d> <Del>
	" end of line
	cnoremap <C-e> <End>
	" back one word
	cnoremap <M-b> <S-Left>
	" forward one word
	cnoremap <M-f> <S-Right>
	" Delete one word to the right
	cnoremap <M-d> <S-Right><C-w>
	" delete one word to the left
	cnoremap <M-BS> <C-w>

elseif has("unix")
	set guifont=Monaco\ 7

	" Turn off the menu bar
	set guioptions-=m

	set bg=light
	colorscheme solarized

endif

set lines=60
set columns=130

" Turn off the toolbar
set guioptions-=T

" Turn off the left scrollbar
set guioptions-=L


" 2011-12-27 
" Make the beeping stop!
" http://vim.wikia.com/wiki/Disable_beeping
" under the section "Disable beep and flash with gvimrc"
set noerrorbells visualbell t_vb=
