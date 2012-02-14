" Pathogen testing
call pathogen#infect($BUNDLE_ROOT) 

" Change the default leader key
let mapleader = ","

" Enable file type detection, all three modes
" detection, indent, plugin.  See :help filetype
filetype plugin indent on

" Turn on syntax highlighting, using defaults
" See :help syntax
syntax on

set laststatus=2
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [FE=%{&fileencoding}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set autoindent
set number
set numberwidth=4
set incsearch

" Turn on search highlighting
set hlsearch

" Use the man.vim plugin if its available
runtime! ftplugin/man.vim

" colorscheme asmdev
" colorscheme norwaytoday
if &term =~ ".*256color"
	colorscheme xoria256
else
	colorscheme ir_black
endif

" 2011-02-08
" Set the NERDTree status line such that it always shows the full path of the
" currently selected node.  I realize this is a little inefficient at the moment
" since it calls GetSelected() twice, but I cannot figure out a way to get
" around that at the moment.  This status line pretty much makes my
" path_copy.vim NERDTree plugin useless, but whatever.
" let g:NERDTreeStatusline='%{has_key(g:NERDTreeFileNode.GetSelected(), "path") ? g:NERDTreeFileNode.GetSelected().path.str() : b:NERDTreeRoot.path.str()}'
let g:NERDTreeStatusline = '%{ getcwd() }'

" 2011-04-14
" Use the fancy arrows for the NERDtree interface and turn off the AutoCenter
" feature
let g:NERDTreeDirArrows  = 1
let g:NERDTreeAutoCenter = 0

" 2011-12-28
" Change the default NERDTree quick help key so I can still reverse search
let g:NERDTreeMapHelp = 'H'

" 2011-12-31
" Set the default size of the NERDTree window
let g:NERDTreeWinSize = 36

" 2012-01-14
" Map :NERDTreeFind to an easily accessible key sequence
nnoremap <silent> ,r :NERDTreeFind<CR>

" 2011-12-31
" Disable netrw
" :help netrw-noload
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1

" Automatically open NERDTree if its a directory
"augroup CPTest
"autocmd CPTest VimEnter * call s:OpenNERDTreeIfDirectory(expand("<amatch>"))
"augroup NONE

function! s:OpenNERDTreeIfDirectory(arg)
	echo "s:OpenNERDTreeIfDirectory called with arg " . a:arg
	let l:dir = fnameescape(a:arg)
	if isdirectory(l:dir)
		let l:bufnum = bufnr(a:arg)
		exec ":bwipe " . l:bufnum
		exec ":NERDTree " . a:arg
	endif
endfunction

" 2011-12-25
" Trying to use buffers instead of tab pages
set hidden

" Make sure that minibufexpl does not try to open a buffer in a window
" occupied by a buffer where &modifiable is not set
" 2012-01-09
" Disabling MiniBufExplorer and trying plain old Bufexplorer
let loaded_minibufexplorer = 1
let g:miniBufExplModSelTarget = 1
let g:miniBufExplorerMoreThanOne = 2
let g:miniBufExplMapWindowNavVim = 1

" 2012-01-09
" Bufexplorer settings
let g:bufExplorerDetailedHelp = 1
let g:bufExplorerSortBy = 'fullpath'
let g:bufExplorerShowTabBuffer = 0
let g:bufExplorerShowRelativePath = 1
let g:bufExplorerJumpToCurBufLine = 1

" 2012-01-09
" Window movement commands
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l

" 2011-12-30
" Easier access to :Kwbd command from plugin/kwbd.vim
nnoremap <silent> <Leader>bd :Kwbd<CR>

" 2011-12-15
" Vimwiki Configuration
let wiki_0 = {}
let wiki_0.path = '~/Dropbox/vimwiki/markup'
let wiki_0.path_html = '~/Dropbox/vimwiki/html'
let wiki_0.nested_syntaxes = {
			\ 'python': 'python',
			\ 'ansi-c': 'c',
			\ 'perl': 'perl',
			\ 'shell': 'sh',
			\ 'config': 'config',
			\ 'vimscript': 'vim',
			\ 'java': 'java' }
let wiki_0.auto_export = 0
let wiki_0.syntax = 'markdown'

let g:vimwiki_list = [ wiki_0 ]
let g:vimwiki_camel_case = 0
let g:vimwiki_folding = 1
let g:vimwiki_fold_lists = 0

" Fixup some vimwiki colors
hi link VimwikiNoExistsLink Comment

" Fixup conflicts with Quicksilver (<C-Space> already used)
map <leader>tt <Plug>VimwikiToggleListItem

" 2011-02-11
" Emacs style command line editing slightly modified
" :help emacs-keys
" 2012-01-26
" Trying to figure the cmd line mappings for macvim
" Mappings for the gui are in gvimrc
" :he macmeta
" :he gui-extras
if !has("gui_running")
	" start of line
	cnoremap <C-a> <Home>
	" delete character under cursor
	cnoremap <C-d> <Del>
	" end of line
	cnoremap <C-e> <End>
	" back one word
	cnoremap <Esc>b	<S-Left>
	" forward one word
	cnoremap <Esc>f	<S-Right>
	" Delete one word to the right
	cnoremap <Esc>d	<S-Right><C-w>
	" delete one word to the left
	cnoremap <Esc><C-?> <C-w>
endif

" 2011-01-11
" Map / and ? while in visual mode to search for the highlighted text
vmap / y/<C-R>=escape('<C-R>"', '/\[]')<CR><CR>
" Escaping `?' is necessary with backward searching
vmap ? y?<C-R>=escape('<C-R>"', '?/\[]')<CR><CR>

" 2012-01-27
" Make copying to the clipboard easier
if has("clipboard")
	vmap <Leader>c "*y<CR>
endif

" 2011-04-15
" Map <Leader>n to toggle line numbers on and off.  I find this useful for when
" I need to copy and paste data out of a vim window
nmap <silent> <Leader>n :exec &number ? ":set nonu" : ":set nu"<CR>

" 2011-05-06
" Some ideas taken from
" http://stevelosh.com/blog/2010/09/coming-home-to-vim/
nnoremap ,<Space> :nohl<CR>
nnoremap <Tab> %

" 2010-09-24
" Added newer python syntax highlighting script
" Enable all the syntax options in it (.vim/syntax/python.vim)
let python_highlight_all = 1
let python_highlight_indent_errors = 0
let python_highlight_space_errors = 0

" 2011-03-30
" Vim's error highlighting of vimscript isn't always correct, turn it off
" :help ft-vim-syntax
let g:vimsyn_noerror = 1

" 2011-12-13
" Control where ".netrwhist" gets written.  If this variable is not set then
" netrw tries to figure it out itself.
" See /opt/local/share/vim/vim73/autoload/netrw.vim
let g:netrw_home = $HOME . "/.netrwhist"

" 2011-05-04
" Playing around with making it easier to deal with sessions with vim
function! s:SaveOrCreateSession()
	let session_dir = '~/.vim/sessions'

	if v:this_session != ""
		" Session already exists just resave it
		let cmd = printf("mksession! %s", v:this_session)
		execute cmd
	else
		" Prompt for a new session name and store it in
		" ~/.vim/sessions
		if glob(session_dir) == ""
			call mkdir(fnamemodify(session_dir, ":p"), "")
		endif
		let name = input("Enter a name for this session: ")	
		if name != ""
			let cmd = printf("mksession! %s", session_dir . '/' . name . '.vim')
			execute cmd
		endif
	endif
	if v:this_session != ""
		echo "Session saved as: " . v:this_session
	endif
endfunction
command! -nargs=0 SaveOrCreateSession call s:SaveOrCreateSession()
nmap <Leader>s :SaveOrCreateSession<CR>

" 2011-03-15
" Playing around with better ways to get shell output into vim
" http://vim.wikia.com/wiki/Display_output_of_shell_commands_in_new_window
function! s:ExecuteInShell(command)
	let command = join(map(split(a:command), 'expand(v:val)'))
	let winnr = bufwinnr('^' . command . '$')
	silent! execute  winnr < 0 ? 'botright new ' . fnameescape(command) : winnr . 'wincmd w'
	setlocal buftype=nowrite bufhidden=wipe nobuflisted noswapfile nowrap number
	echo 'Execute ' . command . '...'
	silent! execute 'silent %!'. command
	silent! execute 'resize ' . line('$')
	silent! redraw
	silent! execute 'au BufUnload <buffer> execute bufwinnr(' . bufnr('#') . ') . ''wincmd w'''
	silent! execute 'nnoremap <silent> <buffer> <LocalLeader>r :call <SID>ExecuteInShell(''' . command . ''')<CR>'
	echo 'Shell command ' . command . ' executed.'
endfunction
command! -complete=shellcmd -nargs=+ Shell call s:ExecuteInShell(<q-args>)
