" 2011-05-07
if !has("gui")
	" Override some highlight settings for NERD_tree.
	hi link NERDTreeUp  Label
	hi link NERDTreeDir Label
	hi link NERDTreeCWD Macro

	" Override some highlight settings for bufexplorer
	hi link bufExplorerMapping Normal
	hi link bufExplorerSplit Normal
	hi link bufExplorerTitle Normal
	hi link bufExplorerHidBuf Normal
else
	hi link NERDTReeDir Statement
endif

