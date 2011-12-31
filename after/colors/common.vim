if has("gui_running")
	" 2010-08-25
	" Override the highlight settings for FoldColumn with macvim as I can
	" barely read the default
	hi clear FoldColumn
	hi link FoldColumn Folded

	" 2011-05-24
	" Override the defaults for NERDTree Openable and ExecFile in the gui
	hi link NERDTreeOpenable Identifier
	hi link NERDTreeExecFile Identifier
else
    " Do nothing for now
endif
