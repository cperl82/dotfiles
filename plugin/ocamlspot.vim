let s:ocamlspot = "/janelibs/ocaml-4.00.1+jane3/bin/ocamlspot.opt"

function! s:OcamlSpotCommand()
	let buf    = bufname("%")
	let path   = fnamemodify(buf, ":p")
	let pos    = getpos(".")
	let line   = pos[1]
	let column = pos[2]
	let cmd =  printf("%s %s:l%dc%d", s:ocamlspot, path, line, column)	

	echo printf("Command: %s", cmd)

	return cmd
endfunction

function! s:Spot()
	let cmd = s:OcamlSpotCommand()
	let out = system(cmd)
	echo out

	let foundlist = matchlist(out, '\vSpot:\s+\<(.{-}):l(\d+)c(\d+)b(\d+).*\>\n')
	if !empty(foundlist)
		let path = foundlist[1]
		let line = foundlist[2]
		let colu = foundlist[3]

		"TODO: can't seem to figure out how to use keepjumps here to
		"not add the top of the jumped to file to the jump list
		exec printf("edit +%d %s", line, path)
		return
	endif
	echo "Spot not found"
endfunction

function! s:Type()
	let cmd = s:OcamlSpotCommand()
	let out = system(cmd)
	echo out

	let foundlist = matchlist(out, '\vVal:\s+(\_.{-})%(\n\S+|$)@=')
	if !empty(foundlist) && len(foundlist) >= 2
		echo foundlist[1]
		return
	endif

	let found = matchstr(out, '\v%(Type|XType):\s+\_.{-}%(\n\S+)@=')
	if !empty(found)
		echo found
		return
	endif
	echo "Type not found"
endfunction

command! Spot :call s:Spot()
command! Type :call s:Type()
