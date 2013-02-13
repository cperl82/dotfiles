let s:ocamlspot = "/janelibs/ocaml-4.00.1+jane3/bin/ocamlspot.opt"

function! s:SpotCommand()
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
	let cmd = s:SpotCommand()
	let out = system(cmd)
	echo out

	let foundlist = matchlist(out, '\vSpot:\s+\<(.{-}):l(\d+)c(\d+)b(\d+).*\>\n')
	if !empty(foundlist)
		let path = foundlist[1]
		let line = foundlist[2]
		let colu = foundlist[3]
		exec printf("edit +%d %s", line, path)
		return
	endif
	echo "Spot not found"
endfunction

function! s:Type()
	let cmd = s:SpotCommand()
	let out = system(cmd)
	echo out

	let found = matchstr(out, '\v%(Type|XType):\s+\_.{-}%(\n\S+:)@=')
	if !empty(found)
		echo found
		return
	endif
	echo "Type not found"
endfunction

command! Spot :call s:Spot()
command! Type :call s:Type()
