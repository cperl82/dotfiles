" Fixup for the bindzone.vim distrubuted with vim
" Removing "*" from the set of NOT allowed characters to start the line
" as things like 
" *.blogs.lightning       IN      A       xxx.xxx.xx.xxx
" are perfectly valid with bind9
syn match zoneOwnerName      contained  /[^[:space:]!"#$%&'()+,\/:;<=>?@[\]\^`{|}~]\+\(\s\|;\|$\)\@=/
