#!/bin/bash

# This script is meant to check my home directory to ensure that all the various
# symlinks that make up my environment are there.  Its purpose is to sort of
# "audit" a given system to ensure everything is setup the way it should be.

# Note that this script checks symlinks not only for files that are part of this
# repository, but also "vimrc" and "gvimrc" which are a part of a separate
# repository.

# Also note that the convention I use is that the symlink is the same name as
# the file in this repository, but with a "." prepended.  And the symlinks are
# relative
#
# i.e. ${HOME}/.bashrc -> .dotfiles/bashrc

SYMLINKS=( 
	"bash_login"
	"bashrc"
	"hgrc"
	"python_startup.py"
	"screenrc"
	"vimrc"
	"gvimrc"
	"ackrc"
)

for symlink in ${SYMLINKS[@]}
do
	link="${HOME}/.${symlink}"
	if [[ -h "${link}" ]];
	then
		printf "%-5s %-20s %s\n" "OK" "${symlink}" \
			"$(ls -l "${link}" | sed -e 's/^.*\(\.'${symlink}'.*\)$/\1/')"
	else
		printf "%-5s %-20s %s\n" "FAIL" "${symlink}"
	fi
done
