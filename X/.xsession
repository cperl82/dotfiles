#!/bin/bash

# 2016-12-11 cperl: just source xinitrc
xinitrc="${HOME}/.xinitrc"
if [[ -e "${xinitrc}" ]]
then
	source "${xinitrc}"
fi
