#!/bin/bash

function main
{
    i3status | while read -r status
    do
	echo "${status}"
    done
}

main "${@}"
