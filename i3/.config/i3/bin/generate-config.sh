#!/bin/bash

config_base="${HOME}/.config/i3"

function generate_workspace_to_output_mappings {
    local monitors=()
    local idx=0

    # Order all monitors according to their left to right and top to
    # bottom order and then bind the first 10 workspaces to those
    # outputs in order.
    monitors=( $(xrandr											\
		     | grep -E '\bconnected\b'								\
		     | sed -re 's/^([^ ]+).* [0-9]+x[0-9]+([+-][0-9]+)([+-][0-9]+) .*$/\2 \3 \1/'	\
		     | sort -n -k 1,2									\
		     | awk '{print $3}') )

    for i in {1..10}
    do
	idx=$(( i % ${#monitors[@]} ))
	printf "workspace ${i} output ${monitors[${idx}]}\n"
    done
}

function main
{
    generate_workspace_to_output_mappings > "${config_base}/config.generated"
    cat   "${config_base}/config.base"          \
          "${config_base}/config.local"         \
          "${config_base}/config.generated"     \
        > "${config_base}/config" 2>/dev/null
}

main
