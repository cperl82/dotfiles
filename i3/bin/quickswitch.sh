#!/bin/bash

set -o pipefail
set -o errexit

function usage {
    echo "${0} restore-from-scratchpad|jump-to-window" 1>&2
}

function scratchpad-window-query {
    cat <<-'EOF'
	  ..
	| select(.name? == "__i3_scratch")
	| .floating_nodes
	| map(.nodes | .[0])
	| [ (map(select(.orientation != "none")) | map({id: .id, windows: .nodes | map(.name)}))
	  , (map(select(.orientation == "none")) | map({id: .id, windows: [.name]}            ))
	  ]
	| add
	| map([(.id | tostring), (.windows | join(", "))])
	| map(join(", "))
	| .[]
	EOF
}

function restore-from-scratchpad {
    local tree=""
    local q=""
    
    tree=$(i3-msg -t get_tree)
    q=$(scratchpad-window-query)
    jq -r "${q}" <<< "${tree}"							\
	| fzf --with-nth=2.. --border --multi					\
	| awk -F, '{print $1}'							\
	| xargs -n1 -t -I{} i3-msg -t command "[con_id={}] scratchpad show"
}

function non-scratchpad-window-query {
    cat <<-'EOF'
	  ..
	| select(.type? == "output")
	| select(.name? != "__i3")
	| .nodes
	| map(select(.type? != "dockarea"))
	| [ .. | select(.nodes? == [] and .floating_nodes == []) ]
	| map([(.id | tostring), .name])
	| map(join(", "))
	| .[]
	EOF
}

function jump-to-window {
    local tree=""
    local q=""
    
    tree=$(i3-msg -t get_tree)
    q=$(non-scratchpad-window-query)
    jq -r "${q}" <<< "${tree}"							\
	| fzf --with-nth=2.. --border						\
	| awk -F, '{print $1}'							\
	| xargs -n1 -t -I{} i3-msg -t command "[con_id={}] focus"
}

function main {
    # CR-someday cperl: You could proably figure out some fancy
    # partial completion with compgen
    case "${1}" in
	restore-from-scratchpad)
	    f=restore-from-scratchpad
	    ;;
	jump-to-window)
	    f=jump-to-window
	    ;;
	*)
	    usage
	    exit 1
    esac

    "${f}"
}

main "${@}"
