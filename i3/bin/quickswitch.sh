#!/bin/bash

set -o pipefail
set -o errexit

function usage {
    echo "${0} restore-from-scratchpad|jump-to-window|jump-to-workspace" 1>&2
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
	| sort -k 2								\
	| fzf --with-nth=2.. --border --multi --no-sort				\
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

function workspace-query {
    cat <<-'EOF'
	[ .. | select(.type? == "workspace") ]
	| map(select(.name != "__i3_scratch"))
	| map({id: (.id | tostring), name: .name})
	| map(join(", "))
	| .[]
	EOF
}

function restore-from-scratchpad-cmd {
    local tree
    local q

    tree=$(i3-msg -t get_tree)
    q=$(scratchpad-window-query)

    jq -r "${q}" <<< "${tree}"							\
	| fzf --with-nth=2.. --border --multi --no-sort				\
	| sort -k 2								\
	| awk -F, '{print $1}'							\
	| xargs -n1 -I{} i3-msg -t command "[con_id={}] scratchpad show"
}

function jump-to-window-cmd {
    local tree
    local q

    tree=$(i3-msg -t get_tree)
    q=$(non-scratchpad-window-query)

    jq -r "${q}" <<< "${tree}"						\
	| fzf --with-nth=2.. --border					\
	| awk -F, '{print $1}'						\
	| xargs -n1 -I{} i3-msg -t command "[con_id={}] focus"
}

function jump-to-workspace-cmd {
    local tree
    local q

    tree=$(i3-msg -t get_tree)
    q=$(workspace-query)

    jq -r "${q}" <<< "${tree}"						\
	| fzf --with-nth=2.. --border					\
	| awk -F, '{print $1}'						\
	| xargs -n1 -I{} i3-msg -t command "[con_id={}] focus"

}

function main {
    local cmds

    cmds=( $(compgen -A function -- "${1}" | grep -- "-cmd$") )
    shift

    if [[ ${#cmds[@]} -eq 1 ]]
    then
        ${cmds[0]} "$@"
    else
        usage
        exit 1
    fi
}

main "${@}"
