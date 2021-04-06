#!/bin/bash

set -o pipefail
set -o errexit

function window-query {
    cat <<-'EOF'
	.nodes
	| map(select(.type? == "output"))
	| map(
	    select(.name? != "__i3")
	    | .nodes
	    | map(select(.type? != "dockarea"))
	    | map(.nodes)
	    | flatten(1)
	    | map(
	       .name as $workspace
	       | [.. | select(.nodes? == [] and .floating_nodes == [] and .focused == false)]
	       | map([(.id | tostring), $workspace, (.window_properties | .class), .name]))
	    | flatten(1)
	    | map(@tsv)
	    | .[]
	  ) as $workspace_windows
	| map(
	    select(.name? == "__i3")
	    | .nodes
	    | map(select(.type? == "con"))
	    | map(.nodes)
	    | flatten(1)
	    | map(select(.name? == "__i3_scratch"))
	    | .[0]
	    | .floating_nodes
	    | map(.nodes)
	    | flatten(1)
	    | map([ (.id | tostring)
	          , "S"
		  , (.window_properties | .class // "Container")
	          , ([.. | select(.nodes? == [] and .floating_nodes? == [])]
		     | map(.name)
		     | join(", "))])
	    | map(@tsv)
	    | .[]
	  ) as $scratchpad_windows
	| [ $scratchpad_windows, $workspace_windows ]
	| flatten(1)
	| .[]
	EOF
}

function workspace-query {
    cat <<-'EOF'
        [ .. | select(.type? == "workspace") ]
        | map(select(.name != "__i3_scratch"))
        | map({id: (.id | tostring), name: .name})
        | map(join(" "))
        | .[]
	EOF
}

function subcmd--find-window {
    local tree
    local q

    tree=$(i3-msg -t get_tree)
    q=$(window-query)

    jq -r "${q}" <<< "${tree}"					\
        | sed -e 's/ - Google Chrome$//'			\
        | column -t -s$'\t'					\
	| sort -k 2,3 -Vr					\
        | fzf --with-nth=2.. --border				\
        | awk '{print $1}'					\
        | xargs -n1 -I{} i3-msg -t command "[con_id={}] focus"
}

function subcmd--jump-to-workspace {
    local tree
    local q

    tree=$(i3-msg -t get_tree)
    q=$(workspace-query)

    jq -r "${q}" <<< "${tree}"                                  \
        | fzf --with-nth=2.. --border                           \
        | awk '{print $1}'                                      \
        | xargs -n1 -I{} i3-msg -t command "[con_id={}] focus"
}

function main {
    local subcmd_prefix="subcmd"
    local completions
    local subcmd

    completions=$(
	declare -F | awk '$2 ~ /^-f$/ {print $NF}' | sed -ne "s/^${subcmd_prefix}--//p")

    function usage {
	subcmds=$(echo "${completions}" | xargs | tr ' ' '|')
	printf "%s %s\n" "${0}" "${subcmds}" 1>&2
    }

    while [[ ${#} -gt 0 ]]
    do
        case "${1}" in
            -*)
                usage
                exit 1
                ;;
            *)
                break
                ;;
        esac
    done

    mapfile -t subcmd < <(compgen -W "${completions}" -- "${1}" || true)

    if [[ ${#subcmd[@]} -eq 1 ]]
    then
        shift
        "${subcmd_prefix}--${subcmd[0]}" "$@"
    else
        usage
        exit 1
    fi
}

main "${@}"
