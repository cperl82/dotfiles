#!/bin/bash

set -o pipefail
set -o errexit

function usage {
    echo "${0} restore-from-scratchpad|jump-to-window|jump-to-workspace" 1>&2
}

function scratchpad-window-query {
    cat <<-'EOF'
        .nodes
        | map(select(.name? == "__i3"))
        | .[0]
        | .nodes
        | .[0]
        | .nodes
        | map(select(.name? == "__i3_scratch"))
        | .[0]
        | .floating_nodes
        | map(.nodes)
        | add
        | map({ id: (.id | tostring)
              , windows: [.. | select(.nodes? == [] and .floating_nodes? == [])] | map(.name) | join(", ")})
        | map(join(" "))
        | .[]
	EOF
}

function non-scratchpad-window-query {
    cat <<-'EOF'
          ..
        | select(.type? == "output")
        | select(.name? != "__i3")
        | .nodes
        | map(select(.type? != "dockarea"))
        | [ .. | select(.nodes? == [] and .floating_nodes == [] and .focused == false) ]
        | map([(.id | tostring), .name])
        | map(join(" "))
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

function subcmd--restore-from-scratchpad {
    local tree
    local q

    tree=$(i3-msg -t get_tree)
    q=$(scratchpad-window-query)

    jq -r "${q}" <<< "${tree}"                                                  \
        | sort -k 2                                                             \
        | fzf --with-nth=2.. --border --multi --no-sort                         \
        | awk '{print $1}'                                                      \
        | xargs -n1 -I{} i3-msg -t command "[con_id={}] scratchpad show"
}

function subcmd--jump-to-window {
    local tree
    local q

    tree=$(i3-msg -t get_tree)
    q=$(non-scratchpad-window-query)

    jq -r "${q}" <<< "${tree}"                                  \
        | fzf --with-nth=2.. --border                           \
        | awk '{print $1}'                                      \
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

    completions=$(declare -F | awk '$2 ~ /^-f$/ {print $NF}' | sed -ne "s/^${subcmd_prefix}--//p")
    subcmd=($(compgen -W "${completions}" -- "${1}"))

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
