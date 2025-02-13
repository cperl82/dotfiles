#!/bin/bash

set -o pipefail
set -o errexit

function window-query {
    cat <<-'EOF'
	.nodes
	| map(select(.type? == "output"))
	| map(.. | select(.type? == "workspace"))
	| map(
	    (if .name == "__i3_scratch" then "S" else .name end) as $workspace
	    | [.nodes?, .floating_nodes? ]
	    | flatten
	    | [.. | select(.nodes? == [] and .floating_nodes? == [] and .focused == false)]
	    | map(
	        [ .id
	        , $workspace
	        , (if .app_id? then .app_id else (.window_properties? | .class) end)
	        , .name
		]))
	| .[]
	| .[]
	| [ . ]
	| map(@tsv)
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

    function mangle_names {
        # 1Password doesn't use an ASCII "-", they use the utf-8 8212 code point, EM DASH
        # shellcheck disable=SC2016
        sed -e 's/ - Google Chrome$//'                                          \
            -e 's/ \xe2\x80\x94 1Password$//'                                   \
            -e 's/org.cryptomator.launcher.Cryptomator$MainApp/Cryptomator/'
    }

    tree=$(i3-msg -t get_tree)
    q=$(window-query)

    jq -r "${q}" <<<"${tree}"                                   \
        | mangle_names                                          \
        | column -t -s$'\t'                                     \
        | sort -k 2,2n -k 3,3Vr -k 4,4Vr                        \
        | fzf --with-nth=2.. --border                           \
        | awk '{print $1}'                                      \
        | xargs -I{} i3-msg -t command "[con_id={}] focus"
}

function subcmd--jump-to-workspace {
    local tree
    local q

    tree=$(i3-msg -t get_tree)
    q=$(workspace-query)

    jq -r "${q}" <<<"${tree}"                                   \
        | fzf --with-nth=2.. --border                           \
        | awk '{print $1}'                                      \
        | xargs -I{} i3-msg -t command "[con_id={}] focus"
}

function main {
    local subcmd_prefix="subcmd"
    local completions
    local subcmd

    completions=$(
        declare -F | awk '$2 ~ /^-f$/ {print $NF}' | sed -ne "s/^${subcmd_prefix}--//p"
    )

    function usage {
        subcmds=$(echo "${completions}" | xargs | tr ' ' '|')
        printf "%s %s\n" "${0}" "${subcmds}" 1>&2
    }

    while [[ ${#} -gt 0 ]]; do
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

    if [[ ${#subcmd[@]} -eq 1 ]]; then
        shift
        "${subcmd_prefix}--${subcmd[0]}" "$@"
    else
        usage
        exit 1
    fi
}

main "${@}"
