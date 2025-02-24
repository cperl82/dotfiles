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
	        , .app_id? // (.window_properties? | .class?)
	        , (.marks | join(","))
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
        awk -F'\t' \
        '{
            OFS="\t"

            # Remove leading dot separated components, e.g. turn
            # org.mozilla.firefox into firefox
            gsub(/^([^.]+\.)+/, "", $3);

            # Remove trailing dollar separate components, e.g. turn
            # Cryptomator$MainApp to Cryptomator
            gsub(/([$][^$]+)+$/, "", $3);

            # Attempt to upper case first letter of app name
            $3 = toupper(substr($3, 1, 1)) substr($3, 2)

            # Remove from "-" or EM DASH (Unicode point 8212) to the
            # end of the line
            gsub(/ (-|\xe2\x80\x94) [[:alnum:][:space:]]+$/, "", $5);

            print
        }'
    }

    function prepend_header {
        printf "%s\t" "Dummy" "W" "App" "Marks" "Title" \
            | sed -e 's/\t$/\n/'
        cat
    }

    tree=$(i3-msg -t get_tree)
    q=$(window-query)

    jq -r "${q}" <<<"${tree}"                                   \
        | mangle_names                                          \
        | sort -k 2,2nr -k 3,3V -k 4,4V                         \
        | prepend_header                                        \
        | column -t -s$'\t'                                     \
        | fzf                                                   \
              --bind 'enter:become(echo {1})'                   \
              --with-nth=2..                                    \
              --border                                          \
              --header-first                                    \
              --header-lines 1                                  \
              --layout reverse                                  \
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
