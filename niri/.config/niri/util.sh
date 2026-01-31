#!/bin/bash

set -o pipefail
set -o errexit

select-window () {
    local id
    local title="${1}"
    local workspaces="${2}"
    local windows="${3}"
    local filter="${4}"
    local last_first="${5}"

    function mangle_names {
        # Fixup various things about window titles
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
           gsub(/ (-|\xe2\x80\x94) [[:alnum:][:space:]]+$/, "", $4);

           print
        }'
    }

    function extract_name_context {
        # If a window title starts with [Foo] extract that as a separate field
        awk -F'\t' \
        '{
           OFS="\t"

           if (where = match($4, /^\[[^\]]+\] /)) {
               $5 = substr($4, RLENGTH+1)
               $4 = substr($4, where+1, RLENGTH-3)
           }
           else {
               $5 = $4
               $4 = ""
           }

           print
        }'
    }

    function prepend_header {
        printf "%s\t" "Id" "W" "App" "Context" "Title" \
            | sed -e 's/\t$/\n/'
        cat
    }

    read -d '' -r q <<-EOF || true
	  .[0] as \$workspaces
	| .[1] as \$windows
	| \$workspaces
	  | sort_by(.idx)
	  | .[-2]
	  | .idx
	  | tostring as \$last_workspace
	| \$workspaces
	  | map({"key": (.id | tostring),
	         "value": (.idx | tostring | sub("^" + \$last_workspace + "\$"; "L"))})
	  | from_entries as \$ws
	| \$windows
	  | map(select(${filter}))
	  | map([.id, \$ws[(.workspace_id | tostring)], .app_id, .title])
	  | .[]
	  | @tsv
	EOF

    if (( last_first )); then
        workspace_keydef="2,2n"
    else
        workspace_keydef="2,2"
    fi

    id=$(jq -r -s "${q}"                                        \
            <(echo "${workspaces}")                             \
            <(echo "${windows}")                                \
             | mangle_names                                     \
             | extract_name_context                             \
             | sort -k "${workspace_keydef}" -k 3,3V -k 4,4V    \
             | prepend_header                                   \
             | column -t -s$'\t'                                \
             | fzf --border-label="[ ${title} ]"                \
                   --accept-nth='{1}'                           \
                   --with-nth=2..                               \
                   --border                                     \
                   --header-first                               \
                   --header-lines 1                             \
                   --layout reverse)
    if [[ -z "${id}" ]]; then
        return 1
    else
        echo "${id}"
    fi
}

subcmd--move-window-to-last-workspace () {
    local workspaces
    local windows
    local lwsid
    local wid

    workspaces=$(niri msg -j workspaces)
    windows=$(niri msg -j windows)

    read -d '' -r last_workspace_query<<-EOF || true
	sort_by(.idx) | .[-2] | .idx
	EOF
    read -d '' -r curr_window_query<<-EOF || true
	map(select(.is_focused) | .id) | .[0]
	EOF

    lwsid=$(jq -r "${last_workspace_query}" <<< "${workspaces}")
    wid=$(jq -r "${curr_window_query}" <<< "${windows}")

    if [[ -z "${lwsid}" || -z "${wid}" ]]; then
        return 1
    fi
    niri msg action move-window-to-workspace    \
         --window-id "${wid}"                   \
         --focus false                          \
         "${lwsid}"
    niri msg action move-window-to-tiling --id "${wid}"
    niri msg action set-window-width "50%" --id "${wid}"
}

subcmd--select-and-pull-window () {
    local workspaces
    local windows
    local id
    local lwsid
    local cwsid
    local last_first

    read -d '' -r curr_workspace_query<<-'EOF' || true
	sort_by(.idx)
	| map(select(.is_active and .is_focused))
	| .[0]
	| .idx
	EOF

    workspaces=$(niri msg -j workspaces)
    windows=$(niri msg -j windows)
    cwsid=$(jq -r "${curr_workspace_query}" <<< "${workspaces}")
    last_first=1
    id=$(select-window                          \
             "Select Window to Pull"            \
             "${workspaces}"                    \
             "${windows}"                       \
             "."                                \
             "${last_first}")
    niri msg action move-window-to-floating --id "${id}"
    niri msg action set-window-height 75% --id "${id}"
    # CR cperl: This is a hack.
    #
    # When this function is done I want the window to be floating and
    # the right size to take up exactly half the screen when "dropped"
    # back to tiling.
    #
    # But, gaps mean "50%" is different for floating vs tiled
    # windows. So, we set the size to 50%, which is just slightly too
    # big, then we manually subtract (1.5 * gap_size), which happens to
    # be 10. That represents the full width of the gap on the left (or
    # right) and then half the middle gap).
    niri msg action set-window-width 50% --id "${id}"
    niri msg action set-window-width -15 --id "${id}"
    niri msg action move-window-to-workspace --window-id "${id}" "${cwsid}"
    niri msg action center-window --id "${id}"
    niri msg action focus-window --id "${id}"
}

subcmd--select-and-focus-window () {
    local workspaces
    local windows
    local id
    local last_first

    workspaces=$(niri msg -j workspaces)
    windows=$(niri msg -j windows)
    last_first=0
    id=$(select-window                          \
             "Select Window to Focus"           \
             "${workspaces}"                    \
             "${windows}"                       \
             ".is_focused | not"                \
             "${last_first}")
    niri msg action focus-window --id "${id}"
}

function main {
    local subcmd_prefix="subcmd"
    local completions
    local subcmd

    completions=$(
        declare -F                                      \
            | awk '$2 ~ /^-f$/ {print $NF}'             \
            | sed -ne "s/^${subcmd_prefix}--//p")

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

    if [[ -n "${1}" && ${#subcmd[@]} -eq 1 ]]; then
        shift
        "${subcmd_prefix}--${subcmd[0]}" "$@"
    else
        usage
        exit 1
    fi
}

main "${@}"
