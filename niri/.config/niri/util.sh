#!/bin/bash

set -o pipefail
set -o errexit

select-window () {
    local id
    local workspaces="${1}"
    local windows="${2}"
    local filter="${3}"
    local title="${4}"

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
 	  | map({"key": (.id | tostring), "value": .idx})
	  | from_entries as \$ws
	| \$windows
	  | map(select(${filter}))
	  | map([.id, \$ws[(.workspace_id | tostring)], .app_id, .title])
	  | .[]
	  | @tsv
	EOF

    id=$(jq -r -s "${q}"				\
	    <(echo "${workspaces}")			\
	    <(echo "${windows}")			\
             | mangle_names				\
	     | extract_name_context                     \
             | sort -k 2,2n -k 3,3V -k 4,4V		\
             | prepend_header				\
             | column -t -s$'\t'			\
             | fzf --border-label="[ ${title} ]"	\
		   --accept-nth='{1}'			\
		   --with-nth=2..			\
		   --border				\
		   --header-first			\
		   --header-lines 1			\
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
    niri msg action move-window-to-workspace	\
	 --window-id "${wid}"			\
	 --focus false				\
	 "${lwsid}"
    niri msg action move-window-to-tiling --id "${wid}"
    niri msg action set-window-width "50%" --id "${wid}"
}

subcmd--select-and-pull-window-from-last-workspace () {
    local workspaces
    local windows
    local id
    local lwsid
    local cwsid

    read -d '' -r curr_and_last_workspace_query<<-'EOF' || true
	sort_by(.idx)
	| (map(select(.is_active and .is_focused)) | .[0]) as $c
	| [($c | .idx), (.[-2] | .id)]
	| @tsv
	EOF

    workspaces=$(niri msg -j workspaces)
    windows=$(niri msg -j windows)
    read -r cwsid lwsid < \
	 <(jq -r "${curr_and_last_workspace_query}" <<< "$workspaces")
    id=$(select-window					\
	     "${workspaces}"				\
	     "${windows}"				\
	     ".workspace_id == ${lwsid}"		\
	     "Pull Window")
    niri msg action move-window-to-floating --id "${id}"
    niri msg action set-window-height 75% --id "${id}"
    niri msg action set-window-width 50% --id "${id}"
    niri msg action move-window-to-workspace --window-id "${id}" "${cwsid}"
    niri msg action center-window --id "${id}"
    niri msg action focus-window --id "${id}"
}

subcmd--select-and-focus-window () {
    local workspaces
    local windows
    local id
    workspaces=$(niri msg -j workspaces)
    windows=$(niri msg -j windows)
    id=$(select-window				\
	     "${workspaces}"			\
	     "${windows}"			\
	     "."				\
	     "Focus Window")
    niri msg action focus-window --id "${id}"
}

subcmd--move-columns-workspace-down () {
    :
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
