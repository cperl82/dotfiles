#!/bin/bash

set -o pipefail
set -o errexit

function usage {
    echo "${0} jump-to-window-or-restore-from-scratchpad" 1>&2
}

function jump-to-window-or-restore-from-scratchpad {
    local window
    local windows
    local candidate
    local candidates
    local selected
    local id
    local name
    local desktop
    local class
    local title
    local len
    local class_w
    local desktop_w
    local wmctrl
    local awk
    local desktop_id_to_name

    declare -A desktop_id_to_name

    read -d '' -r awk <<-'EOF' || true
	$1 == w { next }
	        { gsub(/-1/, "S", $2); gsub(/\..+$/, "", $3); print }
	EOF

    printf -v this_window "0x%08x" "$(xdotool getactivewindow)"
    mapfile -t windows < <(wmctrl -lx | awk -v w="${this_window}" "${awk}")

    # Integer to desktop naming
    desktop_id_to_name['S']="S"
    while read -r id _ _ _ _ _ _ _ name
    do
        desktop_id_to_name["${id}"]="${name}"
    done < <(wmctrl -d)

    # Max width for class and desktop
    for window in "${windows[@]}"
    do
        read -r _ desktop class _ < <(echo "${window}")
        len=${#class}
        if [[ "${len}" -gt "${class_w}" ]]
        then
            class_w="${len}"
        fi

        len=${#desktop}
        if [[ "${len}" -gt "${desktop_w}" ]]
        then
            desktop_w="${len}"
        fi
    done

    # Select the window
    for window in "${windows[@]}"
    do
        read -r id desktop class _ title < <(echo "${window}")

        if [[ -z "${title}" ]]
        then
            title="${class}"
        fi

        # Strip the redundant " - Google Chrome" from the end of
        # Chrome windows since we're already showing the window
        # class
        if [[ "${title}" =~ \ -\ Google\ Chrome$ ]]
        then
            title="${title/ - Google Chrome/}"
        fi

        printf -v candidate                             \
               "%s %-*s  %-*s  %s\n"                    \
               "${id}"                                  \
               "${desktop_w}"                           \
               "${desktop_id_to_name[${desktop}]}"      \
               "${class_w}"                             \
               "${class}"                               \
               "${title}"
        candidates+="${candidate}"
    done
    mapfile -t selected < <(echo "${candidates}" | sort -k 2,3 -V -r | fzf --multi --with-nth=2.. --border)

    # For each window, if it is on the scratchpad, pull it back, else jump to window
    for window in "${selected[@]}"
    do
        read -r id desktop _ _ < <(echo "${window}")
        if [[ "${desktop}" == "S" ]]
        then
            printf -v wmctrl "wmctrl -i -R %s" "${id}"
        else
            printf -v wmctrl "wmctrl -i -a %s" "${id}"
        fi
        eval "${wmctrl}"
    done
}

function subcmd--jump-to-window {
    jump-to-window-or-restore-from-scratchpad
}

function subcmd--restore-from-scratchpad {
    jump-to-window-or-restore-from-scratchpad
}

function subcmd--find-window {
    jump-to-window-or-restore-from-scratchpad
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
