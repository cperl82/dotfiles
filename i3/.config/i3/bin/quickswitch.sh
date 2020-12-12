#!/bin/bash

set -o pipefail
set -o errexit

function usage {
    echo "${0} jump-to-window-or-restore-from-scratchpad" 1>&2
}

function jump-to-window-or-restore-from-scratchpad {
    local window
    local candidate
    local candidates
    local selected
    local id
    local name
    local desktop
    local class
    local title
    local class_w
    local desktop_w
    local wmctrl
    local desktop_id_to_name

    declare -A desktop_id_to_name

    wmctrl_d=$(wmctrl -d)
    wmctrl_w=$(wmctrl -lx)

    # Integer to desktop name and width
    while read -r id _ _ _ _ _ _ _ name
    do
        if [[ ${#name} -gt "${desktop_w}" ]]
        then
            desktop_w=${#name}
        fi
        desktop_id_to_name["${id}"]="${name}"
    done < <(echo "${wmctrl_d}")

    # Class width
    while read -r _ _ class _ _
    do
        class="${class##*.}"
        if [[ ${#class} -gt "${class_w}" ]]
        then
            class_w=${#class}
        fi
    done < <(echo "${wmctrl_w}")

    printf -v this_window "0x%08x" "$(xdotool getactivewindow)"
    while read -r id desktop class _ title
    do
        if [[ "${id}" == "${this_window}" ]]
        then
            continue
        fi

        # Ensure we have a title
        if [[ -z "${title}" ]]
        then
            title="${class}"
        fi

        # Shorten the class
        class="${class##*.}"

        # Get the proper desktop name
        if [[ "${desktop}" -eq -1 ]]
        then
            desktop="S"
        else
            desktop="${desktop_id_to_name[${desktop}]}"
        fi

        # Strip the redundant " - Google Chrome" from the end of
        # Chrome windows since we're already showing the window
        # class
        if [[ "${title}" =~ \ -\ Google\ Chrome$ ]]
        then
            title="${title/ - Google Chrome/}"
        fi

        printf -v candidate                     \
               "%s %-*s  %-*s  %s\n"            \
               "${id}"                          \
               "${desktop_w}"                   \
               "${desktop}"                     \
               "${class_w}"                     \
               "${class}"                       \
               "${title}"
        candidates+="${candidate}"
    done < <(echo "${wmctrl_w}")

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
