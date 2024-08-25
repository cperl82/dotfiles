#!/bin/bash

set -o pipefail
set -o errexit

# The purpose of this script is to provide an abstraction over
# different terminal emulators for use with i3. E.g. it accepts some
# standard arguments that I can rely on from i3 and then figures out
# how to ask the actual terminal emulator how to do that.

usage () {
    echo "${0} [--title TITLE] [--geometry GEOMETRY] [--execute WHAT]" 2>&1
    exit 1
}

run () {
    local title="${1}"
    local geometry="${2}"
    local execute="${3}"
    local emulator=""

    for emulator in "alacritty" "xterm" "urxvt256c" "urxvt" ""
    do
	if command -v "${emulator}" > /dev/null
	then
	    break
	fi
    done

    case "${emulator}" in
	alacritty)
	    local cmd="alacritty"

	    if [[ -n "${title}" ]]
	    then
		printf -v cmd '%s --title "%s"' "${cmd}" "${title}"
	    fi

	    if [[ -n "${geometry}" ]]
	    then
		local columns="${geometry%%x*}"
		local lines="${geometry##*x}"
		printf -v cmd \
		       '%s -o "window.dimensions={columns=%d, lines=%d}"' \
		       "${cmd}" \
		       "${columns}" \
		       "${lines}"
	    fi

	    if [[ -n "${execute}" ]]
	    then
		printf -v cmd '%s --command %s' "${cmd}" "${execute}"
	    fi
	    ;;

	xterm)
	    local cmd="xterm"

	    if [[ -n "${title}" ]]
	    then
		printf -v cmd '%s -title "%s"' "${cmd}" "${title}"
	    fi

	    if [[ -n "${geometry}" ]]
	    then
		printf -v cmd \
		       '%s -geometry %s' \
		       "${cmd}" \
		       "${geometry}"
	    fi

	    if [[ -n "${execute}" ]]
	    then
		printf -v cmd '%s -e %s' "${cmd}" "${execute}"
	    fi
	    ;;

	*)
	    echo "Unable to find suitable terminal emulator" 2>&1
	    exit 1
	;;
    esac

    eval exec "${cmd}"
}

main () {
    local title=""
    local geometry=""

    while [[ ${#} -gt 0 ]]
    do
	case "${1}" in
	    --title|-t)
		title="${2}"
		shift 2
		;;

	    --geometry|-g)
		if [[ ! ${2} =~ [0-9]+x[0-9]+ ]]
		then
		    usage
		fi
		geometry="${2}"
		shift 2
		;;

	    --execute|-e)
		shift
		execute="${*}"
		break
		;;

	    *)
		usage
		;;
	esac
    done

    run "${title}" "${geometry}" "${execute}"
}

main "${@}"
