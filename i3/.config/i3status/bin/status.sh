#!/bin/bash

function i3bar_prolog {
    printf "{\"version\":1}\n"
    printf "[\n"
}

function main
{
    local first=1
    local dir=""
    local output=""

    dir=$(dirname "${0}")

    i3bar_prolog
    while read -r json
    do
	for plugin in $(find "${dir}/plugin.d" -type f -print 2>/dev/null)
	do
	    test -x "${plugin}" || continue
	    read -r type position output < <("${plugin}")
	    case "${type}" in
		 PLUGIN_OUTPUT_TEXT)
		     read -r -d '' output <<-EOF
			{ "name":      "${plugin}"
			, "instance":  "0"
			, "color":     "#FFFFFF"
			, "markup":    "none"
			, "full_text": "${output}"
			}
			EOF
		 ;;

		 PLUGIN_OUTPUT_JSON)
		     # Do nothing, its already supposed to be formatted
		     :
		 ;;

		 *)
		     # Skip
		     continue
		 ;;
	    esac

	    # Insert the json from the plugin into the appropriate
	    # spot in the array that will be consumed by i3bar
	    json=$(jq -rc								\
		      --argjson output "${output}"					\
		      --arg     idx "${position}"					\
		      'map(select(.full_text != ""))
		       | .[0:($idx | tonumber)] + [$output] + .[($idx | tonumber):]'	\
		      <<< "${json}")
	done

	# Send the output, augmented with plugin output, to i3bar
	printf "%s,\n" "${json}"
    done < <(i3status | jq -c --unbuffered --stream 'fromstream(1|truncate_stream(inputs))')
}

main "${@}"
