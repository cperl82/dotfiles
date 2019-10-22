#!/bin/bash

i3bar_pre () {
    printf "{\"version\":1}\n"
    printf "[\n"
}

insert_json_at_position () {
    local array="${1}"
    local position="${2}"
    local json="${3}"
    local program=""

    read -d '' -r program <<-'EOF'
	map(select(.full_text != "")) | .[0:$idx] + [$json] + .[$idx:]
	EOF

    jq -rc --argjson json "${json}" --argjson idx "${position}" "${program}" <<< "${array}"
}

main () {
    local dir=""
    local plugin_dir=""
    local output=""
    local template=""
    local program=""

    dir=$(dirname "${0}")
    plugin_dir="${dir}/../plugin.d"

    i3bar_pre
    while read -r json
    do
	while read -r -d '' plugin
	do
	    test -x "${plugin}" || continue
	    read -r type position output < <("${plugin}")
	    case "${type}" in
		 PLUGIN_OUTPUT_TEXT)
		     read -d '' -r template <<-'EOF'
			{"name": "", "instance":0, "color":"#FFFFFF", "markup":"none", "full_text":""}
			EOF
		     read -d '' -r program <<-'EOF'
			.name=$name | .full_text=$full
			EOF
		     output=$(jq -rc					\
				 --arg name "$(basename "${plugin}")"	\
				 --arg full "${output}"			\
				 "${program}" <<< "${template}")
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
	    json=$(insert_json_at_position "${json}" "${position}" "${output}")
	done < <(find "${plugin_dir}" -type f -print0 2>/dev/null)

	# Send the output, augmented with plugin output, to i3bar
	printf "%s,\n" "${json}"
    done < <(i3status | jq -c --unbuffered --stream 'fromstream(1|truncate_stream(inputs))')
}

main "${@}"
