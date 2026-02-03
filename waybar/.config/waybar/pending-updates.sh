#!/bin/bash

# Expected to be invoked by waybar with a reasonable `interval'

set -o errexit
set -o pipefail
set -o nounset

# CR cperl: This doesn't work well with a laptop as it can wind up
# getting invoked when the network is not yet up (after resume) and
# it'll error out in weird ways that cause the number of updates to
# note display.

# CR cperl: Allow the individual commands to return either a number
# (representing the number of pending updates) or a string if some
# kind of an error occurred. Check for he return type via regex in the
# main function and only update the total if the returned value is
# numeric, but display whatever is returned in the tooltip.

pending_updates_opam () {
    if ! command -v opam > /dev/null; then
        echo "0"
	return
    fi
    echo "0"
}

pending_updates_python () {
    if ! command -v pip > /dev/null; then
        echo "0"
	return
    fi

    pip list --user --outdated \
        | awk 'END {if (NR > 2) {print NR-2} else {print 0}}'
}

pending_updates_cargo () {
    if ! command -v cargo > /dev/null; then
        echo "0"
	return
    fi
    cargo install-update --list \
        | awk 'BEGIN {
                 count = 0;
               };
               $2 ~ /^v/ &&
               $3 ~ /^v/ &&
               $4 ~ /^[Yy][Ee][Ss]$/ {
                 count += 1;
               };
               END {
                 print count;
               };'
}

pending_updates_flatpak () {
    if ! command -v flatpak > /dev/null; then
        echo "0"
	return
    fi
    set +o errexit
    flatpak update < <(printf "n\n") \
        | grep -Ec '^[ ]+[0-9]+\.'
    set -o errexit
}

pending_updates_apt () {
    apt list --upgradable 2>/dev/null \
	| awk 'BEGIN {
	         count = 0
	       };
	       $1 ~ /\// {
                 count += 1
               };
               END {
                 print count
               }'
}

pending_updates_dnf () {
    # CR cperl: This is fedora specific right now, which isn't great
    dnf -q updateinfo list \
        | awk 'BEGIN {
                 count = 0
               };
               $0 ~ /\.fc[0-9][0-9]*\./ {
                 count += 1
               }
               END {
                 if (NR > 2) {
                   print NR-1
                 } else {
                   print 0
                 }
               };'
}

pending_updates_package_manager () {
    if command -v dnf > /dev/null; then
        pending_updates_dnf
    elif command -v apt > /dev/null; then
        pending_updates_apt
    else
        echo "0"
    fi

}

main () {
    local package_manager
    local flatpak
    local cargo
    local python
    local opam
    local all
    local text
    local tooltip

    package_manager=$(pending_updates_package_manager)
    flatpak=$(pending_updates_flatpak)
    cargo=$(pending_updates_cargo)
    python=$(pending_updates_python)
    opam=$(pending_updates_opam)
    all=$((package_manager + flatpak + cargo + python + opam))
    printf -v text "%d" "${all}"
    printf -v tooltip "%s\\\\n"                                 \
           "$(printf "Packages: %3d" "${package_manager}")"     \
           "$(printf "Flatpak:  %3d" "${flatpak}")"             \
           "$(printf "Cargo:    %3d" "${cargo}")"               \
           "$(printf "Python:   %3d" "${python}")"              \
           "$(printf "Opam:     %3d" "${opam}")"
    tooltip="${tooltip%\\n}"
    printf '{"text": "%s", "tooltip": "%s"}\n'  \
           "${text}"                            \
           "${tooltip}"
}

main "${@}"
