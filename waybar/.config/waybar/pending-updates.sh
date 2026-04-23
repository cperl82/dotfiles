#!/bin/bash

# Expected to be invoked by waybar with a reasonable `interval'

set -o pipefail
set -o nounset

pending_updates_package_manager () {
    if command -v dnf > /dev/null; then
        pending_updates_dnf
    elif command -v apt > /dev/null; then
        pending_updates_apt
    else
        echo "_"
    fi
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
    dnf -q updateinfo list \
        | awk 'BEGIN {
                 count = 0
               };
               $5 ~ /^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$/ {
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

pending_updates_flatpak () {
    if ! command -v flatpak > /dev/null; then
        echo "_"
	return 0
    fi
    set +o errexit
    yes "n"					\
	| flatpak update			\
        | grep -Ec '^[ ]+[0-9]+\.'
    set -o errexit
}

pending_updates_cargo () {
    if ! command -v cargo install-update > /dev/null; then
        echo "_"
	return 0
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

pending_updates_rustup () {
    local rustup_init=""

    if ! command -v rustup > /dev/null; then
	echo "_"
	return 0
    fi

    # We're trying to determine if updates listed for rustup itself
    # should be counted. If rustup is installed via the system package
    # manager then they shouldn't, else they should. We attempt to
    # determine if rustup is install by the system package manager by
    # seeing where `rustup-init' is installed. If it's in our home
    # directory, then it's not installed by the system package manager
    # and updates to rustup should count.
    rustup_init=$(which rustup-init)
    if [[ "${rustup_init}" =~ ^${HOME}/ ]]; then
	rustup check | grep -c Update
    else
	rustup check | grep -v "^rustup " | grep -c Update
    fi
}

pending_updates_python () {
    if ! command -v pip > /dev/null; then
        echo "_"
	return 0
    fi

    pip list --user --outdated \
        | awk 'END {if (NR > 2) {print NR-2} else {print 0}}'
}

pending_updates_opam () {
    if ! command -v opam > /dev/null; then
        echo "_"
	return 0
    fi
    opam update >/dev/null 2>&1
    yes "n"									\
	| opam upgrade --dry-run						\
	| sed -n -e 's/^Proceed with.* \([0-9][0-9]*\) upgrades?.*$/\1/p'
}

pending_updates_npm () {
    if ! command -v npm > /dev/null; then
	echo "_"
	return 0
    fi
    npm outdated -g --depth=0 --json \
	| jq -r 'keys | length'
}

run () {
    local package_manager
    local flatpak
    local cargo
    local rustup
    local python
    local opam
    local npm
    local all
    local text
    local tooltip

    package_manager=$(pending_updates_package_manager)
    flatpak=$(pending_updates_flatpak)
    cargo=$(pending_updates_cargo)
    rustup=$(pending_updates_rustup)
    python=$(pending_updates_python)
    opam=$(pending_updates_opam)
    npm=$(pending_updates_npm)

    if [[ -z "${package_manager}" || \
	  -z "${flatpak}"         || \
	  -z "${cargo}"           || \
	  -z "${rustup}"          || \
	  -z "${python}"          || \
	  -z "${opam}"            || \
	  -z "${npm}"
	]]; then
	return 1
    fi

    all=0
    for thing in package_manager flatpak cargo rustup python opam npm; do
	if [[ "${!thing}" =~ ^[0-9]+$ ]]; then
	    all=$((all + ${!thing}))
	fi
    done
    printf -v text "%d" "${all}"
    printf -v tooltip "%s\\\\n"                                 \
           "$(printf "Packages: %3s" "${package_manager}")"     \
           "$(printf "Flatpak:  %3s" "${flatpak}")"             \
           "$(printf "Cargo:    %3s" "${cargo}")"               \
           "$(printf "Rustup:   %3s" "${rustup}")"              \
           "$(printf "Python:   %3s" "${python}")"              \
           "$(printf "Opam:     %3s" "${opam}")"                \
           "$(printf "Npm:      %3s" "${npm}")"
    tooltip="${tooltip%\\n}"
    printf '{"text": "%s", "tooltip": "%s"}\n'  \
           "${text}"                            \
           "${tooltip}"
    return 0
}

main () {
    local output
    local i

    # Try 5 times, sleeping 5s between each attempt, to deal with
    # things like the computer having just been woken up from sleep
    # and the network being disconnected. It's possible you should
    # cache and display the cached version if you can't get a new
    # update.
    i=5
    while [[ "${i}" -gt 0 ]]; do
	if output=$(run); then
	    echo "${output}"
	    break
	fi
	sleep 5
	i=$((i - 1))
    done
}

main
