#!/bin/bash

set -o errexit
set -o pipefail
set -o nounset

# Expected to be invoked by waybar with a reasonable `interval'
pending_updates_opam () {
    if ! command -v opam > /dev/null; then
        echo "_"
    fi
    echo "0"
}

pending_updates_python () {
    if ! command -v pip > /dev/null; then
        echo "0"
    fi

    pip list --user --outdated \
        | awk 'END {if (NR > 2) {print NR-2} else {print 0}}'
}

pending_updates_cargo () {
    if ! command -v cargo > /dev/null; then
        echo "0"
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
    fi
    set +o errexit
    flatpak update < <(printf "n\n") \
        | grep -Ec '^[ ]+[0-9]+\.'
    set -o errexit
}

pending_updates_apt () {
    echo "0"
}

pending_updates_dnf () {
    dnf -q updateinfo list \
        | awk 'BEGIN {
                 count = 0
               };
               $0 ~ /\.fc[0-9][0-9]*\./ {
                 count += 1
               }
               END {
                 print NR-1
               };'
}

pending_updates_package_manager () {
    if command -v dnf > /dev/null; then
        pending_updates_dnf
    elif command -v apt > /dev/null; then
        pending_updates_apt
    else
        echo "N/A"
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
