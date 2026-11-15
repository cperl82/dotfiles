#!/bin/bash

# Expected to be invoked by waybar with a reasonable `interval'

set -o pipefail
set -o nounset

pending_updates_apt () {
    if ! command -v apt > /dev/null; then
        return 1
    fi

    local sort=0
    local updates
    local awk
    read -d '' -r awk <<-'EOF'
	BEGIN { count = 0 };
	$1 ~ /\// { count += 1 };
	END { print count }
	EOF
    updates=$(apt list --upgradable 2>/dev/null | awk "${awk}")
    echo "${sort} ${updates}"
}

pending_updates_dnf () {
    if ! command -v dnf > /dev/null; then
        return 1
    fi

    local sort=0
    local updates
    local awk
    read -d '' -r awk <<-'EOF'
	BEGIN { count = 0 };
	$5 ~ /^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$/ { count += 1 }
	END { if (NR > 2) { print NR-1 } else { print 0 } };
	EOF
    updates=$(dnf -q updateinfo list | awk "${awk}")
    echo "${sort} ${updates}"
}

pending_updates_flatpak () {
    if ! command -v flatpak > /dev/null; then
        return 1
    fi

    set +o errexit
    local sort=1
    local updates
    updates=$(
        flatpak update <<< "n" \
            | grep -Ec '^[ ]+[0-9]+\.')
    echo "${sort} ${updates}"
    set -o errexit
}

pending_updates_rustup () {
    local rustup_init=""

    if ! command -v rustup > /dev/null; then
        return 1
    fi

    local sort=2
    local updates
    # We're trying to determine if updates listed for rustup itself
    # should be counted. If rustup is installed via the system package
    # manager then they shouldn't, else they should. We attempt to
    # determine if rustup is install by the system package manager by
    # seeing where `rustup-init' is installed. If it's in our home
    # directory, then it's not installed by the system package manager
    # and updates to rustup should count.
    rustup_init=$(which rustup-init)
    if [[ "${rustup_init}" =~ ^${HOME}/ ]]; then
        updates=$(rustup check | grep -c Update)
    else
        updates=$(rustup check | grep -v "^rustup " | grep -c Update)
    fi
    echo "${sort} ${updates}"
}

pending_updates_cargo () {
    if ! command -v cargo install-update > /dev/null; then
        return 1
    fi

    local sort=3
    local updates
    local awk
    read -d '' -r awk <<-'EOF'
	BEGIN { count = 0 };
	$2 ~ /^v/ && $3 ~ /^v/ && $4 ~ /^[Yy][Ee][Ss]$/ { count += 1 };
	END { print count };
	EOF
    # CR cperl: I don't think this covers git installed packages
    updates=$(cargo install-update --list | awk "${awk}")
    echo "${sort} ${updates}"
}

pending_updates_pip () {
    if ! command -v pip > /dev/null; then
        return 1
    fi

    local sort=3
    local updates
    updates=$(
        pip list --user --outdated \
            | grep -Ec '[0-9.]+$')
    echo "${sort} ${updates}"
}

pending_updates_pipx () {
    if ! command -v pipx > /dev/null; then
        return 1
    fi

    local sort=3
    local cooldown=7
    local updates
    local awk
    read -d '' -r awk <<-'EOF'
	BEGIN { count = 0 };
	$2 ~ /^[0-9.]+$/ && $3 ~ /^[0-9.]+$/ { count++ };
	END   { print count };
	EOF
    updates=$(
        pipx list --json                                    \
            | jq -r '.venvs | keys | @tsv'                  \
            | xargs -r -I{} \
                    pipx runpip {} list \
                    --outdated  \
                    --uploaded-prior-to "P${cooldown}D" \
            | awk "${awk}")
    echo "${sort} ${updates}"
}

pending_updates_uv () {
    if ! command -v uv > /dev/null; then
        return 1
    fi

    local sort=3
    local cooldown=7
    local updates
    local tool_updates
    local awk

    # This one is a little confusing, so leaving an explainer here.
    #
    # For each tool installed with uv we want to run a pip command in
    # that environment to list the outdated packages. Something like:
    #
    # pip list --oudated --uploaded-prior-to COOLDOWN ...
    #
    # The easiest way to do that is to use uv to run pip. But, we also
    # don't want a brand new pip, so we run it like:
    #
    # uvx ---exclude-newer COOLDOWN pip ...
    #
    # We could just use the system installed pip, but on at least one
    # of my systems that pip doesn't understand the
    # --uploaded-prior-to flag.
    #
    # We then look for output that has version numbers in the second
    # and third columns and then add it all up.
    read -d '' -r awk <<-'EOF'
	BEGIN { count = 0 };
	$2 ~ /^[0-9.]+$/ && $3 ~ /^[0-9.]+$/ { count++ };
	END   { print count };
	EOF
    updates=$(
        while read -r tool; do
            tool_updates=$(
                uvx --exclude-newer "${cooldown} days"                  \
                    pip                                                 \
                    --python "$(uv tool dir)/${tool}/bin/python"        \
                    list                                                \
                    --outdated                                          \
                    --uploaded-prior-to "P${cooldown}D"                 \
                    | awk "${awk}")
            updates=$((updates + tool_updates))
        done < <(uv tool list | grep -Eo '^[^-][^ ]+'))
    echo "${sort} ${updates}"
}

pending_updates_opam () {
    if ! command -v opam > /dev/null; then
        return 1
    fi

    local sort=3
    if opam upgrade --check >/dev/null 2>&1; then
        # CR-soon cperl: Meh, 1 isn't really true, it's just standing
        # in for "there are upgrades", maybe fix that.
        echo "${sort} 1"
    else
        echo "${sort} 0"
    fi
}

pending_updates_npm () {
    if ! command -v npm > /dev/null; then
        return 1
    fi

    local sort=3
    local updates
    updates=$(
        npm outdated -g --min-release-age=7 --depth=0 --json \
            | jq -r 'keys | length')
    echo "${sort} ${updates}"
}

run () {
    local fns
    local name
    local text
    local tooltip
    local sort
    local updates
    local pending_updates=0
    local tooltip_components=()

    mapfile -t fns < <(declare -F | grep -Eo "pending_updates_.*$")
    for fn in "${fns[@]}"; do
        name="${fn#pending_updates_}"
        sort=""
        updates=""
        if ! output=$(${fn}); then
            continue
        else
            if [[ "${output}" =~ ^([0-9]+)[[:space:]]([0-9]+)$ ]]; then
                sort="${BASH_REMATCH[1]}"
                updates="${BASH_REMATCH[2]}"
                pending_updates=$((pending_updates + updates))
            else
                sort="99"
                updates="!"
            fi
        fi
        tooltip_components+=(
            "$(printf "%d %-10s %3d" "${sort}" "${name^}:" "${updates}")")
    done
    text="${pending_updates}"
    mapfile -t tooltip_components < <(
        printf "%s\n" "${tooltip_components[@]}"        \
            | sort -k 1,1n -k 2,2                       \
            | cut -d ' ' -f 2-)
    printf -v tooltip "%s\\\\n" "${tooltip_components[@]}"
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
