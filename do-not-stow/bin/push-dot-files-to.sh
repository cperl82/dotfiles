#!/bin/bash

# This script expects to take a variable number of anonymous arguments
# that are rsync destinations.  It will then sync this dotfiles repo
# to that target and attempt to replicate the stow links that point
# into this repo.

main ()
{
    # dotfiles repo (stow directory) is $(dirname ${0})/../..
    # target directory is ${stow_directory}/..

    # steps are:
    # 1. sync the repo to the destination
    # 2. identify links
    # 3. recreate links in target directory at remote
    #
    # I think you'll want to use the --files-from flag.
    :
}

main "${@}"
