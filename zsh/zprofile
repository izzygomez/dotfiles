#!/bin/zsh

# Source checkers.sh
dotfiles_dir=$(dirname $(realpath ~/.zprofile))
source $dotfiles_dir/checkers.sh

if isMacOS; then
    # TODO: should add check here to make sure `brew` is installed, & if it
    # isn't print an error message (possibly also set flag to not go about
    # setting other configs?)
    # Added as part of Homebrew installation
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi
