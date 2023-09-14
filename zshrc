#!/bin/zsh

################################################################################
# Source checkers.sh
################################################################################
dotfiles_dir=$(dirname $(realpath ~/.zshrc))
source $dotfiles_dir/checkers.sh

################################################################################
# Oh My Zsh configuration.
#
# I deleted most of the commented out template lines that were not being used;
# see file in omz repo here:
# https://github.com/ohmyzsh/ohmyzsh/blob/master/templates/zshrc.zsh-template
################################################################################

export ZSH="$HOME/.oh-my-zsh"
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"
plugins=(git)

source $ZSH/oh-my-zsh.sh

################################################################################
# All-machines User Configuration
# 
# These configuration settings should apply to all synchronized machines that
# make use of https://github.com/izzygomez/dotfiles &
# https://github.com/izzygomez/new-laptop-setup
################################################################################

# Mac OS specific configuration
if isMacOS; then
    source $dotfiles_dir/zshrc_macos
fi

# TODO create different prompts for different machines
# set prompt to have laptop emoji prefix to differentiate terminal GUI on this
# machine from any one that I ssh/mosh into.
PROMPT='💻 '$PROMPT

# TODO add check to see if emacs is available & possibly move all emacs config
# into separate file.
# set emacs as default editor
export EDITOR=emacs

# Shortcuts to start/kill emacs as daemon and use emacsclient, or just use emacs
alias e="emacs"
alias emacsd="emacs --daemon"
alias ec="emacsclient"
alias ek="emacsclient -e '(kill-emacs)'"

# makes ^R backward search support patterns (i.e. '*' characters)
bindkey '^R' history-incremental-pattern-search-backward

# TODO add check to see if git is available.
# Git commit graph alias
# https://tech.serhatteker.com/post/2021-02/git-log-tree/
alias gitgraph='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --all'
alias ggr='gitgraph'

# Shortcut for having a browsable history, with date & time stamps
alias hl='history -f | less'

# Command line weather widget
# Usage:
# `weather`
# or
# `weather {location}`
weather() {
    if [ -n "$1" ]
    then
	eval "curl -s wttr.in/$1"
    else
	curl -s wttr.in
    fi
}
alias wtr=weather

# TODO add check to see pyenv is available.
# Added as part of pyenv install instructions
# https://github.com/pyenv/pyenv#set-up-your-shell-environment-for-pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# Added per instructions (7) while setting up GPG keys for git commits
# https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key#telling-git-about-your-gpg-key
export GPG_TTY=$(tty)

# Added automatically from install script:
# https://github.com/nvm-sh/nvm#installing-and-updating
export NVM_DIR="$HOME/.nvm"
# This loads nvm
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
# This loads nvm bash_completion
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
