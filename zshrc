#!/bin/zsh

################################################################################
# Oh My Zsh configuration.

# I deleted most of the commented out template lines that were not being used;
# see file in omz repo here:
# https://github.com/ohmyzsh/ohmyzsh/blob/master/templates/zshrc.zsh-template
################################################################################

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

################################################################################
# All-machines User Configuration
# 
# These configuration settings should apply to all synchronized machines that
# make use of https://github.com/izzygomez/dotfiles &
# https://github.com/izzygomez/new-laptop-setup
################################################################################

# Syntax highlighting in `less`
# https://ole.michelsen.dk/blog/syntax-highlight-files-macos-terminal-less/
LESSPIPE=`which src-hilite-lesspipe.sh`
export LESSOPEN="| ${LESSPIPE} %s"
export LESS=' -R '

# Added these after running `brew install zsh-syntax-highlighting`
# https://github.com/zsh-users/zsh-syntax-highlighting
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=/opt/homebrew/share/zsh-syntax-highlighting/highlighters

# set emacs as default editor
export EDITOR=emacs
# set prompt to have laptop emoji prefix to differentiate terminal GUI on this
# machine from any one that I ssh/mosh into.
PROMPT='💻 '$PROMPT

# Shortcuts to start/kill emacs as daemon and use emacsclient, or just use emacs
alias e="emacs"
alias emacsd="emacs --daemon"
alias ec="emacsclient"
alias ek="emacsclient -e '(kill-emacs)'"

# Added per install commands from https://github.com/nvbn/thefuck
eval "$(thefuck --alias)"

# makes ^R backward search support patterns (i.e. '*' characters)
bindkey '^R' history-incremental-pattern-search-backward

# Git commit graph alias
# https://tech.serhatteker.com/post/2021-02/git-log-tree/
alias gitgraph='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --all'
alias ggr='gitgraph'

# Shortcut for having a browsable history, with date & time stamps
alias hl='history -f | less'

# Added per instruction after running `brew install chruby ruby-install`, which
# was run per instructions in https://jekyllrb.com/docs/installation/macos/
source /opt/homebrew/opt/chruby/share/chruby/chruby.sh
source /opt/homebrew/opt/chruby/share/chruby/auto.sh
chruby ruby-3.1.2
# add brew-installed ruby to path
export PATH="/usr/local/opt/ruby/bin:$PATH"
# add gem (ruby) executable to path, per
# https://jekyllrb.com/docs/installation/macos/
export PATH="$PATH:$HOME/.gem/ruby/2.6.0/bin"

# Added after `brew install grep`
PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"

# Remove hints about Homebrew behaviour with environment variables (this is
# annoying to have around in `brew` command outputs). See: https://docs.brew.sh/Manpage
export HOMEBREW_NO_ENV_HINTS=1

# Setting env variable so that local formulae/cask changes (via `brew edit ...`)
# are not ignored. See: https://docs.brew.sh/FAQ#can-i-edit-formulae-myself
#
# Context here is that as of writing this, I've run `brew edit expressvpn` &
# added a `auto_updates true` line so that brew doesn't upgrade this cask —
# prefer not having this behavior for this particular cask because upgrade
# process is cumbersome & app seems to upgrade on its own.
export HOMEBREW_NO_INSTALL_FROM_API=1

# Per `brew install zsh-autosuggestions` instructions
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

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

# Added as part of pyenv install instructions
# https://github.com/pyenv/pyenv#set-up-your-shell-environment-for-pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

## Following lines added by gcloud install script: https://cloud.google.com/sdk/docs/install#mac
# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/izzyg/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/izzyg/google-cloud-sdk/path.zsh.inc'; fi
# The next line enables shell command completion for gcloud.
if [ -f '/Users/izzyg/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/izzyg/google-cloud-sdk/completion.zsh.inc'; fi

# Added per instructions (7) while setting up GPG keys for git commits
# https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key#telling-git-about-your-gpg-key
export GPG_TTY=$(tty)

################################################################################
# Izzy's Personal MBA User Configuration
################################################################################

################################################################################
# Izzy's Yuzu MBP User Configuration
################################################################################

# Added automatically from install script:
# https://github.com/nvm-sh/nvm#installing-and-updating
export NVM_DIR="$HOME/.nvm"
# This loads nvm
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
# This loads nvm bash_completion
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
