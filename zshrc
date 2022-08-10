# -*- mode: shell-script; -*-

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

# Add python3 to path --- TODO only doing this for trezorctl setup, determine
# if I should do this long-term
export PATH=$PATH:$HOME/Library/Python/3.9/bin

# TODO per https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html
# these linese might need to be at the bottom of the file
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

# Shortcut for having a browsable history
alias hl='history | less'

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
