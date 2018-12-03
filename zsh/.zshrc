export ZSH=${HOME}/.oh-my-zsh

ZSH_THEME="norm"

plugins=(git, vi-mode, aws)

# User configuration

# Enable vim keybindings
bindkey -v

export PATH="/home/derrick/bin:/home/derrick/.local/bin:$PATH"

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

export EDITOR=vim

export ERL_AFLAGS="-kernel shell_history enabled"

if whence dircolors >/dev/null; then
  eval `dircolors ~/.dircolors`
else
  export CLICOLORS=1
  export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
fi

alias fonts='fc-list | cut -f2 -d: | sort -u'
