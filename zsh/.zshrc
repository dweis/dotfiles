export ZSH=${HOME}/.oh-my-zsh

ZSH_THEME="norm"

#plugins=(git, vi-mode, aws)
plugins=(aws)

# User configuration

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
alias nix-search="nix-env -qaP '*' | grep"
alias nix-cleanup="sudo nix-collect-garbage -d && nix-store --optimize"

# Enable vim keybindings
bindkey -v
bindkey '^R' history-incremental-search-backward

#if [ -n "$DESKTOP_SESSION" ]; then
#  eval $(gnome-keyring-daemon --start)
#  export SSH_AUTH_SOCK
#fi
