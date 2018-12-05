#!/bin/sh
stow -t $HOME alacritty
stow -t $HOME bash
stow -t $HOME compton
stow -t $HOME git
stow -t $HOME i3
stow -t $HOME taffybar
stow -t $HOME spacemacs
stow -t $HOME termite
stow -t $HOME vim
stow -t $HOME xscreensaver
stow -t $HOME Code
stow -t $HOME dircolors
stow -t $HOME hyperjs
stow -t $HOME rofi
stow -t $HOME stack
stow -t $HOME tmux
stow -t $HOME xmonad
stow -t $HOME zsh
stow -t $HOME nixpkgs
stow -t $HOME face

./scripts/vscode-install-extensions.sh

#dconf write /org/mate/desktop/session/required-components/windowmanager "'i3'"
dconf write /org/mate/desktop/background/show-desktop-icons "false"
