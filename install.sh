#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
mkdir -p ~/.config

ln -s ${DIR}/dircolors ~/.dircolors
ln -s ${DIR}/git/gitconfig ~/.gitconfig
ln -s ${DIR}/tmux/tmux.conf ~/.tmux.conf
ln -s ${DIR}/vim/vim ~/.vim
ln -s ${DIR}/vim/vim/init.vim ~/.vimrc
ln -s ${DIR}/zsh/zshrc ~/.zshrc
ln -s ${DIR}/spacemacs/spacemacs ~/.spacemacs
ln -s ${DIR}/xmonad ~/.xmonad
ln -s ${DIR}/taffybar ~/.config/taffybar
ln -s ${DIR}/termite ~/.config/termite
ln -s ${DIR}/stalonetray/stalonetrayrc ~/.stalonetrayrc
ln -s ${DIR}/xprofile/xprofile ~/.xprofile
ln -s ${DIR}/xscreensaver/xscreensaver ~/.xscreensaver
ln -s ${DIR}/rofi ~/.config/rofi
ln -s ${DIR}/compton/compton.conf ~/.config/compton.conf
