#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
mkdir -p ~/.config/Code
mkdir -p ~/.stack

ln -s ${DIR}/dircolors ~/.dircolors
ln -s ${DIR}/git/gitconfig ~/.gitconfig
ln -s ${DIR}/tmux/tmux.conf ~/.tmux.conf
ln -s ${DIR}/vim/vim ~/.vim
ln -s ${DIR}/vim/vim/init.vim ~/.vimrc
ln -s ${DIR}/zsh/zshrc ~/.zshrc
ln -s ${DIR}/spacemacs/spacemacs ~/.spacemacs
ln -s ${DIR}/xmonad ~/.xmonad
ln -s ${DIR}/my-taffybar ~/.config/taffybar
ln -s ${DIR}/termite ~/.config/termite
ln -s ${DIR}/xscreensaver/xscreensaver ~/.xscreensaver
ln -s ${DIR}/rofi ~/.config/rofi
ln -s ${DIR}/compton/compton.conf ~/.config/compton.conf
ln -s ${DIR}/hyperjs/hyper.js ~/.hyper.js
ln -s ${DIR}/Code/User ~/.config/Code/User
ln -s ${DIR}/bash/profile ~/.profile
ln -s ${DIR}/stack/config.yaml ~/.stack/config.yaml
