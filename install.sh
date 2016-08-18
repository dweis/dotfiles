#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ln -sf ${DIR}/vim/vimrc ~/.vimrc
ln -sf ${DIR}/vim/vim ~/.vim
ln -sf ${DIR}/zsh/zshrc ~/.zshrc
ln -sf ${DIR}/git/gitconfig ~/.gitconfig
ln -sf ${DIR}/tmux/tmux.conf ~/.tmux.conf
