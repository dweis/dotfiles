#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ln -sf ${DIR}/dircolors ~/.dircolors
#ln -sf ${DIR}/git/gitconfig ~/.gitconfig
ln -sf ${DIR}/tmux/tmux.conf ~/.tmux.conf
ln -sf ${DIR}/vim/vim ~/.vim
ln -sf ${DIR}/vim/vim/init.vim ~/.vimrc
ln -sf ${DIR}/zsh/zshrc ~/.zshrc
