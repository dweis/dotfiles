set nocompatible

"let g:deoplete#enable_at_startup = 1

" Plugins
call plug#begin('~/.vim/plugged')
" Defaults
Plug 'tpope/vim-sensible'
" Deoplete
Plug 'roxma/vim-hug-neovim-rpc'
Plug 'Shougo/deoplete.nvim'
Plug 'roxma/nvim-yarp'
" General
Plug 'ctrlpvim/ctrlp.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'ervandew/supertab'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'scrooloose/syntastic'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'vim-airline/vim-airline'
" Colorschemes
Plug 'altercation/vim-colors-solarized'
Plug 'dracula/vim'
" Languages
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
Plug 'digitaltoad/vim-pug', { 'for': 'pug' }
Plug 'purescript-contrib/purescript-vim', { 'for': 'purescript' }
Plug 'FrigoEU/psc-ide-vim', { 'for': 'purescript' }
Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
Plug 'godlygeek/tabular', { 'for': 'haskell' }
call plug#end()

set number
set nowrap
set showmode
set smartcase
set smartindent
set softtabstop=2
set shiftwidth=2
set expandtab
set history=1000
set colorcolumn=80,100
set list listchars=trail:.

set completeopt=menuone,menu,longest

set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox,output,node_modules,bower_components
set wildmode=longest,list,full

set t_Co=256

set cmdheight=1

colorscheme dracula
set background=dark

" -- Supertab
let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

if has("gui_running")
  imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
  if has("unix")
    inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
  endif
endif

" -- NERDtree
map <Leader>n :NERDTreeToggle<CR>

" -- Ctrl-P
map <silent> <Leader>t :CtrlP()<CR>
noremap <leader>b<space> :CtrlPBuffer<cr>
let g:ctrlp_custom_ignore = '\v[\/]dist$'

" -- Airline
let g:airline_powerline_fonts = 1
