set number
set relativenumber

call plug#begin()
Plug 'cocopon/iceberg.vim'
Plug 'kassio/neoterm'
Plug 'hashivim/vim-terraform'
Plug 'bakpakin/janet.vim'
call plug#end()

let mapleader = ","

colorscheme iceberg

set termguicolors
set undofile  " Maintain a persistent undo log

" Open a new tab at the current buffer's path
" Useful when you want to edit a file in the same directory as the current one
map <leader>te :tabedit <C-r>=escape(expand("%:p:h"), " ")<cr>/<Enter>
