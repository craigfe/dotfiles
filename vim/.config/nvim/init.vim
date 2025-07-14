set number
set relativenumber

call plug#begin()
Plug 'cocopon/iceberg.vim'
Plug 'kassio/neoterm'
Plug 'hashivim/vim-terraform'
Plug 'bakpakin/janet.vim'
Plug 'github/copilot.vim'
Plug 'tpope/vim-commentary'
call plug#end()

let mapleader = ","
nnoremap <Space> :

colorscheme iceberg

set termguicolors
set undofile  " Maintain a persistent undo log

function! JudgeAtCursor()
  " Save the current file before running `judge` on it
  update

  let filename = expand('%')
  let line = line('.')
  let col = col('.')
  botright new
  resize 10
  call termopen('judge ' . filename . ':' . line . ':' . col)
  startinsert

  " Keybinding for 'A' inside the terminal buffer
  execute 'tnoremap <buffer> A <C-\><C-n>:call JudgeAccept("' . filename . '", ' . line . ', ' . col . ')<CR>'

  " Keybinding to close the terminal buffer on <Esc>
  execute 'tnoremap <buffer> <Esc> <C-\><C-n>:bd!<CR>'
endfunction

function! JudgeAccept(filename, line, col)
  " Run the command in a terminal buffer and close it immediately after
  " Once the command is done, reload the file with 'edit!' to reflect the changes
  call jobstart('judge --accept ' . a:filename . ':' . a:line . ':' . a:col, {'on_exit': {-> execute('edit!')}})
  bd!
endfunction

autocmd FileType janet nnoremap <leader>j :call JudgeAtCursor()<CR>
autocmd FileType python nnoremap <leader>r :!./%<CR>

" Open a new tab at the current buffer's path
" Useful when you want to edit a file in the same directory as the current one
map <leader>te :tabedit <C-r>=escape(expand("%:p:h"), " ")<cr>/<Enter>
