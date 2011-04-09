set nocompatible
set nobk
set encoding=utf-8
set fileencodings=utf-8,chinese,latin-1
language messages zh_CN.UTF-8
"tab setting
set tabstop=4
set expandtab
set shiftwidth=4
set softtabstop=4
"buffer access
set wildchar=<Tab> wildmenu wildmode=full
set wildcharm=<C-Z>
nnoremap <F10> :b <C-Z>
"no bell
set vb
" Buffers - explore/next/previous: Alt-F12, F12, Shift-F12.
nnoremap <silent> <M-F12> :BufExplorer<CR>
nnoremap <silent> <F12> :bn<CR>
nnoremap <silent> <S-F12> :bp<CR>


map <F2> :NERDTreeToggle<CR>
map <A-,> <S-CR>
map <A-.> <C-CR>
map <A-m> <M-CR>
map <A-M> <C-S-CR>
nnoremap <esc> :noh<return><esc>

source $VIMRUNTIME/vimrc_example.vim
" source $VIMRUNTIME/mswin.vim
" behave mswin
