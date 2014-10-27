if empty(glob('~/.vim/autoload/plug.vim'))
  silent !mkdir -p ~/.vim/autoload
  silent !curl -fLo ~/.vim/autoload/plug.vim
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
call plug#begin('~/.vim/bundle')

" Bundles
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-unimpaired'
Plug 'tommcdo/vim-lion'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/syntastic'
Plug 'Rykka/colorv.vim'
Plug 'airblade/vim-gitgutter'
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }
Plug 'tpope/vim-sensible'
Plug 'Shougo/vimproc'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-projectionist'

"Clojure
Plug 'tpope/vim-fireplace', {'for': 'clojure' }
Plug 'tpope/vim-leiningen', {'for': 'clojure' }
Plug 'kovisoft/paredit', {'for': 'clojure' }
Plug 'guns/vim-clojure-static', {'for': 'clojure' }
Plug 'guns/vim-slamhound', {'for': 'clojure' }

" Filetypes
Plug 'tpope/vim-markdown'

" Aesthetics
Plug 'chriskempson/base16-vim'
Plug 'luochen1990/rainbow'
Plug 'bling/vim-airline'

call plug#end()

filetype plugin indent on
syntax on

scriptencoding utf-8
set encoding=utf-8

"Set tab options to preferred 2 spaces
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

set history=250        " Save last 250 commands
if $TMUX == ''
  set clipboard=unnamed " Yanks go on the clipboard
endif
set pastetoggle=<F10>  " Safer pastes
set autoread           " Update buffer when file changes
set splitbelow         " Split windows at bottom
set splitright         " Split windows to the right
set copyindent
set showmode
set hidden
set wildmode=list:longest
set wildignore=*.swp,*.bak
set wildmenu
set cursorline
set ttyfast
set laststatus=2
set title

" Visual block mode allows bounds outside the text (freeform)
set virtualedit=block

" Electric return for paredit mode
" Example: http://img8.imageshack.us/img8/9479/openparen.gif
let g:paredit_electric_returns = 1
" '(' and ')' will also jump to square brackets and curly braces
let g:paredit_smartjump = 1

"Search options
nnoremap / /\v
vnoremap / /\v
set ignorecase
set smartcase
set gdefault
set showmatch
set hlsearch

"Set up column at column width to stick with sane column width while coding
set nowrap
set formatoptions=qrn1
set colorcolumn=80

if has("autocmd")
  autocmd FileType tex set fo+=t tw=80
  autocmd FileType latex set fo+=t tw=80
  autocmd FileType txt set fo+=t tw=80
  autocmd FileType markdown set fo+=t tw=80
  autocmd FileType liquid set fo+=t tw=80
endif

"Save all files anytime focus is lost
if (expand("%") != "")
  au FocusLost * :wa
endif

"Allow for quick and easy editing and sourcing of vimrc
nnoremap <silent> <leader>ev :e $MYVIMRC<CR>
nnoremap <silent> <leader>sv :so $MYVIMRC<CR>

"" Aesthetics
""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Color scheme!
set background=dark
let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-eighties

"Rainbow parens on all the time
let g:rainbow_active = 1

" Set the font
if has("gui_running")
  if has("gui_macvim")
    set guifont=Sauce\ Code\ Powerline:h12
  else
    set guifont=Liberation\ Mono\ for\ Powerline\ 10
  endif
endif

" Attempt to use fancy symbols in the statusline
let g:airline_powerline_fonts=1
"If there's only a single tab, show buffers instead
let g:airline#extensions#tabline#enabled = 1

"Remove the menubar
set guioptions-=T

"Move by screen lines instead of file lines, in case of screen wrap
nnoremap j gj
nnoremap k gk

"Remap jj to esc, for quicker exiting to normal mode
inoremap jj <ESC>

"Show invisibles - tab and EOL, in the style of Textmate
nnoremap <leader>v :set list!<CR>

"Attempt to prevent CtrlP from sometimes opening a new split, sometimes not
let g:ctrlp_jump_to_buffer = 0

autocmd FileType text set wrap

"Insert a quick carriage return, instead of entering and exiting insert mode
nnoremap <silent> <leader><CR> i<CR><ESC>

"Easily navigate between open buffers
nnoremap <leader>z :bp<CR>
nnoremap <leader>x :bn<CR>

" Window navigation
" from http://www.agillo.net/simple-vim-window-management/
function! WinMove(key)
  let t:curwin = winnr()
  exec "wincmd ".a:key
  if (t:curwin == winnr()) "we havent moved
    if (match(a:key,'[jk]')) "were we going up/down
      wincmd v
    else
      wincmd s
    endif
    exec "wincmd ".a:key
  endif
endfunction

nnoremap <leader>h  :call WinMove('h')<cr>
nnoremap <leader>k  :call WinMove('k')<cr>
nnoremap <leader>l  :call WinMove('l')<cr>
nnoremap <leader>j  :call WinMove('j')<cr>
" Close window
nnoremap <leader>wc :wincmd q<cr>
"Rotate windows
nnoremap <leader>wr <C-W>r
nnoremap <leader>H  :wincmd H<cr>
nnoremap <leader>K  :wincmd K<cr>
nnoremap <leader>L  :wincmd L<cr>
nnoremap <leader>J  :wincmd J<cr>

if $TMUX == ''
  nnoremap <left>  :3wincmd <<cr>
  nnoremap <right> :3wincmd ><cr>
  nnoremap <up>    :3wincmd +<cr>
  nnoremap <down>  :3wincmd -<cr>
else
  nnoremap OD :3wincmd <<cr>
  nnoremap OC :3wincmd ><cr>
  nnoremap OA :3wincmd +<cr>
  nnoremap OB :3wincmd -<cr>
endif


" Taken from
" http://stackoverflow.com/questions/4331776/change-vim-swap-backup-undo-file-name
" Sets up undo, backup, and swap directories more like they should be

" Backups, undos, and swap files
"-----------------------------------------------------------------------------
" Save your backups to a less annoying place than the current directory.
" If you have .vim-backup in the current directory, it'll use that.
" Otherwise it saves it to ~/.vim/backup or . if all else fails.
if isdirectory($HOME . '/.vim-backup') == 0
  :silent !mkdir -p ~/.vim-backup >/dev/null 2>&1
endif
set backupdir^=~/.vim-backup/
set backup
" Prevent backups from overwriting each other. The naming is weird,
" since I'm using the 'backupext' variable to append the path.
" So the file '/home/docwhat/.vimrc' becomes '.vimrc%home%docwhat~'
if has("autocmd")
  autocmd BufWritePre * let &backupext = substitute(expand('%:p:h'), '/', '%', 'g') . '~'
endif

" Save your swp files to a less annoying place than the current directory.
" If you have .vim-swap in the current directory, it'll use that.
" Otherwise it saves it to ~/.vim/swap, ~/tmp or .
if isdirectory($HOME . '/.vim-swap') == 0
  :silent !mkdir -p ~/.vim-swap >/dev/null 2>&1
endif
set directory^=~/.vim-swap/
set directory+=~/tmp//
set directory+=.

" viminfo stores the the state of your previous editing session
set viminfo+=n~/.viminfo

if exists("+undofile")
  " undofile - This allows you to use undos after exiting and restarting
  " This, like swap and backups, uses .vim-undo first, then ~/.vim/undo
  " :help undo-persistence
  " This is only present in 7.3+
  if isdirectory($HOME . '/.vim-undo') == 0
    :silent !mkdir -p ~/.vim-undo > /dev/null 2>&1
  endif
  set undodir^=~/.vim-undo/
  set undofile
  set undolevels=1000
  set undoreload=10000
endif

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
" Also don't do it when the mark is in the first line, that is the default
" position when opening a file.
if has("autocmd")
  autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \   exe "normal! g`\"" |
        \ endif
endif

" Set syntastic to show error lines
let g:syntastic_enable_signs=1

"Prevent highlight being lost on indent change
vnoremap < <gv
vnoremap > >gv

"Keybinding to pretty-print the current day
nnoremap <leader>d "=strftime("%a %b %d %G")<CR>p

" Enable omni completion. Not required if they are already set elsewhere in .vimrc
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete

" Press the space bar to send lines (in Normal mode) and selections to R:
autocmd FileType r vmap <Space> <Plug>RDSendSelection
autocmd FileType r nmap <Space> <Plug>RDSendLine

" Haskell goodness
autocmd FileType haskell nmap <leader>ht :GhcModType<CR>
autocmd FileType haskell nmap <silent> <leader>hc :GhcModTypeClear<CR>
autocmd FileType haskell nmap <leader>hl :GhcModLint<CR>
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
autocmd FileType haskell setlocal iskeyword=a-z,A-Z,_,.,39
let g:ycm_semantic_triggers = {'haskell' : ['.']}
" Check and lint on buffer save
autocmd BufWritePost *.hs GhcModCheckAndLintAsync

" Attempt to pull completion words from the languages' syntax file
let g:ycm_seed_identifiers_with_syntax = 1

" View tags
nmap <leader>t :TagbarToggle<CR>

" If a directory is specified, explore the directory on startup
autocmd VimEnter * if isdirectory(expand('<afile>')) | Explore | endif

" Tags support for Hasktags, according to the tagbar wiki
let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

" Tags support for R, according to tagbar wiki
let g:tagbar_type_r = {
    \ 'ctagstype' : 'r',
    \ 'kinds'     : [
        \ 'f:Functions',
        \ 'g:GlobalVariables',
        \ 'v:FunctionVariables',
    \ ]
\ }
