set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

" Bundles
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-speeddating'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-characterize'
Plugin 'tpope/vim-unimpaired'
Plugin 'godlygeek/tabular'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/syntastic'
Plugin 'Rykka/colorv.vim'
Plugin 'vim-scripts/sudo.vim'
Plugin 'mhinz/vim-signify'
Plugin 'Valloric/YouCompleteMe'
Plugin 'guns/ultisnips'
Plugin 'tpope/vim-fireplace'
Plugin 'dahu/LearnVim'
Plugin 'vim-scripts/paredit.vim'
Plugin 'rking/ag.vim'
Plugin 'mileszs/ack.vim'
Plugin 'tpope/vim-sensible'
Plugin 'JuliaLang/julia-vim'
Plugin 'Shougo/vimproc'

" Filetypes
Plugin 'guns/vim-clojure-static'
Plugin 'gerw/vim-latex-suite'
Plugin 'xuhdev/vim-latex-live-preview'
Plugin 'tpope/vim-liquid'
Plugin 'tpope/vim-markdown'
Plugin 'mutewinter/nginx.vim'
Plugin 'leshill/vim-json'
Plugin 'vim-scripts/Vim-R-plugin'
Plugin 'dag/vim2hs'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'eagletmt/neco-ghc'

" Aesthetics
Plugin 'tomasr/molokai'
Plugin 'chriskempson/base16-vim'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'bling/vim-airline'

call vundle#end()

filetype plugin indent on
syntax on

"Set tab options to preferred 2 spaces
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

set copyindent
set showmode
set hidden
set wildmode=list:longest
set wildignore=*.swp,*.bak
set cursorline
set ttyfast
set laststatus=2
set nu
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
" Clear search highlighting with Escape
nnoremap <silent> <esc> :noh<return><esc>

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
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

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

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfunction

"Automatically tabulate when inserting the pipe symbol.
"Borrowed from Tim Pope's gist: https://gist.github.com/287147
inoremap <silent> <Bar>   <Bar><Esc>:call <SID>align()<CR>a

"Some great mappings from Vimcast #29: http://vimcasts.org/episodes/aligning-text-with-tabular-vim/
nnoremap <leader>a= :Tabularize /=<CR>
vnoremap <leader>a= :Tabularize /=<CR>
nnoremap <leader>a: :Tabularize /:\zs<CR>
vnoremap <leader>a: :Tabularize /:\zs<CR>

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

nnoremap <left>  :3wincmd <<cr>
nnoremap <right> :3wincmd ><cr>
nnoremap <up>    :3wincmd +<cr>
nnoremap <down>  :3wincmd -<cr>

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
autocmd FileType haskell nmap <F2> :GhcModType<CR>
autocmd FileType haskell nmap <silent> <F3> :GhcModTypeClear<CR>
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
autocmd FileType haskell setlocal iskeyword=a-z,A-Z,_,.,39
let g:ycm_semantic_triggers = {'haskell' : ['.']}

" Attempt to pull completion words from the languages' syntax file
let g:ycm_seed_identifiers_with_syntax = 1
