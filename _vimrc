set nocompatible              " Don't be compatible with vi
" ==========================================================
" Vundle
" ==========================================================
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
Plugin 'fatih/vim-go'
"Plugin 'altercation/vim-colors-solarized'
Plugin 'jwhitley/vim-colors-solarized'
Plugin 'sjl/gundo.vim'
Plugin 'SirVer/ultisnips'
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<leader><tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

Plugin 'kien/ctrlp.vim'
let g:ctrlp_map = '<D-O>'
let g:ctrlp_max_height = 30
let g:ctrlp_match_window_bottom=1
let g:ctrlp_max_height = 20
let g:ctrlp_match_window_reversed = 1

Plugin 'scrooloose/syntastic'
"let g:syntastic_aggregate_errors = 1
"let g:syntastic_go_checkers = ['go', 'govet']
"highlight SyntasticErrorLine guibg=#2f0000
"highlight SyntasticWarningLine guibg=#2f0000
let g:syntastic_error_symbol = '✗'
let g:syntastic_warning_symbol = '⚠'
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_enable_highlighting = 0 "Why doesn't highlighting work?!?!
"let g:syntastic_enable_signs = 0
let g:syntastic_auto_jump = 2

Plugin 'scrooloose/nerdtree'

Plugin 'jiangmiao/auto-pairs'
Plugin 'Valloric/YouCompleteMe'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


" ==========================================================
" Shortcuts
" ==========================================================
let mapleader="\\"

" Seriously, guys. It's not like :W is bound to anything anyway.
command! W :w

" sudo write this
cmap W! w !sudo tee % >/dev/null

" open/close the quickfix window
nmap <leader>c :copen<CR>
nmap <leader>cc :cclose<CR>

" ctrl-jklm  changes to that split
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

" and lets make these all work in insert mode too ( <C-O> makes next cmd
"  happen as if in command mode )
imap <C-W> <C-O><C-W>

" Load the Gundo window
map <leader>u :GundoToggle<CR>

" Autocomplete
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-Space>
" close preview window automatically when we move around
"autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
"autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" Source vimrc
map <silent> <leader>V :source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

" ==========================================================
" Basic Settings
" ==========================================================
syntax on                     " syntax highlighing
filetype on                   " try to detect filetypes
filetype plugin indent on     " enable loading indent file for filetype
set number                    " Display line numbers
set numberwidth=1             " using only 1 column (and 1 space) while possible
set title                     " show title in console title bar
set wildmenu                  " Menu completion in command mode on <Tab>
set wildmode=full             " <Tab> cycles between all matching choices.
set guifont=Menlo\ Regular:h14

" http://stackoverflow.com/questions/2414626/vim-unsaved-buffer-warning
set hidden

" don't bell or blink
set noerrorbells
set vb t_vb=

" Ignore these files when completing
set wildignore+=*.o,*.obj,.git,*.pyc,*/ios/*,*/ruby/*,*/chef/cookbooks/*
set grepprg=ack-grep          " replace the default grep program with ack

" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

""" Insert completion
" don't select first item, follow typing in autocomplete
"set completeopt=menuone,longest,preview
set completeopt=menuone,longest
"set pumheight=6             " Keep a small completion window

"highlight past column 80
"http://stackoverflow.com/questions/2447109/showing-a-different-background-colour-in-vim-past-80-characters
set textwidth=80
"let &colorcolumn=join(range(81,199),",")
set colorcolumn=+1

""" Moving Around/Editing
set ruler                   " show the cursor position all the time
set nostartofline           " Avoid moving cursor to BOL when jumping around
set virtualedit=block       " Let cursor move past the last char in <C-v> mode
set scrolloff=3             " Keep 3 context lines above and below the cursor
set backspace=2             " Allow backspacing over autoindent, EOL, and BOL
set showmatch               " Briefly jump to a paren once it's balanced
set nowrap                  " don't wrap text
set linebreak               " don't wrap textin the middle of a word
set autoindent              " always set autoindenting on
set tabstop=2               " <tab> inserts 4 spaces
set shiftwidth=2            " but an indent level is 2 spaces wide.
set softtabstop=2           " <BS> over an autoindent deletes both spaces.
"set expandtab              " Use spaces, not tabs, for autoindent/tab key.
set shiftround              " rounds indent to a multiple of shiftwidth
set matchpairs+=<:>         " show matching <> (html mainly) as well
set foldmethod=indent       " allow us to fold on indents
set foldlevel=99            " don't fold by default

" don't outdent hashes
"inoremap # #

"""" Reading/Writing
set noautowrite             " Never write a file unless I request it.
set noautowriteall          " NEVER.
set noautoread              " Don't automatically re-read changed files.
set modeline                " Allow vim options to be embedded in files;
set modelines=5             " they must be within the first or last 5 lines.
set ffs=unix,dos,mac        " Try recognizing dos, unix, and mac line endings.

"""" Messages, Info, Status
set ls=2                    " allways show status line
set vb t_vb=                " Disable all bells.  I hate ringing/flashing.
set confirm                 " Y-N-C prompt if closing with unsaved changes.
set showcmd                 " Show incomplete normal mode commands as I type.
set report=0                " : commands always print changed line count.
set shortmess+=a            " Use [+]/[RO]/[w] for modified/readonly/written.
set laststatus=2            " Always show statusline, even if only 1 window.
set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ %{fugitive#statusline()}

" displays tabs with :set list & displays when a line runs off-screen
"set listchars=tab:>-,eol:$,trail:-,precedes:<,extends:>
"set list

""" Searching and Patterns
set ignorecase              " Default to using case insensitive searches,
set smartcase               " unless uppercase letters are used in the regex.
set smarttab                " Handle tabs more intelligently
set hlsearch                " Highlight searches by default.
set incsearch               " Incrementally search while typing a /regex

" Paste from clipboard
"map <leader>p "+gP

" Quit window on <leader>q
nnoremap <leader>q :q<CR>
"
" hide matches on <leader>space
nnoremap <leader><space> :nohlsearch<cr>

" ===========================================================
" FileType specific changes
" ============================================================
" Mako/HTML
autocmd BufNewFile,BufRead *.mako,*.mak setlocal ft=html
autocmd FileType html,xhtml,xml,css setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" Python
au FileType python setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4

"fixes the bug in Insert-Visual/Select mode
nmap <silent><C-A>      :cal SmartHome("n")<CR>
nmap <silent><C-E>       :cal SmartEnd("n")<CR>
imap <silent><C-A>     <C-r>=SmartHome("i")<CR>
imap <silent><C-E>      <C-r>=SmartEnd("i")<CR>
vmap <silent><C-A> <Esc>:cal SmartHome("v")<CR>
vmap <silent><C-E>  <Esc>:cal SmartEnd("v")<CR>

nmap <silent><Home>      :cal SmartHome("n")<CR>
nmap <silent><End>       :cal SmartEnd("n")<CR>
imap <silent><Home>     <C-r>=SmartHome("i")<CR>
imap <silent><End>      <C-r>=SmartEnd("i")<CR>
vmap <silent><Home> <Esc>:cal SmartHome("v")<CR>
vmap <silent><End>  <Esc>:cal SmartEnd("v")<CR>

""""""""""""""""""""
"smart home function
function! SmartHome(mode)
  let curcol = col(".")

  "gravitate towards beginning for wrapped lines
  if curcol > indent(".") + 2
    call cursor(0, curcol - 1)
  endif

  if curcol == 1 || curcol > indent(".") + 1
    if &wrap
      normal g^
    else
      normal ^
    endif
  else
    if &wrap
      normal g0
    else
      normal 0
    endif
  endif

  if a:mode == "v"
    normal msgv`s
  endif

  return ""
endfunction

"""""""""""""""""""
"smart end function
function! SmartEnd(mode)
  let curcol = col(".")
  let lastcol = a:mode == "i" ? col("$") : col("$") - 1

  "gravitate towards ending for wrapped lines
  if curcol < lastcol - 1
    call cursor(0, curcol + 1)
  endif

  if curcol < lastcol
    if &wrap
      normal g$
    else
      normal $
    endif
  else
    normal g_
  endif

  "correct edit mode cursor position, put after current character
  if a:mode == "i"
    call cursor(0, col(".") + 1)
  endif

  if a:mode == "v"
    normal msgv`s
  endif

  return ""
endfunction

"Run pyflakes
map <leader>pf :!pyflakes %<CR>

"Get rid of annoying mac title bar thingy
set guioptions=egmrLt

" This beauty remembers where you were the last time you edited the file, and returns to the same position.
au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif

" ------------------------------------------------------------------
" Solarized Colorscheme Config
" ------------------------------------------------------------------
syntax enable
if has("gui_running")
	colorscheme solarized
	set background=light
endif
" ------------------------------------------------------------------

au BufRead,BufNewFile {*.go}	setl ft=go tabstop=2 softtabstop=2 noexpandtab smarttab

"vim-go stuff
"Import the package under your cursor with <leader>i (useful if you have 
"disabled auto import via GoDisableGoimport)
au FileType go nmap <Leader>i <Plug>(go-import)

"Open the relevant Godoc for the word under the cursor with <leader>gd or open
"it vertically with <leader>gv
au FileType go nmap <Leader>gd <Plug>(go-doc)
au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)

"Run commands, such as go run with <leader>r for the current file or go build
"and go test for the current package with <leader>b and <leader>t.
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>v <Plug>(go-vet)

"Replace gd (Goto Declaration) for the word under your cursor (replaces current buffer):
au FileType go nmap gd <Plug>(go-def)

"Or open the defitinion/declaration in a new vertical, horizontal or tab for the word under your cursor:
au FileType go nmap <Leader>ds <Plug>(go-def-split)
au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
au FileType go nmap <Leader>dt <Plug>(go-def-tab)
