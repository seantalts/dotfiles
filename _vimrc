set nocompatible              " Don't be compatible with vi
" ==========================================================
" https://github.com/junegunn/vim-plug 
"
" ==========================================================
filetype off                  " required

call plug#begin('~/.vim/plugged')

" let Vundle manage Vundle, required
Plug 'gmarik/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plug commands between vundle#begin/end.
" plugin on GitHub repo
Plug 'tpope/vim-fugitive'
Plug 'fatih/vim-go'

Plug 'jwhitley/vim-colors-solarized'
Plug 'simnalamburt/vim-mundo'

Plug 'kien/ctrlp.vim'
let g:ctrlp_map = '<C-P>'
"let g:ctrlp_cmd = 'CtrlPMRUFiles'
let g:ctrlp_max_height = 30
let g:ctrlp_match_window_bottom=1
let g:ctrlp_max_height = 20
let g:ctrlp_match_window_reversed = 1
if executable('rg')
  set grepprg=rg\ --color=never
  autocmd QuickFixCmdPost *grep* cwindow "should open quickfix with any grep command results
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
endif

Plug 'scrooloose/syntastic'
"let g:syntastic_aggregate_errors = 1
"let g:syntastic_go_checkers = ['go', 'govet']
"let g:syntastic_html_checkers = []
"let g:syntastic_use_quickfix_lists = 1
"highlight SyntasticErrorLine guibg=#2f0000
"highlight SyntasticWarningLine guibg=#2f0000
"let g:syntastic_error_symbol = '✗'
"let g:syntastic_warning_symbol = '⚠'
"let g:syntastic_always_populate_loc_list=1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_enable_highlighting = 0 "Why doesn't highlighting work?!?!
"let g:syntastic_enable_signs = 0
"let g:syntastic_auto_jump = 2

map <Leader>s :SyntasticToggleMode<CR>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

Plug 'scrooloose/nerdtree'

"Plug 'jiangmiao/auto-pairs'
"Bundle 'Raimondi/delimitMate'
"let delimitMate_expand_cr = 1
"let delimitMate_expand_space = 1

Plug 'lambdatoast/elm.vim'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
"let g:racer_cmd = ""

"Plug 'fholgado/minibufexpl.vim'
noremap <C-TAB>   :bnext<CR>
noremap <C-S-TAB> :bprev<CR>
let g:airline#extensions#tabline#enabled = 1
set laststatus=2 " always show status bar

" Haskell
"Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
"Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
Plug 'Twinside/vim-hoogle', { 'for': 'haskell' }
"Plug 'mpickering/hlint-refactor-vim', { 'for': 'haskell' }
Plug 'bitc/vim-hdevtools'

Plug 'Shougo/neocomplete.vim'
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Python
Plug 'davidhalter/jedi-vim'
Plug 'ivanov/vim-ipython'

autocmd FileType python setlocal omnifunc=jedi#completions
    let g:jedi#completions_enabled = 0
    let g:jedi#auto_vim_configuration = 0
    let g:jedi#smart_auto_mappings = 0
    "let g:neocomplete#force_omni_input_patterns.python =
    "\ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
    " alternative pattern: '\h\w*\|[^. \t]\.\w*'

" == neco-ghc ==

let g:haskellmode_completion_ghc = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

Plug 'ervandew/supertab'

" == supertab ==

let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

if has("gui_running")
  imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
  if has("unix")
    inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
  endif
endif

Plug 'godlygeek/tabular'
let g:haskell_tabular = 1

au FileType haskell vmap a= :Tabularize /=<CR>
au FileType haskell vmap a; :Tabularize /::<CR>
au FileType haskell vmap a- :Tabularize /-><CR>

Plug 'Shougo/vimproc.vim', { 'do': 'make' }
"https://github.com/eagletmt/ghcmod-vim/wiki/Customize
"autocmd BufWritePost *.hs GhcModCheckAndLintAsync

"au FileType haskell nmap K :silent :HoogleInfo<CR>
"au FileType haskell vmap K :silent :HoogleInfo<CR>

au Filetype haskell nmap <leader>t :HdevtoolsType<CR>
"au Filetype haskell nmap <leader>i :GhcModTypeInsert<CR>
"au Filetype haskell nmap <silent> ts :GhcModSplitFunCase<CR>

"Turn off showing matching < in haskell
au FileType haskell setlocal matchpairs-=<:>         " show matching <> (html mainly) as well

" Idris
Plug 'idris-hackers/idris-vim'

" Clojure
"Plug 'guns/vim-clojure-static'
Plug 'tpope/vim-fireplace'
Plug 'vim-scripts/paredit.vim'
Plug 'guns/vim-clojure-highlight'
let g:ycm_filetype_blacklist = {'clojure': 1}

" This should enable Emacs like indentation
let g:clojure_fuzzy_indent=1
let g:clojure_align_multiline_strings = 1
 
" Add some words which should be indented like defn etc: Compojure/compojure-api, midje and schema stuff mostly.
let g:clojure_fuzzy_indent_patterns=['^GET', '^POST', '^PUT', '^DELETE', '^ANY', '^HEAD', '^PATCH', '^OPTIONS', '^def']
autocmd FileType clojure setlocal lispwords+=describe,it,testing,facts,fact,provided

Plug 'vim-airline/vim-airline'

" All of your Plugins must be added before the following line
call plug#end()

filetype plugin indent on    " required


" ==========================================================
" Shortcuts
" ==========================================================
let mapleader="\\"

" Seriously, guys. It's not like :W is bound to anything anyway.
command! W :w
" close buffers instead of windows
"cnoreabbrev wq w<bar>bd
"cnoreabbrev q bd

" sudo write this
cmap W! w !sudo tee % >/dev/null

" open/close the quickfix window
nmap <leader>c :copen<CR>
nmap <leader>cc :cclose<CR>

nmap <leader>l :llist<CR>
nmap <leader>n :lnext<CR>
nmap <leader>p :lprev<CR>

" and lets make these all work in insert mode too ( <C-O> makes next cmd
"  happen as if in command mode )
imap <C-W> <C-O><C-W>

" Load the Mundo window
map <leader>u :MundoToggle<CR>

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
set history=1000

" http://stackoverflow.com/questions/2414626/vim-unsaved-buffer-warning
set hidden

" don't bell or blink
set noerrorbells
set vb t_vb=

" Ignore these files when completing
set wildignore+=*.o,*.obj,*/.git/*,*.pyc,*.pyo,*/ios/*,*.a,*/node_modules/*,*/dist/*,*/js/vendor/app/components/*

"grep stuff
" set grepprg=ag " replace the default grep program with ag
autocmd QuickFixCmdPost *grep* cwindow "should open quickfix with any grep command results

" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

""" Insert completion
" don't select first item, follow typing in autocomplete
set completeopt=menuone,longest,preview
"set completeopt=menuone,longest
set pumheight=8             " Keep a small completion window

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
set tabstop=4               " <tab> inserts 4 spaces
set shiftwidth=4            " but an indent level is 2 spaces wide.
set softtabstop=4           " <BS> over an autoindent deletes both spaces.
set expandtab              " Use spaces, not tabs, for autoindent/tab key.
set shiftround              " rounds indent to a multiple of shiftwidth
set matchpairs+=<:>         " show matching <> (html mainly) as well
set foldmethod=indent       " allow us to fold on indents
set foldlevel=4            " don't fold by default

"""" Reading/Writing
set noautowrite             " Never write a file unless I request it.
set noautowriteall          " NEVER.
set noautoread              " Don't automatically re-read changed files.
set modeline                " Allow vim options to be embedded in files;
set modelines=5             " they must be within the first or last 5 lines.
set ffs=unix,dos,mac        " Try recognizing dos, unix, and mac line endings.

"""" Messages, Info, Status
set vb t_vb=                " Disable all bells.  I hate ringing/flashing.
set confirm                 " Y-N-C prompt if closing with unsaved changes.
set showcmd                 " Show incomplete normal mode commands as I type.
set report=0                " : commands always print changed line count.
set shortmess+=a            " Use [+]/[RO]/[w] for modified/readonly/written.

""" Searching and Patterns
set ignorecase              " Default to using case insensitive searches,
set smartcase               " unless uppercase letters are used in the regex.
set smarttab                " Handle tabs more intelligently
set hlsearch                " Highlight searches by default.
set incsearch               " Incrementally search while typing a /regex

" Enable folding with the spacebar
nnoremap <space> za

" Paste from clipboard
"map <leader>p "+gP

" Quit window on <leader>q
nnoremap <leader>q :q<CR>
"
" hide matches on <leader>space
nnoremap <leader><space> :nohlsearch<CR>

" ===========================================================
" FileType specific changes
" ============================================================
" Mako/HTML
autocmd BufNewFile,BufRead *.mako,*.mak setlocal ft=html
autocmd FileType html,xhtml,xml,css setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" Python
au FileType python setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4 encoding=utf-8

" Haskell
au FileType haskell nnoremap <silent> <leader><space> :nohlsearch<CR>:HdevtoolsClear<CR>:HoogleClose<CR>

"Elm
au FileType elm setlocal expandtab shiftwidth=2 tabstop=4 softtabstop=2

" Go
"Import the package under your cursor with <leader>i (useful if you have 
"disabled auto import via GoDisableGoimport)
au FileType go nmap <Leader>i <Plug>(go-import)

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

au BufRead,BufNewFile {*.go}    setl ft=go tabstop=4 softtabstop=4 noexpandtab smarttab

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


" vim haskell now stuff

" Don't redraw while executing macros (good performance config)
"set lazyredraw

" Source the vimrc file after saving it
augroup sourcing
  autocmd!
  if has('nvim')
    autocmd bufwritepost init.vim source $MYVIMRC
  else
    autocmd bufwritepost .vimrc source $MYVIMRC
  endif
augroup END

" Show types in completion suggestions
"let g:necoghc_enable_detailed_browse = 1
" Resolve ghcmod base directory
au FileType haskell let g:ghcmod_use_basedir = getcwd()

" Open file prompt with current path
nmap <leader>e :e <C-R>=expand("%:p:h") . '/'<CR>

let g:ctrlp_max_files=0
let g:ctrlp_show_hidden=1
let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$' }

" Word wrap markdown files automatically
augroup WrapLineInTeXFile
    autocmd!
    autocmd FileType markdown setlocal wrap
augroup END

autocmd FileType html setlocal shiftwidth=2 tabstop=2
autocmd FileType cpp setlocal shiftwidth=2 tabstop=2
autocmd FileType hpp setlocal shiftwidth=2 tabstop=2
