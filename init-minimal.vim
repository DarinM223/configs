"
" Darin's configuration file for vim/neovim
"
" My relatively minimal vimrc/init.vim file
" Most plugins are mostly simple and don't use too much vim magic
"
" I have lots of custom setups for different languagues like Clojure, Rust,
" and Go, so if you do not use these languages, you might be better off with
" a simpler vimrc.
"
" Set up instructions
"
" 1. Copy this file to ~/.vimrc (or ~/.config/nvim/init.vim if you are using
" neovim)
" 2. Install vim-plug
" 3. Open vim/nvim and type :PlugInstall

"
" Autocommands
"
" Sets relative line numbering when entering a buffer, set line highlight
" on cursor, and set timeout from visual mode for nerdcommenter
autocmd BufEnter * set relativenumber | set nu | set cursorline | set timeoutlen=5000
" Exits completion preview window when exiting insert mode
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" Fixes weird cursor bug in guake and other terminals.
let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 0
set guicursor=
" Workaround some broken plugins which set guicursor indiscriminately.
autocmd OptionSet guicursor noautocmd set guicursor=

" Install vim-plug first
call plug#begin('~/.vim/plugged') " required

set laststatus=2

syntax on
syntax enable

filetype off                  " required!

"
" Plugins
"

" Essential plugins: Every vim fanboy needs to have these plugins!

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'scrooloose/nerdtree'        " project file explorer on left pane: open with <C-n>
Plug 'jistr/vim-nerdtree-tabs'    " doesn't glitch when using vim tabs
Plug 'tpope/vim-fugitive'         " git functions inside vim
Plug 'justinmk/vim-sneak'         " jump to code with s key and two letters
Plug 'scrooloose/syntastic.git'   " display errors in code
Plug 'scrooloose/nerdcommenter'   " comment with \cl or \cc and undo with \cu
Plug 'ctrlpvim/ctrlp.vim'         " Just like sublime's ctrl-p fuzzy search through project
Plug 'mattn/emmet-vim'            " HTML Zen coding plugin <C-y>, to activate
Plug 'alvan/vim-closetag'         " Close tags automatically in HTML-like files

" themes
Plug 'vivkin/flatland.vim'
Plug 'jordwalke/flatlandia'
Plug 'morhetz/gruvbox'

" language based plugins
Plug 'autozimu/LanguageClient-neovim'    " LSP client for neovim.
Plug 'adimit/prolog.vim'                 " prolog
Plug 'pangloss/vim-javascript'           " javascript
Plug 'marijnh/tern_for_vim'              " javascript autocompletion
Plug 'mxw/vim-jsx'                       " jsx
Plug 'digitaltoad/vim-jade'              " jade
Plug 'derekwyatt/vim-scala'              " scala
Plug 'fatih/vim-go'                      " golang
Plug 'rust-lang/rust.vim'                " rust syntax
Plug 'timonv/vim-cargo'                  " Rust cargo runner
Plug 'Chiel92/vim-autoformat'            " custom formatting with rustfmt integration
Plug 'tpope/vim-fireplace'               " clojure dynamic evaluation
Plug 'edwinb/idris2-vim'                 " idris mode
Plug 'ziglang/zig.vim'                   " zig
Plug 'purescript-contrib/purescript-vim' " purescript
Plug 'jpalardy/vim-slime'                " send-to-repl for haskell and python

" Clojure structured editing of lisp s expressions
" Go to ~/.vim/bundle/parinfer-rust and run `cargo build --release`.
Plug 'eraserhd/parinfer-rust'

Plug 'nvie/vim-flake8'                     " python linter plugin
Plug 'elixir-lang/vim-elixir'              " elixir highlighting and indentation
Plug 'mtscout6/syntastic-local-eslint.vim' " Prefer local eslint over global
Plug 'dag/vim2hs'                          " haskell tools

call plug#end() " required
filetype plugin indent on " required

"
" Environment variables
"
set nocompatible
set nofoldenable
set background=dark
set backspace=indent,eol,start
set tabstop=4
set expandtab
set shiftwidth=4
set nu

" clear background color in tmux
if &term =~ '256color'
    set t_ut=
endif

"
" Keyboard mappings
"
if has('nvim')
    " Escape from terminal mode in neovim with Esc
    tnoremap <Esc> <C-\><C-n>
    " Drop highlighting when redrawing with Ctrl-l
    nnoremap <silent> <C-l> :nohlsearch<CR><C-l>
endif
" Ctrl-a selects all text in file
map <C-a> <esc>gg0vG$<CR>
" Ctrl-n toggles NERDTree
map <C-n> :NERDTreeTabsToggle<CR>
" Ctrl-c copies to clipboard
vmap <C-C> "+y
" Ctrl-t opens a new tab
nnoremap <C-t> :tabnew<CR>
" Ctrl-f formats using Neoformat (right now only formatting Haskell).
nnoremap <C-f> :Neoformat<CR>

" Meta (Alt) key with the left and right arrow keys will move a tab
" to the left/right position.
map <A-Left> :-tabmove<CR>
map <A-Right> :+tabmove<CR>

nmap <F9> :TagbarToggle<CR>
noremap <F5> :Autoformat<CR>

" Switches between windows with Ctrl-keys
" with keys being jkhl similar to normal vim navigation.
noremap <C-J>j <C-W>w
noremap <C-K>k <C-W>W
noremap <C-L>l <C-W>l
noremap <C-H>h <C-W>h

"
" Vim Explore mode configuration
" Configures the presentation of the Vim explore mode
" and hides certain file types
"
let g:netrw_liststyle=3 " Visual style that allows you to expand folders
let g:netrw_list_hide = '.*\.swp$,.*\.pyc$,.*\.bk$' " Hide .pyc, .bk, and .swp files

"
" NERDTree configuration
"
let NERDTreeShowLineNumbers=1
let NERDTreeIgnore = ['\.pyc$', '\.bk$', '\.swp$'] " Hide .pyc, .swp and .bk files

if !has('nvim')
    " Set terminal to 256 color
    set term=xterm-256color
endif

if has('gui_running')
    colorscheme flatlandia
else
    set mouse=a
    colorscheme gruvbox
endif

" LanguageClient-neovim configuration
let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ }
let g:LanguageClient_useVirtualText = 0
let g:LanguageClient_diagnosticsMaxSeverity = "Error"
let g:LanguageClient_diagnosticsDisplay =
            \ {
            \     1: {
            \         "name": "Error",
            \         "texthl": "ALEError",
            \         "signText": "",
            \         "signTexthl": "ALEErrorSign",
            \         "virtualTexthl": "Error",
            \     },
            \     2: {
            \         "name": "Warning",
            \         "texthl": "ALEWarning",
            \         "signText": "⚠",
            \         "signTexthl": "ALEWarningSign",
            \         "virtualTexthl": "Todo",
            \     },
            \     3: {
            \         "name": "Information",
            \         "texthl": "ALEInfo",
            \         "signText": "ℹ",
            \         "signTexthl": "ALEInfoSign",
            \         "virtualTexthl": "Todo",
            \     },
            \     4: {
            \         "name": "Hint",
            \         "texthl": "ALEInfo",
            \         "signText": "➤",
            \         "signTexthl": "ALEInfoSign",
            \         "virtualTexthl": "Todo",
            \     },
            \ }

let g:go_fmt_command = "gofmt"
let g:go_fmt_autosave = 1
let $GOPATH = "/Users/darin/go"
let $GOROOT="/usr/local/Cellar/go/1.6/libexec"

let purescript_indent_where = 1
let purescript_indent_case = 2
let purescript_indent_do = 2
let purescript_indent_in = 0

"
" coc.nvim Configuration
"
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

"
" Syntastic Configuration
"
let g:syntastic_enable_highlighting=0
let g:syntastic_python_flake8_args='--ignore=E501, E225'

" NOTE: if you want syntastic to check eslint you have to
" install eslint using npm
" let g:syntastic_javascript_checkers = ['eslint'] " uncomment for eslint

" NOTE: if you want syntastic to check standard you have to install
" it using npm
" Uses javascript standard style (https://github.com/feross/standard)
" by default
let g:syntastic_javascript_checkers = ['standard']

let g:syntastic_always_populate_loc_list=1

" prevent scala files from taking forever to save
let g:syntastic_mode_map = {'mode': 'active',
            \ 'passive_filetypes': ['java', 'scala', 'python', 'go'] }
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_haskell_checkers = ['hlint']

"
" vim-autoformat configuration for rustfmt and scalafmt
"
" NOTE: Installing rustfmt:
" from normal rust installation:
" cargo install rustfmt
" from multirust:
" multirust run nightly cargo install rustfmt
"
" NOTE: Installing scalafmt:
" brew tap olafurpg/scalafmt
" brew install scalafmt
"
let g:formatdef_rustfmt = '"rustfmt"'
let g:formatters_rust = ['rustfmt']
let g:formatdef_scalafmt = '"scalafmt"'
let g:formatters_scala = ['scalafmt']

"
" Ctrl-P configuration
"
" Allows tag search in Ctrlp
let g:ctrlp_extensions = ['tag', 'line']
let g:ctrlp_max_files = 0
let g:ctrlp_working_path_mode = 0

if exists("g:ctrlp_user_command")
    unlet g:ctrlp_user_command
endif
if executable('ag')
    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command =
       \ 'ag %s --files-with-matches -g "" --ignore "\.git$\|\.hg$\|\.svn$\|node_modules$\|bower_components$\|env$"'

    " ag is fast enough that CtrlP doesn't need to cache
    let g:ctrlp_use_caching = 0
else
    " Fall back to using git ls-files if Ag is not available
    let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$\|node_modules$\|bower_components$\|env$'
    let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']
endif

"
" vim-slime configuration
"
" Press Ctrl-c Ctrl-c to send the selection to the repl.
" To find the tmux id of the repl pane, run `echo $TMUX_PANE` in the pane.
let g:slime_target = "tmux"

"
" CloseTag.vim configuration
"
" close tags for html type files
let g:closetag_filenames = "*.html,*.xhtml,*.xml,*.erb,*.jsx,*.php,*.ejs,*.hbs"

"
" File indentation settings
"
au FileType python setl sw=4 sts=4 et
au FileType rust setl sw=4 sts=4 et
au FileType javascript setl sw=2 sts=2 et
au FileType cpp setl sw=2 sts=2 et
au FileType c setl sw=4 sts=4 et
au FileType ruby setl sw=2 sts=2 et
au FileType java setl sw=4 sts=4 et
au FileType scala setl sw=2 sts=2 et
au FileType vim setl sw=4 sts=4 et
au FileType php setl sw=4 sts=4 et
au FileType haskell setl sw=2 sts=2 et
au FileType purescript setl sw=2 sts=2 et
au FileType verilog setl sw=4 sts=4 et
au FileType matlab setl sw=2 sts=2 et
au FileType html setl sw=2 sts=2 et
au FileType xml setl sw=2 sts=2 et
au FileType eruby setl sw=2 sts=2 et
au FileType yaml setl sw=2 sts=2 et
au Filetype go set tabstop=4
au FileType cs setl sts=4 noexpandtab

au BufNewFile,BufRead *.ejs set filetype=html