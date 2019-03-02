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
" 2. Clone vundle as ~/.vim/bundle/vundle
" 3. Open vim/nvim and type :PluginInstall
" 4. If you want autocomplete, cd into ~/.vim/bundle/YouCompleteMe
" and run ./install.py --clang-completer --gocode-completer --rust-completer
" 5. Edit the Rust configuration to add the path to the rust source code

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

set rtp+=~/.vim/bundle/vundle/      " vundle needs to installed at ~/.vim/bundle/vundle to install packages
set rtp^=~/.vim/bundle/ctrlp.vim
set laststatus=2

syntax on
syntax enable

filetype off                  " required!

call vundle#begin()

"
" Plugins
"

Plugin 'gmarik/vundle'              " main package manager - don't remove!

" Essential plugins: Every vim fanboy needs to have these plugins!

" NOTE: To install YouCompleteMe, cd into the bundle/YouCompleteMe and run
" ./install.py --clang-completer --gocode-completer --racer-completer which will install YouCompleteMe, 
" clang completion, and racer (Rust) completion
Plugin 'Valloric/YouCompleteMe'     " autocomplete either automatically or with <C-space>
Plugin 'rdnetto/YCM-Generator'      " generate .ycm_extra_conf.py files automatically
Plugin 'scrooloose/nerdtree'        " project file explorer on left pane: open with <C-n>
Plugin 'jistr/vim-nerdtree-tabs'    " doesn't glitch when using vim tabs
Plugin 'majutsushi/tagbar'          " lists functions on right pane open with <F9>
Plugin 'tpope/vim-fugitive'         " git functions inside vim
Plugin 'justinmk/vim-sneak'         " jump to code with s key and two letters 
Plugin 'scrooloose/syntastic.git'   " display errors in code
Plugin 'scrooloose/nerdcommenter'   " comment with \cl or \cc and undo with \cu
Plugin 'bling/vim-airline'          " customized bar on bottom that changes color on different modes
Plugin 'tpope/vim-surround'         " Manipulate parenthesis, brackets, quotes, etc easily 
Plugin 'kien/ctrlp.vim'             " Just like sublime's ctrl-p fuzzy search through project
Plugin 'mattn/emmet-vim'            " HTML Zen coding plugin <C-y>, to activate
Plugin 'alvan/vim-closetag'         " Close tags automatically in HTML-like files
 
" themes
Plugin 'Lokaltog/vim-distinguished'
Plugin 'vivkin/flatland.vim'
Plugin 'jordwalke/flatlandia'
Plugin 'ajh17/Spacegray.vim'
Plugin 'sickill/vim-monokai'
Plugin 'morhetz/gruvbox'

" language based plugins
Plugin 'adimit/prolog.vim'                 " prolog
Plugin 'pangloss/vim-javascript'           " javascript
Plugin 'marijnh/tern_for_vim'              " javascript autocompletion
Plugin 'mxw/vim-jsx'                       " jsx
Plugin 'digitaltoad/vim-jade'              " jade
Plugin 'derekwyatt/vim-scala'              " scala
Plugin 'fatih/vim-go'                      " golang
Plugin 'rust-lang/rust.vim'                " rust syntax
Plugin 'timonv/vim-cargo'                  " Rust cargo runner
Plugin 'Chiel92/vim-autoformat'            " custom formatting with rustfmt integration
Plugin 'tpope/vim-fireplace'               " clojure dynamic evaluation
Plugin 'idris-hackers/idris-vim'           " idris mode
Plugin 'purescript-contrib/purescript-vim' " purescript

" Clojure structured editing of lisp s expressions
" Go to ~/.vim/bundle/parinfer-rust and run `cargo build --release`.
Plugin 'eraserhd/parinfer-rust'

" Slime-like Common Lisp environment for vim
" To install:
" 1. Install ncat (in Ubuntu `sudo apt-get install nmap`)
" 2. Install QuickLisp
"    a. curl -O https://beta.quicklisp.org/quicklisp.lisp
"    b. sbcl --load quicklisp.lisp
"    c. (quicklisp-quickstart:install)
"    d. (ql:add-to-init-file)
" 3. sbcl --load <your bundle dir>/vlime/lisp/start-vlime.lisp
"
" Leader key is '\', for help with commands use '\?' in normal mode.
Plugin 'l04m33/vlime', { 'rtp': 'vim/' }

Plugin 'nvie/vim-flake8'            " python linter plugin
Plugin 'elixir-lang/vim-elixir'     " elixir highlighting and indentation
Plugin 'mtscout6/syntastic-local-eslint.vim' " Prefer local eslint over global

Plugin 'dag/vim2hs'                 " haskell tools

" Autoreloads code in GHC and fills quickfix window with errors.
" Type `:cw` to open the quickfix window when there are errors and
" `:ccl` to close the quickfix window. Type `:Ghcid` to open and close
" a Ghcid window (for more complete error messages).
" First install ghcid with 'cabal install ghcid'
Plugin 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
" For formatting Haskell code using brittany.
Plugin 'sbdchd/neoformat'

call vundle#end()         " required
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
    " colorscheme spacegray
    colorscheme flatlandia
else
    set mouse=a
    " colorscheme spacegray
    " colorscheme distinguished
    " colorscheme monokai
    colorscheme gruvbox
endif

"
" YouCompleteMe Configuration
" NOTE: When using on a new computer don't forget to:
" 1) cd into the bundle/YouCompleteMe folder
" 2) run ./install.sh --clang-completer to install with C/C++ autocomplete
"
let g:ycm_path_to_python_interpreter = '/usr/bin/python'
let g:ycm_add_preview_to_completeopt=1
let g:ycm_autoclose_preview_window_after_completion=0
let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
" The path to the rust source code for Rust autocompletion
let g:ycm_rust_src_path = "/home/d/Documents/git/rust/rust"

let g:go_fmt_command = "gofmt"
let g:go_fmt_autosave = 1
let $GOPATH = "/Users/darin/go"
let $GOROOT="/usr/local/Cellar/go/1.6/libexec"

set hidden " Used for ghcid so that buffer closes when there are no errors
let g:neoformat_enabled_haskell = ['brittany']
let g:vlime_contribs = ['SWANK-ASDF', 'SWANK-PACKAGE-FU', 'SWANK-PRESENTATIONS', 'SWANK-FANCY-INSPECTOR', 'SWANK-C-P-C', 'SWANK-ARGLISTS', 'SWANK-REPL', 'SWANK-FUZZY', 'SWANK-TRACE-DIALOG']

" Ties Eclim with YCM autocomplete
let g:EclimCompletionMethod='omnifunc'

let purescript_indent_where = 1
let purescript_indent_case = 2
let purescript_indent_do = 2
let purescript_indent_in = 0

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
            \ 'passive_filetypes': ['java', 'scala'] }
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_haskell_checkers = ['hlint']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

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

" NOTE: Install standard-format first with 
" npm install -g standard-format
"autocmd bufwritepost *.js silent !standard-format -w %
"set autoread

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

""
"" Airline configuration
""
"" Fancy style for airline
"let g:airline_powerline_fonts = 1
"filetype plugin indent on     " required!

"" Airline updates when escaping from insert mode
"if ! has('gui_running')
"    set ttimeoutlen=10
"    augroup FastEscape
"        autocmd!
"        au InsertEnter * set timeoutlen=0
"        au InsertLeave * set timeoutlen=5000
"    augroup END
"endif

"
" CloseTag.vim configuration
"
" close tags for html type files
let g:closetag_filenames = "*.html,*.xhtml,*.xml,*.erb,*.jsx,*.php,*.ejs,*.hbs"


" For code folding (uncomment the two lines below to enable)
"autocmd BufWrite * mkview
"autocmd BufRead * silent loadview

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
