set hidden
set nu
set relativenumber
set incsearch
set list
set listchars=eol:␍,tab:»\ ,extends:»,precedes:«
set tabstop=2
set shiftwidth=2
set expandtab
set shiftround
set ignorecase
set smartcase
set smartindent
set smarttab
set whichwrap=b,s,h,l,<,>,[,]
set wildmenu
set confirm
set autoread
set signcolumn=yes
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300

let s:nvim_home = expand('~/.config/nvim')
let s:cache_path = empty($XDG_CACHE_HOME) ? expand('~/.cache') : $XDG_CACHE_HOME
let s:dein_path = s:cache_path . '/dein'
let s:dein_repo_path = s:dein_path . '/repos/github.com/Shougo/dein.vim'

let &runtimepath = s:dein_repo_path .','. &runtimepath

if dein#load_state(s:dein_path)
  call dein#begin(s:dein_path)

  call dein#load_toml(s:nvim_home . '/rc/dein.toml',      { 'lazy': 0 })
  call dein#load_toml(s:nvim_home . '/rc/dein_lazy.toml', { 'lazy': 1 })

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

nnoremap <expr>gp '`[' . strpart(getregtype(), 0, 1) . '`]'

vnoremap < <gv
vnoremap > >gv

inoremap <C-a> <C-o>I
inoremap <C-e> <C-o>A
inoremap <C-w> <C-o><C-w>
inoremap <C-CR> <C-o>O
nnoremap x "_x

cnoremap <C-a> <HOME>
cnoremap <C-e> <END>

" Exit terminal insert mode
tmap <C-o> <C-\><C-r>

colorscheme forest-night

" Support jsonc syntax
autocmd FileType json syntax match Comment +\/\/.\+$+

command! VimrcEdit call s:open_vimrc()
function! s:open_vimrc()
  edit $MYVIMRC
endfunction

command! NvimFiles call s:open_nvim_dir()
function! s:open_nvim_dir()
  exec 'edit' s:nvim_home
endfunction
