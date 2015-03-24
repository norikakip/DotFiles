set encoding=utf-8
scriptencoding

function! IncludePath(path)
  "define delimiter depends on platform
  if has('win16') || has('has32') || has('win64')
    let delimiter = ";"
  else
    let delimiter = ":"
  endif
  let pathlist = split($PATH, delimiter)
  if isdirectory(a:path) && index(pathlist, a:path) == -1
    let $PATH=a:path.delimiter.$PATH
  endif
endfunction
call IncludePath(expand("~/.pyenv/shims"))
call IncludePath(expand("~/.rbenv/shims"))
"----------------------------------------------------------------
"基本的な設定"
"----------------------------------------------------------------

"オートコマンドの多重読み込み防止（破棄）
autocmd!
" Dropboxフォルダの設定
if has('win64')
  let g:DropBox_dir = '/c/Users/kaki/Dropbox'
elseif has('win32unix')
  let g:DropBox_dir = '/cygdrive/c/Users/kaki/Dropbox'
elseif has('mac')
  let g:DropBox_dir = '/home/kaki/Dropbox'
elseif has('unix')
  let g:DropBox_dir = '/home/kaki/Dropbox'
endif

set fileformats=unix,dos
" 新しい行のインデントを現在行と同じにする
" set autoindent
" バックアップファイルを作るディレクトリ
set backupdir=~/.vim/vimbackup
" undofile dir
set undodir=~/.vim/vimbackup
" ファイル保存ダイアログの初期ディレクトリをバッファファイル位置に設定
set browsedir=buffer
" clipbord compatible with OS
set clipboard+=unnamed

" スワップファイル用のディレクトリ
set directory=~/.vim/vimbackup

" タブの代わりに空白文字を挿入する
set expandtab
" 変更中のファイルでも、保存しないで他のファイルを表示
set hidden

set history=5000
" インクリメンタルサーチを行う
set incsearch
" タブ文字、行末など不可視文字を表示する
set list
" listで表示される文字のフォーマットを指定する
set listchars=eol:␍,tab:»\ ,extends:»,precedes:«
" 行番号を表示する
set number
" ファイル内の <Tab> が対応する空白の数
set tabstop=2
" シフト移動幅
set shiftwidth=2
set shiftround
" 閉じ括弧が入力されたとき、対応する括弧を表示する
set showmatch
" 検索時に大文字小文字を区別しない
set ignorecase
" 検索時に大文字を含んでいたら大/小を区別
set smartcase
" 新しい行を作ったときに高度な自動インデントを行う
set smartindent
" 行頭の余白内で Tab を打ち込むと、'shiftwidth' の数だけインデントする。
set smarttab
" カーソルを行頭、行末で止まらないようにする
set whichwrap=b,s,h,l,<,>,[,]
" arrow BackSpace
set backspace=start,indent,eol
" 検索をファイルの先頭へループしない
" set nowrapscan
" shellslash設定（デフォルト）
set shellslash
" コマンドライン補完
set wildmenu
" バッファに変更があったときエラーではなく確認する
set confirm
" ファイルが外部で変更された時読みなおす
set autoread
" 78字目を可視
" set colorcolumn=78
" ビープの代わりにvisualbellの使用と無効化
set visualbell
set t_vb=
" color
set t_Co=256
"preview windowを表示しない
set completeopt-=preview
" 補完ポップメニュー項目数の最大値
set pumheight=80
set scrolloff=15


"----------------------------------------------------------------
"キーマップの設定 (プラグインを使わない) mapping"
"----------------------------------------------------------------

" mouseを無効化
inoremap <LeftMouse> <Nop>
inoremap <LeftDrag> <Nop>
inoremap <LeftRelease> <Nop>
" if has('windows')
"   inoremap <LeftMouse> <ESC><LeftMouse>
"   cnoremap <LeftMouse> <ESC><LeftMouse>
" endif

nnoremap <silent><ESC><ESC> :set nohlsearch<Enter>
inoremap <expr><A-x>day strftime('%Y-%m-%d')
inoremap <expr><A-x>time strftime('%Y-%m-%d %H:%M')
inoremap <silent><expr><A-x>uname :r!uname -a<Enter>
" 直前にペーストした行を選択
nnoremap <expr>gp '`[' . strpart(getregtype(), 0, 1) . '`]'

" 選択範囲を維持したままShift
vnoremap < <gv
vnoremap > >gv
"TIPS: textobj-indentプラグインを用いると vii で同じインデントサイズの行を選択可

inoremap <C-a> <C-o>I
inoremap <C-e> <C-o>A
inoremap <C-w> <C-o><C-w>
nnoremap <silent><C-l> :tabnext<Enter>
nnoremap <silent><C-h> :tabprevious<Enter>
inoremap <silent><C-l> <C-o>:tabnext<Enter>
inoremap <silent><C-h> <C-o>:tabprevious<Enter>

cnoremap <C-a> <HOME>
cnoremap <C-e> <END>

"----------------------------------------------------------------
" NeoBundleによるプラグインの設定"
"----------------------------------------------------------------
filetype off
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
  call neobundle#begin(expand('~/.vim/bundle'))
endif

" util
NeoBundleFetch 'Shougo/neobundle.vim'
if !has('Kaoriya')
  NeoBundle 'Shougo/vimproc', {
        \ 'build' : {
        \   'windows' : 'make -f make_mingw64.mak',
        \   'unix'    : 'gmake',
        \   'cygwin'  : 'make -f make_cygwin.mak',
        \   'mac'     : 'make -f make_mac.mak',
        \   'linux'   : 'make',
        \ },
        \}
else
  NeoBundleFetch 'Shougo/vimproc'
endif
NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/neossh.vim' " SSH interface for Vim plugins
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/tabpagebuffer.vim'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'Shougo/vimfiler'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'vimplugin/project.vim' "project管理

NeoBundle 'vim-scripts/ShowMarks' "マークを可視化
NeoBundle 'vim-scripts/showmarks2'
NeoBundle 'Lokaltog/vim-easymotion' "キーカーソル移動補助
NeoBundle 'vim-scripts/Justify' "両端揃え
NeoBundle 'h1mesuke/vim-alignta' "テキスト整形
NeoBundle 'kana/vim-textobj-user' "テキストオブジェクト拡張
NeoBundle 'kana/vim-textobj-indent' "テキストオブジェクト拡張
" NeoBundle 'kana/vim-textobj-jabraces' 
NeoBundle 'koron/minimap-vim'
NeoBundle 'vim-jp/vimdoc-ja' " A project which translate Vim documents into Japanese.
" NeoBundle 'fuenor/JpFormat.vim'
NeoBundle 'vim-scripts/autodate.vim'
NeoBundle 'tyru/eskk.vim'
NeoBundle 'fuenor/qfixhowm'
NeoBundle 'mattn/webapi-vim'
" NeoBundle 'Lokaltog/vim-powerline'
" NeoBundle 'Lokaltog/powerline', { 'rtp' : 'powerline/bindings/vim'}
NeoBundle 'bling/vim-airline' " lean & mean statusline for vim that's light as air
NeoBundle 'kmnk/vim-unite-giti'
NeoBundle 'tsukkee/unite-help' " help source for unite.vim
NeoBundle 'thinca/vim-unite-history' " A source of unite.vim for history of command/search.

"colorscheme
NeoBundle 'tomasr/molokai' "colorscheme
NeoBundle 'sickill/vim-monokai'
NeoBundle 'altercation/vim-colors-solarized' " precision colorscheme for the vim text editor
NeoBundle 'chriskempson/vim-tomorrow-theme'
NeoBundle 'w0ng/vim-hybrid'
NeoBundle 'morhetz/gruvbox'
NeoBundle 'jpo/vim-railscasts-theme'
NeoBundle 'vim-scripts/Wombat'
NeoBundle 'vim-scripts/wombat256.vim'
NeoBundle 'vim-scripts/ecostation'

"cording plugin
NeoBundle 'Shougo/neocomplcache', {
      \ 'disabled' : has('lua')
      \}
NeoBundle 'Shougo/neocomplete', {
      \ 'disabled' : !has('lua'),
      \ 'vim_version' : '7.3.885',
      \}
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'osyo-manga/unite-quickfix'
NeoBundle 'osyo-manga/shabadou.vim'
" NeoBundle 'scrooloose/syntastic' " Syntax checking hacks for vim
NeoBundle 'nathanaelkane/vim-indent-guides' "インデントにバーを表示
NeoBundle 'tpope/vim-abolish' "変数を命名規則に従って変換
NeoBundle 'tomtom/tcomment_vim' "コメントアウト機能
NeoBundle 'thinca/vim-template'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'pekepeke/ref-javadoc' " javadoc plugin for vim-ref
NeoBundle 'mojako/ref-sources.vim' " vim-ref 用の追加ソース (javascript, jquery, kotobank, wikipedia)
NeoBundle 'thinca/vim-ref' "リファレンスビューア
NeoBundle 'vim-scripts/errormarker.vim'

NeoBundle 'mattn/emmet-vim'
NeoBundle 'othree/html5.vim'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'Simple-Javascript-Indenter' " 1.0.1 A simple javascript indent script, support OOP, jquery
NeoBundle 'jelera/vim-javascript-syntax' " Enhanced javascript syntax file for Vim

NeoBundle 'tpope/vim-rails'
NeoBundle 'mattn/excitetranslate-vim'
NeoBundleFetch 'Shougo/neocomplcache-rsense' " The neocomplcache source for RSense
NeoBundle 'basyura/unite-rails' " a unite.vim plugin for rails
NeoBundle 'ujihisa/unite-rake' " A Unite.vim plugin to run tasks or to view descriptions easily, using rake command
NeoBundle 'rhysd/unite-ruby-require.vim'
NeoBundle 'rhysd/neco-ruby-keyword-args'
NeoBundle 'rhysd/vim-textobj-ruby'

NeoBundle 'Shougo/unite-outline'
NeoBundle 'tyru/open-browser.vim'
NeoBundle 'neco-look' " 1.0   A neocomplcache plugin for `/usr/bin/look` for completing words in English.
NeoBundle 'javacomplete', {
      \ 'build' : {
      \     'mac' : 'javac autoload/Reflection.java',
      \     'unix' : 'javac autoload/Reflection.java',
      \ },
      \}
NeoBundle 'kamichidu/vim-javaclasspath'
NeoBundle 'kamichidu/vim-unite-javaimport' , {
      \ 'depends': [
      \     'Shougo/unite.vim',
      \     'Shougo/vimproc.vim',
      \     'kamichidu/vim-javaclasspath',
      \     'yuratomo/w3m.vim',
      \  ],
      \}
NeoBundle 'Shougo/unite-build' " Build by unite interface
NeoBundle 'osyo-manga/unite-qfixhowm'
NeoBundle 'ujihisa/unite-colorscheme' " A unite.vim plugin
NeoBundle 'ujihisa/unite-gem' " A Unite plugin for RubyGems
NeoBundle 'tsukkee/unite-tag' 
NeoBundle 'osyo-manga/unite-quickrun_config'
NeoBundle 'rhysd/quickrun-unite-quickfix-outputter' " An outputter of QuickRun for unite-quickfix.
NeoBundle 'cakebaker/scss-syntax.vim' " Vim syntax file for scss (Sassy CSS)
NeoBundle 'vim-scripts/VimClojure' " A filetype, syntax and indent plugin for Clojure
NeoBundle 'ujihisa/unite-locate'

NeoBundle 'aharisu/vim-gdev' " Vim上でGaucheを書くお手伝いをするスクリプト
NeoBundle 'Shougo/unite-session' " unite.vim session source
NeoBundle 'Shougo/unite-sudo' " sudo source for unite.vim
NeoBundle 'osyo-manga/vim-anzu' " Vim search status.
NeoBundle 'osyo-manga/vim-watchdogs'
NeoBundle 'jceb/vim-hier'
NeoBundle 'majutsushi/tagbar' " Vim plugin that displays tags in a window, ordered by class etc.
NeoBundle 'tagbar-phpctags' "git clone https://github.com/techlivezheng/phpctags.git
NeoBundle 'mfumi/ref-dicts-en'
NeoBundle 'rhysd/clever-f.vim'
NeoBundle 'osyo-manga/vim-over' " :substitute preview
NeoBundle 'LeafCage/yankround.vim' " レジスタの履歴を取得し再利用する。
NeoBundleFetch 'osyo-manga/vim-marching' " Async clang code completion.
NeoBundle 'osyo-manga/vim-snowdrop'
NeoBundle 'shawncplus/phpcomplete.vim' " Improved PHP omnicompletion
NeoBundleFetch 'supermomonga/neocomplete-rsense.vim' " The neocomplete source for RSense
NeoBundle 'osyo-manga/vim-monster', {
      \ 'disabled' : !has('ruby'),
      \ 'build' : {
      \     'mac' : 'gem install rcodetools',
      \     'unix' : 'gem install rcodetools',
      \ }
      \}
NeoBundle 'mackee/unite-httpstatus'
NeoBundle 'Shougo/vinarise.vim' " Ultimate hex editing system with Vim
NeoBundle 'davidhalter/jedi-vim', {
      \ 'disabled' : !has('python'),
      \ 'build' : {
      \     'mac' : 'pip install jedi',
      \     'unix' : 'pip install jedi',
      \ },
      \}
NeoBundle 'tacroe/unite-mark'
NeoBundle 'superbrothers/vim-quickrun-markdown-gfm'
NeoBundle 'sudar/vim-arduino-syntax'
NeoBundle 't9md/vim-quickhl'

NeoBundleLazy 'lambdalisue/vim-gista', {
      \ 'autoload' : {
      \   'commands': ['Gista'],
      \   'mappings': '<Plug>(gista-',
      \   'unite_sources': 'gista',
      \}}
NeoBundleFetch 'lambdalisue/vim-pyenv', {
      \ 'depends': ['davidhalter/jedi-vim'],
      \ 'autoload': {
      \   'filetypes': ['python', 'python3', 'djangohtml']
      \ }}
" NeoBundleLazy 'lambdalisue/vim-django-support', {
"       \ 'autoload': {
"       \   'filetypes': ['python', 'python3', 'djangohtml']
"       \ }}
NeoBundleLazy 'alpaca-tc/vim-unite-watson.vim', {
      \ 'commands' : 'Watson',
      \ 'depends' : 'Shougo/unite.vim',
      \ 'autoload' : {
      \   'unite_sources' : ['watson', 'watson/dirty', 'watson/clean', 'watson/current_file'],
      \ }}
NeoBundle 'vim-scripts/AnsiEsc.vim'
NeoBundle 'kana/vim-operator-user'
NeoBundle 'kana/vim-operator-replace'
NeoBundle 'todesking/ruby_hl_lvar.vim', {
      \ 'disabled' : !has('lua'),
      \ 'autoload' : {
      \   'filetypes' : ['ruby', 'ruby.rspec'],
      \ }
      \}

NeoBundle 'yuku-t/vim-ref-ri' " A vim-ref and Unite.vim source for ri.
NeoBundle 'thinca/vim-localrc'
NeoBundle 'Yggdroot/indentLine'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'yuratomo/w3m.vim.git'
NeoBundle 'cohama/lexima.vim'
NeoBundle 'koron/codic-vim'
NeoBundle 'rhysd/unite-codic.vim'
NeoBundle 'mtth/scratch.vim'
NeoBundle 'haya14busa/incsearch.vim'
NeoBundle 'tpope/vim-haml' " Vim runtime files for Haml, Sass, and SCSS
NeoBundle 'slim-template/vim-slim'
NeoBundle 'monochromegane/unite-yaml'
NeoBundle 'vim-scripts/nginx.vim'

NeoBundle 'tpope/vim-fugitive' " fugitive.vim: a Git wrapper so awesome, it should be illegal
NeoBundle 'shumphrey/fugitive-gitlab.vim' " An extension to fugitive.vim for gitlab support
NeoBundle 'idanarye/vim-merginal'
NeoBundle 'gregsexton/gitv'
NeoBundle 'int3/vim-extradite'

NeoBundle 'hsanson/vim-android'

call neobundle#end()
NeoBundleCheck

filetype plugin indent on
syntax enable


"----------------------------------------------------------------
"プラグインの設定 オートコマンド・キーマップの設定 (プラグインを使う)"
"----------------------------------------------------------------
"
"----------------------------------------------------------------
"solarized colorscheme 設定(gvimrcにてcolorschemeをmolokai){{{
let g:solarized_termcolors=256
colorscheme solarized
if has('unix')
  colorscheme monokai
endif
"}}}

"----------------------------------------------------------------
" Unite VimFiler Tagbar{{{"

nnoremap [ide] <Nop>
nnoremap [unite] <Nop>
nmap <Space> [ide]
nmap <Space>u [unite]

noremap  <silent>[ide]f :<C-u>VimFilerExplorer<Enter>
noremap  <silent>[ide]t :<C-u>TagbarToggle<Enter>
noremap  <silent>[ide]p :<C-u>Project<Enter>
noremap  <silent>[ide]s :<C-u>VimShellPop<Enter>
noremap  <silent>[ide]q :bd<Enter>
noremap  <silent>[unite]b :<C-u>Unite buffer_tab<Enter>
noremap  <silent>[unite]d :<C-u>Unite -start-insert codic<enter>
noremap  <silent>[unite]g :<C-u>Unite grep<enter>
noremap  <silent>[unite]h :<C-u>Unite file_mru<Enter>
noremap  <silent>[unite]l :<C-u>Unite line<Enter>
noremap  <silent>[unite]ma :<C-u>Unite mark<Enter>
noremap  <silent>[unite]me :<C-u>Unite output:message<Enter>
noremap  <silent>[unite]o :<C-u>Unite outline <Enter>
noremap  <silent>[unite]p :<C-u>Unite history/yank<Enter>
noremap  <silent>[unite]s :<C-u>Unite session<Enter>
noremap  <silent>[unite]t :<C-u>Unite tag<Enter>

noremap  [unite]<Space> :<C-u>Unite output:
noremap  <silent><C-TAB> :<C-u>Unite buffer<Enter>
inoremap <silent><C-TAB> <C-o>:Unite buffer<Enter>

call unite#custom#profile('default', 'context', {
      \ 'prompt': '» '
      \})

" }}}


"----------------------------------------------------------------
" lexima.vim {{{
let g:lexima_no_default_rules = 1
call lexima#set_default_rules()
" }}}

"----------------------------------------------------------------
" textobjs-settings {{{
" let g:textobj_ruby_more_mappings = 1
" }}}
"----------------------------------------------------------------
"html5.vim設定 {{{
let g:html5_event_handler_attributes_complete = 1
let g:html5_rdfa_attributes_complete = 0
let g:html5_microdata_attributes_complete = 0
let g:html5_aria_attributes_complete = 0
" }}}

"----------------------------------------------------------------
"Excite翻訳に飛ばす w3mが必要
"let g:excitetranslate_options = 1 

"----------------------------------------------------------------
" NeoComplCache設定 {{{
if neobundle#is_sourced("neocomplecache")
  let g:acp_enableAtStartup = 0
  let g:neocomplcache_enable_at_startup = 1
  let g:neocomplcache_enable_smart_case = 'infercase'
  let g:neocomplcache_enable_camel_case_completion = 1
  let g:neocomplcache_enable_underbar_completion = 1
  let g:neocomplcache_skip_auto_completion_time = '0.3'
  let g:neocomplcache_max_list = 20
  let g:neocomplcache_enable_auto_close_preview = 1

  inoremap <expr><C-Tab> neocomplcache#start_manual_complete()
  inoremap <expr><C-x><C-f> neocomplcache#manual_omni_complete()
  inoremap <expr><C-x><C-o> neocomplcache#start_manual_complete()
  inoremap <expr><C-n> pumvisible() ? "\<C-n>" : "\<C-x>\<C-u>\<C-p>"
  inoremap <expr><C-p> pumvisible() ? "\<C-p>" : "\<C-p>\<C-n>"
  inoremap <expr><C-g> neocomplcache#undo_completion()
  inoremap <expr><C-l> neocomplcache#complete_common_string()
  inoremap <expr><Left>  neocomplcache#cancel_popup() . "\<Left>"
  inoremap <expr><Right> neocomplcache#cancel_popup() . "\<Right>"
  inoremap <expr><Up>    neocomplcache#cancel_popup() . "\<Up>"
  inoremap <expr><Down>  neocomplcache#cancel_popup() . "\<Down>"
  inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
  inoremap <expr>; neocomplcache#close_popup() . ";"
  inoremap <expr><C-;> neocomplcache#close_popup()

  "g:neocomplcache_same_filetype_lists initialize
  " if !exists('g:neocomplcache_same_filetype_lists')
  "   let g:neocomplcache_same_filetype_lists = {}
  " endif
  " if !exists('g:neocomplcache_include_paths')
  "   let g:neocomplcache_include_paths = {}
  " endif
  " if !exists('g:neocomplcache_include_patterns')
  "   let g:neocomplcache_include_patterns = {}
  " endif
  " if !exists('g:neocomplcache_filename_include_exts')
  "   let g:neocomplcache_filename_include_exts = {}
  " endif
  if !exists('g:neocomplcache_omni_functions')
    let g:neocomplcache_omni_functions = {}
  endif
  if !exists('g:neocomplcache_omni_patterns')
    let g:neocomplcache_omni_patterns = {}
  endif

  let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\h\w*\|\h\w*::'
  let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
  let g:neocomplcache_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
  let g:neocomplcache_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

  let g:neocomplcache#sources#rsense#home_directory = expand('~/.rsense')

  "In javascript buffers, completes from js and html buffers.
  let g:neocomplcache_omni_functions.javascript = 'jscomplete#CompleteJS'

  augroup omnifuncs-dup
    autocmd!
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType css           setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType javascript    setlocal omnifunc=jscomplete#CompleteJS
    autocmd FileType python        setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType xml           setlocal omnifunc=xmlcomplete#CompleteTags
    " autocmd FileType java          set omnifunc=javacomplete#Complete
    " autocmd FileType java          set completefunc=javacomplete#Complete
  augroup END
endif
"}}}

"----------------------------------------------------------------
" NeoComplete設定 {{{
let g:acp_enableAtStartup = 0
let g:neocomplete#enable_auto_select = 0
let g:neocomplete#enable_prefetch = 1
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 'infercase'
let g:neocomplete#enable_camel_case_completion = 1
let g:neocomplete#enable_underbar_completion = 1

let g:neocomplete#sources#syntax#min_keyword_length = 2
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
let g:neocomplete#skip_auto_completion_time = ''
let g:neocomplete#max_list = 80
let g:neocomplete#enable_auto_close_preview = 1
call neocomplete#custom#source('file','rank','10')

inoremap <expr><C-Tab> neocomplete#start_manual_complete()
inoremap <expr><C-x><C-u> neocomplete#start_manual_complete('omni')
" inoremap <expr><C-x><C-o> neocomplete#start_manual_complete('omni')
inoremap <expr><C-x><C-f> neocomplete#start_manual_complete('file')
inoremap <expr><C-x><C-l> neocomplete#start_manual_complete('file/include')
inoremap <expr><C-x><C-s> neocomplete#start_manual_complete('neosnippet')
inoremap <expr><C-x><C-b> neocomplete#start_manual_complete('buffer')
inoremap <expr><C-x><C-w> neocomplete#start_manual_complete('look')
inoremap <expr><C-n> pumvisible() ? "\<C-n>" : "\<C-x>\<C-u>"
inoremap <expr><C-p> pumvisible() ? "\<C-p>" : "\<C-p>\<C-n>"

" <CR>: close popup and save indent.
inoremap <expr><silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return pumvisible() ? neocomplete#close_popup() : "\<CR>"
  " return neocomplete#close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
inoremap <expr><C-g> neocomplete#undo_completion()
inoremap <expr><C-l> neocomplete#complete_common_string()
inoremap <expr><Left>  neocomplete#cancel_popup() . "\<Left>"
inoremap <expr><Right> neocomplete#cancel_popup() . "\<Right>"
inoremap <expr><Up>    neocomplete#cancel_popup() . "\<Up>"
inoremap <expr><Down>  neocomplete#cancel_popup() . "\<Down>"
imap <expr><TAB>  neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_expand_or_jump)"
      \: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB>  neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_expand_or_jump)"
      \: pumvisible() ? "\<C-n>" : "\<TAB>"
" inoremap <expr><BS> pumvisible() ? neocomplete#undo_completion() . "\<BS>" : "\<BS>"
inoremap <expr><C-BS> pumvisible() ? neocomplete#undo_completion() . "\<BS>" : "\<BS>"
inoremap <expr>; neocomplete#close_popup() . ";"
inoremap <expr><C-;> neocomplete#close_popup()

let g:neocomplete#sources = {
      \ '_':['file', 'neosnippet', 'look', 'vim', 'omni', 'include', 'buffer', 'file/include'],
      \}
" Search from neocomplete, omni candidates, vim keywords.
" let g:neocomplete#fallback_mappings = ['\<C-x>\<C-o>', '\<C-x>\<C-n>'] 
"
"g:neocomplete#same_filetype_lists initialize
" if !exists('g:neocomplete#same_filetypes')
"   let g:neocomplete#same_filetypes = {}
" endif

" if !exists('g:neocomplete#include_paths')
"   let g:neocomplete#include_paths = {}
" endif
" if !exists('g:neocomplete#include_patterns')
"   let g:neocomplete#include_patterns = {}
" endif
" if !exists('g:neocomplete#filename_include_exts')
"   let g:neocomplete#filename_include_exts = {}
" endif
" if !exists('g:neocomplete#tags_filter_patterns')
"   let g:neocomplete#tags_filter_patterns = {}
" endif
" let g:neocomplete#tags_filter_patterns.c = ['vim', 'git']
" let g:neocomplete#tags_filter_patterns.cpp = ['vim', 'git']
" let g:neocomplete#tags_filter_patterns.ruby = ['vim', 'git']

if !exists('g:neocomplete#omni_functions')
  let g:neocomplete#sources#omni#functions = {}
endif
if !exists('g:neocomplete#omni_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif

let g:neocomplete#force_omni_input_patterns.c      = '[^.[:digit:] *\t]\%(\.\|->\)\%(\h\w*\)\?'
let g:neocomplete#sources#omni#input_patterns.php  = '\h\w*\|[^. \t]->\%(\h\w*\)\?\|\h\w*::\%(\h\w*\)\?'
let g:neocomplete#force_omni_input_patterns.ruby   = '[^. *\t]\.\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.ruby = '[^. *\t]\.\h\w*\|\h\w*::'
let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
" let g:neocomplete#force_omni_input_patterns.c      = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.cpp    = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#sources#snowdrop#enable = 1

"In javascript buffers, completes from js and html buffers.
let g:neocomplete#sources#omni#functions.javascript = 'jscomplete#CompleteJS'

augroup omnifuncs
  autocmd!
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType css           setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType javascript    setlocal omnifunc=jscomplete#CompleteJS
  autocmd FileType python        setlocal omnifunc=jedi#completions
  autocmd FileType xml           setlocal omnifunc=xmlcomplete#CompleteTags
  " autocmd FileType ruby          setlocal omnifunc=rubycomplete#Complete
  " autocmd FileType java          set omnifunc=javacomplete#Complete
  " autocmd FileType java          set completefunc=javacomplete#Complete
augroup END
"}}}

"----------------------------------------------------------------
" quickrun設定 important unite_quickfix,shabadoubi{{{
  "実行中のquickrunセッションの終了
nnoremap <expr><silent><C-c> quickrun#is_running() ? quickrun#sweep_sessions() : "\<C-c>"
let g:quickrun_config = {
      \ "_" : {
      \   'hook/close_unite_quickfix/enable_hook_loaded' : 1,
      \   'hook/unite_quickfix/enable_failure' : 1,
      \   'hook/close_quickfix/enable_exit' : 1,
      \   'hook/close_buffer/enable_failure' : 1,
      \   'hook/close_buffer/enable_empty_data' : 1,
      \   'hook/echo/enable' : 1,
      \   'hook/echo/output_success' : 'success',
      \   'hook/echo/output_failure' : 'failure',
      \   'runner' : 'vimproc',
      \   'runner/vimproc/updatetime' : 40,
      \   'outputter' : 'multi:buffer:quickfix',
      \ },
      \ "java" : {
      \   'exec' : ['javac %o %s:p', '%c %s:t:r %a'],
      \   'hook/sweep/files' : '%S:p:r.class', 
      \   'tempfile' : '%{expand("%")}',
      \ },
      \ "html" : {
      \   'outputter' : 'browser',
      \ },
      \ "markdown" : {
      \   'type': 'markdown/gfm',
      \   'outputter' : 'browser',
      \ },
      \ "tex" : {
      \   'type': 'tex/latexmk',
      \ },
      \ "tex/platex" : {
      \   'type': 'tex/platex',
      \   'command' : 'platex',
      \   'exec' : ['%c %s:p', 'dvipdfmx %s:p:r.dvi'],
      \   'outputter' : 'quickfix',
      \ },
      \ "tex/pdflatex" : {
      \   'type': 'tex/pdflatex',
      \   'command' : 'pdflatex',
      \   'exec' : ['%c %s'],
      \   'outputter' : 'quickfix',
      \ },
      \ "tex/latexmk" : {
      \   'type': 'tex/latexmk',
      \   'command' : 'latexmk',
      \   'exec' : ['%c %o %s'],
      \   'cmdopt': '-pdfdvi',
      \   'outputter' : 'quickfix',
      \ },
      \ "c" : {
      \   'type' : 'c/gstreamer',
      \ },
      \ "c/bluetooth" : {
      \   'command' : 'gcc',
      \   'exec' : ['%c %s %o'],
      \   'cmdopt' : "-Wall -lbluetooth -pthread",
      \ },
      \ "c/opencv" : {
      \   'command' : 'gcc',
      \   'exec' : ['%c %s %o'],
      \   'cmdopt' : "-Wall `pkg-config --cflags --libs opencv`",
      \ },
      \ "c/gstreamer" : {
      \   'command' : 'gcc',
      \   'exec' : ['%c %s %o'],
      \   'cmdopt' : "-Wall `pkg-config --cflags --libs gstreamer-0.10`",
      \ },
      \ "cpp/opencv" : {
      \   'command' : 'g++',
      \   'exec' : ['%c %s %o'],
      \   'cmdopt' : "-Wall `pkg-config --cflags --libs opencv`",
      \ },
      \ "arduino" : {
      \   'type' : 'arduino/ino',
      \ },
      \ "arduino/ino" : {
      \   'command' : 'ino',
      \   'exec' : '%c build %s',
      \ },
      \ "ruby/testunit" : {
      \ },
      \ "ruby/rspec" : {
      \   'command' : 'rspec',
      \   'cmdopt' : '--format progress -I .',
      \   'filetype' : 'result.rspec',
      \ },
      \}

nmap <Leader><Space>r :<C-u>QuickRun 
" }}}
 
"----------------------------------------------------------------
" watchdogs {{{
let g:watchdogs_check_BufWritePost_enables = {
      \ 'c' : 1,
      \ 'cpp' : 1,
      \ 'coffee' : 1,
      \ 'javascript' : 1,
      \ 'php' : 1,
      \ 'ruby' : 1,
      \ 'python' : 1,
      \ 'scss' : 1,
      \}
let g:quickrun_config["watchdogs_checker/_"] = {
      \   "hook/close_quickfix/enable_exit" : 1,
      \ }
call watchdogs#setup(g:quickrun_config)
"}}}

"----------------------------------------------------------------
" Simple-Javascript-Indenter設定{{{
let g:SimpleJsIndenter_CaseIndentLevel = -1
"}}}
"----------------------------------------------------------------
" jedi-vim設定
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
let g:jedi#rename_command = ""
"}}}
"----------------------------------------------------------------
" android設定
if has('mac')
  let g:android_sdk_path = 'FIXME'
elseif has('unix')
  let g:android_sdk_path = '~/android-sdk-linux'
endif
"----------------------------------------------------------------
" vim-ruby設定
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_classes_in_global = 1
augroup rubySettings
  autocmd!
  autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
  autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
  autocmd User Rails let g:rubycomplete_rails = 1
augroup END
"----------------------------------------------------------------
" rsense設定　{{{
" let g:neocomplete#sources#rsense#home_directory = expand('~/rsense')
" }}}
"----------------------------------------------------------------
" monster設定 {{{
" let g:monster#completion#rcodetools#backend = 'async_rct_complete'
" }}}

"----------------------------------------------------------------
" ruby_hl_lvar設定 {{{
let g:ruby_hl_lvar_auto_enable = 1
" }}}
"
"----------------------------------------------------------------
" quickhl設定 {{{
nmap mm <Plug>(quickhl-manual-this)
xmap mm <Plug>(quickhl-manual-this)
nmap MM <Plug>(quickhl-manual-reset)
xmap MM <Plug>(quickhl-manual-reset)
" }}}
"----------------------------------------------------------------
" marching, snowdrop設定 {{{

if has('mac')
  let g:marching_clang_command = '/usr/bin/clang'
  let s:include_path = filter(
        \ split(glob('/Library/Frameworks/*/Headers/*'), '\n') +
        \ split(glob('/usr/include/c++/*'), '\n') +
        \ split(glob('/usr/include/*/c++/*'), '\n') +
        \ split(glob('/usr/include/*'), '\n'),
        \ 'isdirectory(v:val)')
  let g:snowdrop#libclang_directory = '/Library/Developer/CommandLineTools/usr/lib/'
  let g:snowdrop#libclang_file = 'libclang.dylib'
elseif has('unix')
  let g:marching_clang_command = '/usr/bin/clang'
  let s:include_path = filter(
        \ split(glob('/usr/include/c++/*'), '\n') +
        \ split(glob('/usr/include/*/c++/*'), '\n') +
        \ split(glob('/usr/include/*'), '\n'), 
        \ 'isdirectory(v:val)')
  let g:snowdrop#libclang_directory = '/usr/lib/llvm-3.4/lib' "ubuntu
  let g:snowdrop#libclang_file = 'libclang.so' "ubuntu
endif

let g:marching_backend = "clang_command"
let g:marching#clang_command#options = {
      \ 'c'   : '',
      \ 'cpp' : '-std=c++1y',
      \ }
let g:marching_include_paths = s:include_path
let g:marching_enable_neocomplete = 1

let g:snowdrop#libclang#default_binding = "python"
let g:snowdrop#command_options = {
      \ 'c'   : '',
      \ 'cpp' : '-std=c++1y',
      \ }
let g:snowdrop#include_paths = {
        \ 'cpp' : s:include_path,
        \ }
unlet s:include_path
" }}}
"----------------------------------------------------------------
" jscomplete-vim設定{{{
let g:jscomplete_use = ['dom', 'moz', 'es6th']
" }}}

"----------------------------------------------------------------
" neosnippet設定{{{
let g:neosnippet#snippets_directory = "~/.vim/vimnewfiles/snippet"
imap <C-s> <Plug>(neosnippet_expand_or_jump)
" imap <C-s> <Plug>(neosnippet_expand_or_jump)
"}}}

"----------------------------------------------------------------
" リンクを開くブラウザ指定
if has('unix')
  let openuri_cmd = "call system('firefox %s &')"
else
  " let openuri_cmd = '!start "rundll32.exe" url.dll,FileProtocolHandler %s'
  " Internet explorer
  " let openuri_cmd = '!start "C:/Program Files/Internet Explorer/iexplore.exe" %s'
  " firefox
  let openuri_cmd = '!start "C:/Program Files/Mozilla Firefox/firefox.exe" %s'
endif


"----------------------------------------------------------------
" VimFiler設定 {{{
" vimfilerをデフォルトに設定
let g:vimfiler_as_default_explorer = 1
" }}}

"----------------------------------------------------------------
" emmet設定 {{{
let s:user_emmet_settings_jquery_version = "1.10.2"
let g:user_emmet_settings = {
      \
      \  'lang' : 'ja',
      \  'html' : {
      \    'filters' : 'html',
      \    'indentation' : '  ',
      \    'snippets' : {
      \      'jquery' : '<script src="//ajax.googleapis.com/ajax/libs/jquery/' . s:user_emmet_settings_jquery_version . '/jquery.min.js"></script>${cursor}',
      \      'jqueryui' : '<script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js"></script>${cursor}',
      \    },
      \  },
      \  'php' : {
      \    'extends' : 'html',
      \    'filters' : 'html,c',
      \  },
      \  'css' : {
      \    'filters' : 'fc',
      \  },
      \  'custom_expands1' : {
      \    '^\%(lorem\|lipsum\)\(\d*\)$' : function('zencoding#lorem#ja#expand'),
      \  },
      \}
" }}}

"----------------------------------------------------------------
" project.vim {{{
" ファイルが選択されたら、ウインドウを閉じる
let g:proj_flags = "imstc"
" <Leader>Pで、プロジェクトをトグルで開閉する
nmap <silent><Leader>P <Plug>ToggleProject
" <Leader>pで、デフォルトのプロジェクトを開く
nmap <silent><Leader>p :Project<Enter>
" $HOME/.vim/after/plugin/project.vim
if getcwd() != $HOME
  if filereadable(getcwd(). '/.vimprojects')
    Project .vimprojects
  endif
endif
" プロジェクトを開いたときに、フォールディングを展開した状態にする
au BufAdd .vimprojects silent! %foldopen!
" }}}

"----------------------------------------------------------------
" vim-indent-guides設定 {{{
if neobundle#is_sourced("vim-indent-guideline")
  let g:indent_guides_guide_size = 2
  let g:indent_guides_enable_on_vim_startup = 1
endif
" }}}

"----------------------------------------------------------------
" indentLine設定 {{{
if neobundle#is_sourced("indenLine")
  let g:intentLine_char = "︙"
endif
" }}}

"----------------------------------------------------------------
" errormarker設定 {{{
let g:errormarker_errortext = '!!'
let g:errormarker_warningtext = '??'
let g:errormarker_errorgroup = 'Error'
let g:errormarker_warninggroup = 'Todo'
if has('win32') || ('win64')
  let g:errormarker_erroricon = expand('$VIM/signs/err.bmp')
  let g:errormarker_warningicon = expand('$VIM/signs/warn.bmp')
else
  let g:errormarker_erroricon = expand('$VIM/signs/err.png')
  let g:errormarker_warningicon = expand('$VIM/signs/warn.png')
endif
" }}}

"----------------------------------------------------------------
" ShowMarks設定 {{{
let g:showmarks_include="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
" }}}

"----------------------------------------------------------------
" VimShell設定 {{{
map <Leader>sh <Plug>(vimshell_split_switch)
let g:vimshell_scrollback_limit = 5000
let g:vimshell_user_prompt = 'getcwd()'
let g:vimshell_enable_smart_case = 1
if has('win32') || has('win64')
  "Display user name on Windows.
  let g:vimshell_prompt = $USERNAME. "% "
else
  "Display user name on Linux.
  let g:vimshell_prompt = $USER . "% "
endif
" Initialize execute file list
let g:vimshell_execute_file_list = {}
call vimshell#set_execute_file('txt,vim,c,h,cpp,d,java,css,scss,js,tex', 'vim') "open in vim
call vimshell#set_execute_file('html,xhtml', 'gexe firefox')
call vimshell#set_execute_file('pdf', 'gexe evince')

let g:vimshell_execute_file_list['rb'] = 'ruby'
let g:vimshell_execute_file_list['pl'] = 'perl'
let g:vimshell_execute_file_list['py'] = 'python'
let g:vimshell_execute_file_list['html'] = 'firefox'
" }}}

"----------------------------------------------------------------
" QfixHowm/qfixmemo設定 {{{
let QfixHowm_key = 'g'
let howm_dir = g:DropBox_dir . '/editedByVim/howm'
let hown_filename = '%Y/%m/%Y-%m-%d-%H%M%S.howm.txt'
let howm_fileencoding = 'utf-8'
let howm_fileformat = 'unix'
let QFixHowm_FileType = 'qfix_memo'
let qfixmemo_filetype = 'qfix_memo'
let qfixmemo_pairfile_dir = 'pairfile'
let calendar_holidayfile = howm_dir . '/Sche-Hd-0000-00-00-000000.cp932'
call openuri#init()
let QfixMRU_RootDir = '~/.vim/vimnewfiles/'
let QfixMRU_Filename = '~/.vim/vimnewfiles/.qfixmru'
let QfixMRU_Title = {}
" }}}

"----------------------------------------------------------------
" Easymotion設定{{{
let g:EasyMotion_hl_group_target = 1
let g:EasyMotion_grouping = 1
let g:EasyMotion_do_shade = 1
let g:EasyMotion_keys ="asdfghjkl;'qwertyuiop[]"
let g:EasyMotion_hl_group_shade = 'Comment'
let g:EasyMotion_hl_group_target = 'Search'
" }}}

"----------------------------------------------------------------
" ref設定 ref.vim ref-sources.vim {{{
" python
let g:ref_pydoc_cmd = "pydoc"
let g:ref_pydoc_complete_head = 0
" java
let g:ref_javadoc_path = ''
let g:ref_use_cache = 1
" Javascript
let g:ref_javascript_doc_path = ''
" jquery
let g:ref_jquery_doc_path = ''
" }}}

"----------------------------------------------------------------
"eskk設定 {{{
let g:eskk#directory = '~/.eskk'
let g:eskk#dictionary = {
      \     'path' : "~/.skk-jisyo",
      \     'sorted' : 0,
      \     'encoding' : 'utf-8',
      \}
let g:eskk#large_dictionary = {
      \     'path' : "/usr/share/skk/SKK-JISYO.L",
      \     'sorted' : 1,
      \     'encoding' : 'euc-jp',
      \}
let g:eskk#enable_completion = 1
"}}}

"----------------------------------------------------------------
"unite-ruby-require.vim設定 {{{
let g:unite_source_ruby_require_ruby_command = '$HOME/.rbenv/shims/ruby'
"}}}
"----------------------------------------------------------------
" vim-rails {{{
"有効化
let g:rails_default_file='config/database.yml'
let g:rails_level = 4
let g:rails_mappings=1
let g:rails_modelines=0
" let g:rails_some_option = 1
let g:rails_statusline = 1
" let g:rails_subversion=0
let g:rails_syntax = 1
" let g:rails_url='http://localhost:3000'
" let g:rails_ctags_arguments='--languages=-javascript'
" let g:rails_ctags_arguments = ''
function! SetUpRailsSetting()
  nnoremap <buffer><[rails] <Nop>
  nmap     <buffer><Space>r [rails]
  nnoremap <buffer>[rails]r :R<CR>
  nnoremap <buffer>[rails]a :A<CR>
  nnoremap <buffer>[rails]m :Rmodel<Space>
  nnoremap <buffer>[rails]c :Rcontroller<Space>
  nnoremap <buffer>[rails]v :Rview<Space>
  nnoremap <buffer>[rails]p :Rpreview<CR>
endfunction

aug MyAutoCmd
  au User Rails call SetUpRailsSetting()
aug END

aug RailsDictSetting
  au!
aug END
"}}}
"----------------------------------------------------------------
" Unite-rails.vim {{{
function! UniteRailsSetting()
  nnoremap <buffer>[uniterails] <Nop>
  nmap     <buffer><Space>ur [uniterails]
  noremap <buffer>[uniterails]v :<C-U>Unite rails/view<CR>
  noremap <buffer>[uniterails]m :<C-U>Unite rails/model<CR>
  noremap <buffer>[uniterails]c :<C-U>Unite rails/controller<CR>

  noremap <buffer>[uniterails]<Space> :<C-U>Unite rails/

  noremap <buffer>[uniterails]oc :<C-U>Unite rails/config<CR>
  noremap <buffer>[uniterails]os :<C-U>Unite rails/spec<CR>
  noremap <buffer>[uniterails]om :<C-U>Unite rails/db -input=migrate<CR>
  noremap <buffer>[uniterails]ol :<C-U>Unite rails/lib<CR>
  noremap <buffer><expr>[uniterails]og ':e '.b:rails_root.'/Gemfile<CR>'
  noremap <buffer><expr>[uniterails]or ':e '.b:rails_root.'/config/routes.rb<CR>'
  noremap <buffer><expr>[uniterails]ose ':e '.b:rails_root.'/db/seeds.rb<CR>'
  noremap <buffer>[uniterails]ora :<C-U>Unite rails/rake<CR>
  noremap <buffer>[uniterails]oh :<C-U>Unite rails/heroku<CR>
endfunction
aug MyAutoCmd
  au User Rails call UniteRailsSetting()
aug END
"}}}

"----------------------------------------------------------------
" "Powerline {{{
" "temp powerline
" " if has('unix')
" " autocmd WinEnter * set guifont=Ricty\ for\ Powerline\ 12
" " endif
" set laststatus=2
" "}}}

"----------------------------------------------------------------
"airline {{{
let g:airline_detect_modified=1
let g:ariline_detect_paste=1
" let g:airline_enable_branch=1
let g:airline_theme='wombat'
let g:airline_powerline_fonts=1

" let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#bufferline#anabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#branch#empty_message = '(no branch)'
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#quickfix#quickfix_text = 'Quickfix'
let g:airline#extensions#quickfix#location_text = 'Location'

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#tab_nr_type = 1
let g:airline#extensions#tabline#show_tab_nr = 1

let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
"}}}

"----------------------------------------------------------------
" anzu {{{
nmap n <Plug>(anzu-n)
nmap N <Plug>(anzu-N)
nmap * <Plug>(anzu-star)
nmap # <Plug>(anzu-sharp)
" }}}

"----------------------------------------------------------------
" over {{{
let g:over#command_line#search#enable_move_curser = 1
let g:over#command_line#paste_escape_chars = '/.*$^~'
let g:over#command_line#paste_filters = {
      \ "\n" : '\\n',
      \ "\r" : '\\r',
      \}
" }}}

"----------------------------------------------------------------
" incsearch {{{
map / <Plug>(incsearch-forward)
map ? <Plug>(incsearch-backward)
map g/ <plug>(incsearch-stay)
" }}}

"----------------------------------------------------------------
" operator-raplace {{{
nmap  _ <Plug>(operator-replace)
" }}}

"----------------------------------------------------------------
" Tagbar {{{
let g:tagbar_show_linenumbers = -1
let g:tagbar_iconchars = ['▸', '▾']
let g:tagbar_type_javascript = {
      \ 'ctagsbin' : '/home/kaki/.nodebrew/current/bin/'
      \}
let g:tagbar_phpctags_bin = '/home/kaki/phpctags/phpctags'
let g:tagbar_phpctags_memory_limit = '256M'
let g:tagbar_type_tex = {
      \ 'ctagstype' : 'latex',
      \ 'kinds' : [
      \   's:sections',
      \   'g:graphics:0:0',
      \   'l:labels',
      \   'r:refs:1:0',
      \   'p:pagerefs:1:0'
      \ ],
      \ 'sort' : 0,
      \ 'deffile' : expand('<sfile>:p:h:h') . '/ctags/latex.cnf'
      \}
let g:tagbar_type_r = {
      \ 'ctagstype' : 'r',
      \ 'kinds' : [
      \   'f:Functions',
      \   'g:GlobalVariables',
      \   'v:FunctionVariables',
      \ ]
      \ }
let g:tagbar_type_ruby = {
      \ 'kinds' : [
      \   'm:modules',
      \   'c:classes',
      \   'd:describes',
      \   'C:contexts',
      \   'f:methods',
      \   'F:singleton methods'
      \ ]
      \}
let g:tagbar_type_cpp = {
      \ 'ctagstype' : 'c++',
      \ 'kinds' : [
      \   'd:macros:1',
      \   'p:prototypes:1',
      \   'g:enums',
      \   'e:enumerators',
      \   't:tupedefs',
      \   'n:namespaces',
      \   'c:classes',
      \   's:structs',
      \   'u:unions',
      \   'f:functions',
      \   'm:menbers',
      \   'v:variables'
      \ ],
      \ 'sro' : '::',
      \ 'kind2scope' : {
      \   'g' : 'enum',
      \   'n' : 'namespace',
      \   'c' : 'class',
      \   's' : 'struct',
      \   'u' : 'union'
      \ },
      \ 'scope2kind' : {
      \   'enum' : 'g',
      \   'namespace' : 'n',
      \   'class' : 'c',
      \   'struct' : 's',
      \   'union' : 'u'
      \ }
      \}
if executable('marktag') "gem install marktag
  let g:tagbar_type_markdown = {
        \ 'ctagstype' : 'markdown',
        \ 'ctagsbin' : 'marktag',
        \ 'kinds' : [
        \   'h:header'
        \ ],
        \ 'sro' : '.',
        \ 'kind2scope' : {
        \   'h' : 'header'
        \ },
        \ 'scope2kind' : {
        \   'header' : 'h'
        \ }
        \}
endif
" }}}

"----------------------------------------------------------------
" YankRound {{{
let g:yankround_max_history = 80
let g:yankround_dir = $HOME . '/.cache/yankround'
nmap p <Plug>(yankround-p)
nmap P <Plug>(yankround-P)
nmap <C-p> <Plug>(yankround-prev)
nmap <C-n> <Plug>(yankround-next)
" }}}

"---------------------------------------------------------------
" Gista {{{
let g:gista#github_user = 'norikakip'
" }}}

"---------------------------------------------------------------
" codic {{{
nmap <silent><space>d :Codic<Enter>
" }}}

"---------------------------------------------------------------
"オートコマンドの設定 
"---------------------------------------------------------------
augroup BufInit
  autocmd!
  autocmd GUIEnter * cd ~
  autocmd BufNewFile,BufRead <buffer> set iminsert=0
  autocmd BufNewFile,BufRead <buffer> let $PATH = expand("%:p:h") .":". $PATH
augroup END

augroup vimrc-checktime
  autocmd!
  autocmd WinEnter * checktime
augroup END

"GUIウィンドウを最大化して起動
"au GUIEnter * simalt ~x
"
"GUIで入力モード時、ステータスラインのカラーを変更
"augroup InsertHook
"autocmd InsertEnter * highlight StatusLine guifg=#ccdc90 guibg=#2E4340
"autocmd InsertLeave * highlight StatusLine guifg=#2E4340 guibg=#ccdc90
"augroup END
augroup filetype-settings
  autocmd FileType text call s:text_filetype_settings()
  function! s:text_filetype_settings()
    setlocal iminsert=2 shiftround
  endfunction

  autocmd BufWinEnter,BufNewFile *.h set ft=c
  autocmd FileType c call s:c_filetype_settings()
  function! s:c_filetype_settings()
    setlocal tabstop=4 shiftwidth=4
    noremap <buffer> <silent>[unite]i :Unite snowdrop/includes<Enter>
    noremap <buffer> <silent>[unite]o :Unite snowdrop/outline<Enter>
    noremap <buffer> <silent>[ide]v :Unite Ref/man<Enter>
  endfunction

  autocmd FileType cpp call s:cpp_filetype_settings()
  " let $CPP_STDLIB = ''
  " autocmd BufReadPost $CPP_STDLIB/* if empty(&filetype) | set filetype=cpp | endif
  function! s:cpp_filetype_settings()
    setlocal tabstop=4 shiftwidth=4
    setlocal matchpairs+=<:>
    setlocal balloonexpr=snowdrop#ballonexpr_typeof()
    setlocal balloondelay=15
    setlocal ballooneval
    noremap <buffer> <silent>[unite]i :Unite snowdrop/include<Enter>
    noremap <buffer> <silent>[unite]o :Unite snowdrop/outline<Enter>
    noremap <buffer> <silent>[ide]v :Unite Ref/man<Enter>
  endfunction

  autocmd FileType java call s:java_filetype_settings()
  function! s:java_filetype_settings()
    setlocal tabstop=4 shiftwidth=4 shiftround
    setlocal includeexpr=substitute(v:fname,'\\.','/','g')
  endfunction

  autocmd FileType javascript call s:javascript_filetype_settings()
  function! s:javascript_filetype_settings()
    setlocal tabstop=4 shiftwidth=4 shiftround
  endfunction

  autocmd FileType ruby call s:ruby_filetype_settings()
  function! s:ruby_filetype_settings()
    setlocal tabstop=2 shiftround shiftwidth=2
    noremap <buffer> <silent>[unite]i :Unite ruby/require<Enter>
    noremap <buffer> <silent><Leader>t :QuickRun
    noremap <buffer> <silent>[ide]v :Unite -start-insert -default-action=split ref/ri<Enter>
    noremap <buffer> <silent>[ide]p :VimShellInteractive pry<Enter>
  endfunction

  autocmd BufWinEnter,BufNewFile *_spec.rb set filetype=ruby.rspec
  autocmd FileType ruby.rspec call s:ruby_rspec_filetype_settings()
  function! s:ruby_rspec_filetype_settings()
    setlocal tabstop=2 shiftwidth=2
  endfunction
  autocmd Filetype result.rspec call s:ruby_rspec_result_filetype_settings()
  function! s:ruby_rspec_result_filetype_settings()
    syntax case match
    syntax match RSpecGreen /^\.*$/
    syntax match RSpecRed   /^[F.]*F[F.]*$/
    syntax match RSpecGreen /^.* 0 failure.*$/
    syntax match RSpecRed   /^.* [1-9][0-9]* failure.*$/
    highlight RSpecGreen ctermfg=White ctermbg=Green guifg=White guibg=Green
    highlight RSpecRed   ctermfg=White ctermbg=Red   guifg=White guibg=Red
    noremap <buffer> q :q<Enter>
  endfunction

  autocmd FileType python call s:python_filetype_settings()
  function! s:python_filetype_settings()
    setlocal tabstop=4 shiftwidth=4
    nnoremap <buffer> <silent>[ide]v :Ref pydoc 
  endfunction

  autocmd FileType php call s:php_filetype_settings()
  function! s:php_filetype_settings()
    setlocal tabstop=4
    setlocal shiftwidth=4
  endfunction

  autocmd FileType htaccess call s:htaccess_filetype_settings()
  function! s:htaccess_filetype_settings()
    setlocal tabstop=4
    setlocal shiftwidth=4
  endfunction

  autocmd FileType gitcommit call s:gitcommit_filetype_settings()
  function! s:gitcommit_filetype_settings()
    setlocal spell spelllang=en_us
  endfunction

  function! s:lisp_filetype_settings()
    let g:lisp_instring = 1
    let g:lisp_rainbow = 1
  endfunction

  autocmd FileType help call s:help_filetype_settings()
  function! s:help_filetype_settings()
    nnoremap <buffer> q :q<Enter>
  endfunction

  autocmd BufWinEnter,BufNewFile *.epub set filetype=epub
  autocmd BufReadCmd *.epub call zip#Browse(expand("<amatch>"))
  autocmd FileType epub call s:epub_filetype_settings()
  function! s:epub_filetype_settings()
    nnoremap <buffer> q :q<Enter>
  endfunction

augroup END

command! CDCurrent call s:CDCurrent()
function! s:CDCurrent()
  if isdirectory(expand('%:h'))
    cd %:h
  endif
endfunction

command! CDDropbox call s:CDDropbox()
function! s:CDDropbox()
  if g:DropBox_dir != ""
    execute "cd ".g:DropBox_dir
  endif
endfunction

command! LinuxKernelReading call s:LinuxKernelReading()
function! s:LinuxKernelReading()
  let s:kernel_source_directory = "$HOME/src/linux-3.15.7"
  tabnew "LinuxKernel"
  execute "lcd ".s:kernel_source_directory
  execute "VimFilerSimple -auto-cd -explorer -winwidth=40 -focus -status -no-quit"
  let &path = 
        \ join(
        \ filter(split(glob(s:kernel_source_directory."/*/*/*"), '\n'), 'isdirectory(v:val)'),
        \ ',')
        \ . &path 
endfunction

command! Gentags call s:Gentags()
function! s:Gentags()
  execute "!ctags -R"
endfunction

" TODO:implements
function! s:Notification(str)
  let s:notification_icon_path = expand('~/src/vim/src/vim.ico')
endfunction

command! VimrcEdit call s:VimrcEdit()
function! s:VimrcEdit()
  edit $MYVIMRC
endfunction

