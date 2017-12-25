set encoding=utf-8
scriptencoding

set fileformats=unix,dos
" バックアップファイルを作るディレクトリ
set backupdir=~/.vim/vimbackup
" undofile dir
set undodir=~/.vim/vimbackup
set undofile
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
set relativenumber
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
" 段数以上のフォールディングは展開
set foldlevel=6

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
inoremap <C-Enter> <C-o>O
nnoremap x "_x
nnoremap <silent><C-l> :tabnext<Enter>
nnoremap <silent><C-h> :tabprevious<Enter>
nnoremap <silent><C-w><C-w> :tabnext<enter>
inoremap <silent><C-l> <C-o>:tabnext<Enter>
inoremap <silent><C-h> <C-o>:tabprevious<Enter>

cnoremap <C-a> <HOME>
cnoremap <C-e> <END>

"----------------------------------------------------------------
" Plugins"
"----------------------------------------------------------------
filetype off
set runtimepath+=~/.vim/dein/repos/github.com/Shougo/dein.vim
call dein#begin(expand('~/.vim/dein'))

" util
if !has('Kaoriya')
  call dein#add('Shougo/vimproc', {'build': 'make'})
else
  call dein#add('Shougo/vimproc')
endif
call dein#add('Shougo/neossh.vim') " SSH interface for Vim plugins
call dein#add('Shougo/unite.vim')
call dein#add('Shougo/tabpagebuffer.vim')
call dein#add('Shougo/neomru.vim')
call dein#add('Shougo/vimfiler')
call dein#add('tpope/vim-surround')
call dein#add('tpope/vim-repeat')
call dein#add('sjl/gundo.vim') " A git mirror of gundo.vim
call dein#add('h1mesuke/vim-alignta') "テキスト整形
call dein#add('kana/vim-textobj-user') "テキストオブジェクト拡張
call dein#add('kana/vim-textobj-indent') "テキストオブジェクト拡張
" call dein#add('kana/vim-textobj-jabraces') 
call dein#add('koron/minimap-vim')
call dein#add('vim-jp/vimdoc-ja') " A project which translate Vim documents into Japanese.
call dein#add('vim-scripts/autodate.vim')
call dein#add('tyru/eskk.vim', {'on_i': 1})
call dein#add('fuenor/qfixhowm')
call dein#add('mattn/webapi-vim')
call dein#add('bling/vim-airline') " lean & mean statusline for vim that')s light as air
call dein#add('tsukkee/unite-help') " help source for unite.vim
call dein#add('thinca/vim-unite-history') " A source of unite.vim for history of command/search.

"colorscheme
call dein#add('tomasr/molokai') "colorscheme
call dein#add('sickill/vim-monokai')
call dein#add('altercation/vim-colors-solarized') " precision colorscheme for the vim text editor
call dein#add('chriskempson/vim-tomorrow-theme')
call dein#add('w0ng/vim-hybrid')
call dein#add('morhetz/gruvbox')
call dein#add('jpo/vim-railscasts-theme')
call dein#add('vim-scripts/Wombat')
call dein#add('vim-scripts/wombat256.vim')
call dein#add('vim-scripts/ecostation')

"cording plugin
call dein#add('Shougo/neocomplete', {'if': has('lua') && (version >= 704)})
call dein#add('Shougo/neoinclude.vim')
call dein#add('Shougo/neosnippet')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('thinca/vim-quickrun')
call dein#add('osyo-manga/unite-quickfix')
call dein#add('osyo-manga/shabadou.vim')
call dein#add('nathanaelkane/vim-indent-guides') "インデントにバーを表示
call dein#add('tpope/vim-abolish') "変数を命名規則に従って変換
call dein#add('tomtom/tcomment_vim') "コメントアウト機能
call dein#add('thinca/vim-template')
call dein#add('tpope/vim-endwise')
call dein#add('pekepeke/ref-javadoc') " javadoc plugin for vim-ref
call dein#add('mojako/ref-sources.vim') " vim-ref 用の追加ソース (javascript, jquery, kotobank, wikipedia)
call dein#add('thinca/vim-ref') "リファレンスビューア
call dein#add('vim-scripts/errormarker.vim')

call dein#add('mattn/emmet-vim')
call dein#add('othree/html5.vim')
call dein#add('godlygeek/tabular')
call dein#add('joker1007/vim-markdown-quote-syntax')
call dein#add('rcmdnk/vim-markdown')
call dein#add('hail2u/vim-css3-syntax')
call dein#add('isRuslan/vim-es6')

call dein#add('tpope/vim-rails')
call dein#add('tpope/vim-rake')
call dein#add('tpope/vim-bundler', {'build': 'gem install bundler gem-browse gem-ctags'})
call dein#add('basyura/unite-rails') " a unite.vim plugin for rails
call dein#add('ujihisa/unite-rake') " A Unite.vim plugin to run tasks or to view descriptions easily, using rake command
call dein#add('rhysd/unite-ruby-require.vim')
call dein#add('rhysd/neco-ruby-keyword-args')
call dein#add('rhysd/vim-textobj-ruby')

call dein#add('Shougo/unite-outline')
call dein#add('tyru/open-browser.vim')
call dein#add('vim-scripts/neco-look') " 1.0   A neocomplcache plugin for `/usr/bin/look` for completing words in English.
call dein#add('vim-scripts/javacomplete', {
      \ 'if': executable('javac'),
      \ 'build': 'javac autoload/Reflection.java',
      \})
call dein#add('kamichidu/vim-javaclasspath', {'on_ft': 'java'})
call dein#add('kamichidu/vim-unite-javaimport' , {
      \ 'depends': [
      \     'unite.vim',
      \     'vimproc',
      \     'vim-javaclasspath',
      \     'w3m.vim',
      \  ],
      \ 'on_ft': 'java'
      \})
call dein#add('Shougo/unite-build') " Build by unite interface
call dein#add('osyo-manga/unite-qfixhowm')
call dein#add('ujihisa/unite-colorscheme') " A unite.vim plugin
call dein#add('ujihisa/unite-gem') " A Unite plugin for RubyGems
call dein#add('tsukkee/unite-tag') 
call dein#add('osyo-manga/unite-quickrun_config')
call dein#add('rhysd/quickrun-unite-quickfix-outputter') " An outputter of QuickRun for unite-quickfix.
call dein#add('cakebaker/scss-syntax.vim') " Vim syntax file for scss (Sassy CSS)
call dein#add('vim-scripts/VimClojure') " A filetype, syntax and indent plugin for Clojure
call dein#add('ujihisa/unite-locate')

" Vim上でGaucheを書くお手伝いをするスクリプト
call dein#add('aharisu/vim-gdev', {'if': executable('gosh')})
call dein#add('Shougo/unite-session') " unite.vim session source
call dein#add('Shougo/unite-sudo') " sudo source for unite.vim
call dein#add('osyo-manga/vim-anzu') " Vim search status.
call dein#add('osyo-manga/vim-watchdogs')
call dein#add('jceb/vim-hier')
call dein#add('dannyob/quickfixstatus')
call dein#add('majutsushi/tagbar') " Vim plugin that displays tags in a window, ordered by class etc.
call dein#add('vim-scripts/tagbar-phpctags' ,{
      \ 'if': executable('chmod'),
      \ 'build': 'chmod +x bin/phpctags',
      \})
call dein#add('mfumi/ref-dicts-en')
call dein#add('rhysd/clever-f.vim')
call dein#add('osyo-manga/vim-over') " :substitute preview
call dein#add('LeafCage/yankround.vim') " レジスタの履歴を取得し再利用する。
call dein#add('osyo-manga/vim-marching', {'rtp': ''}) " Async clang code completion.
call dein#add('osyo-manga/vim-snowdrop')
call dein#add('shawncplus/phpcomplete.vim') " Improved PHP omnicompletion
call dein#add('osyo-manga/vim-monster', {
      \ 'if': has('ruby') && executable('gem'),
      \ 'build' : 'gem install rcodetools',
      \})
call dein#add('mackee/unite-httpstatus')
call dein#add('Shougo/vinarise.vim') " Ultimate hex editing system with Vim
call dein#add('davidhalter/jedi-vim', {
      \ 'if': has('python') || has('python3'),
      \ 'build': 'git submodule update --init --recursive',
      \})
call dein#add('tacroe/unite-mark')
call dein#add('superbrothers/vim-quickrun-markdown-gfm')
call dein#add('sudar/vim-arduino-syntax')
call dein#add('t9md/vim-quickhl')

call dein#add('lambdalisue/vim-gista', {
      \ 'on_cmd': ['Gista'],
      \ 'on_map': '<Plug>(gista-',
      \ 'unite_sources': 'gista',
      \})
call dein#add('lambdalisue/vim-pyenv', {
      \ 'on_source': ['jedi-vim', 'vim-django-support'],
      \ 'on_ft': ['python', 'python3', 'djangohtml']
      \ })
call dein#add('lambdalisue/vim-django-support', {
      \ 'depends': 'vim-pyenv',
      \ 'on_source': 'jedi-vim',
      \ 'on_ft': ['python', 'python3', 'djangohtml']
      \})
call dein#add('kana/vim-operator-user')
call dein#add('kana/vim-operator-replace')
call dein#add('todesking/ruby_hl_lvar.vim', {
      \ 'if' : has('lua'),
      \ 'on_ft' : ['ruby', 'ruby.rspec'],
      \})
call dein#add('yuku-t/vim-ref-ri') " A vim-ref and Unite.vim source for ri.
call dein#add('thinca/vim-localrc')
call dein#add('vim-ruby/vim-ruby')
call dein#add('yuratomo/w3m.vim.git', {'if': executable('w3m')})
call dein#add('cohama/lexima.vim')
call dein#add('koron/codic-vim')
call dein#add('rhysd/unite-codic.vim')
call dein#add('mtth/scratch.vim')
call dein#add('haya14busa/incsearch.vim')
call dein#add('tpope/vim-haml') " Vim runtime files for Haml, Sass, and SCSS
call dein#add('slim-template/vim-slim')
call dein#add('monochromegane/unite-yaml')
call dein#add('vim-scripts/nginx.vim')

call dein#add('tpope/vim-fugitive') " fugitive.vim: a Git wrapper so awesome, it should be illegal
call dein#add('shumphrey/fugitive-gitlab.vim') " An extension to fugitive.vim for gitlab support
call dein#add('idanarye/vim-merginal')
call dein#add('int3/vim-extradite')

call dein#add('hsanson/vim-android', {'rtp': ''})
call dein#add('cohama/agit.vim')
call dein#add('airblade/vim-gitgutter')

call dein#add('chase/vim-ansible-yaml')
call dein#add('noprompt/vim-yardoc')
call dein#add('Shougo/echodoc.vim')
"call dein#add('gilligan/vim-lldb')
call dein#add('elixir-lang/vim-elixir')
call dein#add('mattreduce/vim-mix', {'on_ft': 'elixir'})
call dein#add('BjRo/vim-extest', {'on_ft': 'elixir'})
call dein#add('avdgaag/vim-phoenix', {'on_ft': 'elixir'})
call dein#add('wannesm/wmgraphviz.vim')
call dein#add('marijnh/tern_for_vim', {
      \ 'if': has('python') && executable('npm'),
      \ 'build': 'npm install',
      \ 'on_ft': 'javascript',
      \ 'on_func': ['tern#Complete', 'tern#Enable'],
      \})
call dein#add('ElmCast/elm-vim', {'on_ft': 'elm'})
call dein#add('rhysd/vim-crystal', {'on_ft': 'crystal'})
call dein#add('udalov/kotlin-vim', {'on_ft': 'kotlin'})
call dein#add('tpope/vim-fireplace', {'on_ft': 'clojure'})
call dein#add('ujihisa/neoclojure.vim', {'on_ft': 'clojure'})
call dein#add('vim-scripts/gtags.vim')
call dein#add('slashmili/alchemist.vim', {'on_ft': 'elixir'})
call dein#add('powerman/vim-plugin-AnsiEsc')
call dein#add('vim-erlang/vim-erlang-omnicomplete', {'on_ft': 'erlang'})

call dein#add('rust-lang/rust.vim', {'on_ft': 'rust'})
call dein#add('racer-rust/vim-racer', {'on_ft': 'rust'})

call dein#add('leafgarland/typescript-vim')
call dein#add('Quramy/tsuquyomi', {'on_ft': 'typescript'})

call dein#add('dart-lang/dart-vim-plugin')
call dein#add('miyakogi/vim-dartanalyzer', {'on_ft': 'dart'})

call dein#add('fatih/vim-go', {'on_ft': 'go'})

call dein#add('Shougo/denite.nvim')
call dein#add('Shougo/deoplete.nvim')
if !has('nvim')
  call dein#add('roxma/nvim-yarp')
  call dein#add('roxma/vim-hug-neovim-rpc')
endif

call dein#add('lambdalisue/gina.vim')

call dein#add('w0rp/ale')

call dein#end()

filetype plugin indent on
syntax enable


"----------------------------------------------------------------
"プラグインの設定 オートコマンド・キーマップの設定 (プラグインを使う)"
"----------------------------------------------------------------
"
"----------------------------------------------------------------
"solarized colorscheme 設定(gvimrcにてcolorschemeをmolokai){{{
if dein#tap("vim-colors-solarized")
  let g:solarized_termcolors=256
  let g:solarized_termtrans=1
  set background=dark
  colorscheme solarized
endif
"if dein#tap("vim-tomorrow-theme") && has('unix') 
"  colorscheme Tomorrow-Night-Eighties
"endif
"}}}
if has('unix')
  colorscheme elflord
endif

"----------------------------------------------------------------
" Unite VimFiler Tagbar{{{"

nnoremap [ide] <Nop>
nnoremap [git] <Nop>
nmap <Space> [ide]
nmap <Space>g [git]

noremap  <silent>[ide]f :<C-u>VimFilerBufferDir<Enter>
noremap  <silent>[ide]t :<C-u>TagbarToggle<Enter>
noremap  <silent>[ide]q :bd<Enter>

if dein#tap("unite.vim")
  nnoremap [unite] <Nop>
  nmap <Space>u [unite]
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

  if executable('ag')
    let g:unite_source_grep_command = 'ag'
    let g:unite_source_grep_default_opts = '--nogroup --nocolor --column'
    let g:unite_source_grep_recursive_opt = ''
  endif
endif

if dein#tap("denite.nvim")
  nnoremap [denite] <Nop>
  nmap <Space>d [denite]
  noremap  <silent>[denite]b :<C-u>Denite buffer<Enter>
  noremap  <silent>[denite]h :<C-u>Denite file_mru<Enter>
  noremap  <silent>[denite]g :<C-u>Denite grep<Enter>

  if executable('rg')
    call denite#custom#var('grep', 'command', ['rg'])
    call denite#custom#var('grep', 'default_opts', ['--vimgrep', '--no-heading'])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])
  endif

  call denite#custom#option('default', 'prompt', '>')
  call denite#custom#map(
        \ 'insert',
        \ '<C-n>',
        \ '<denite:move_to_next_line>',
        \ 'noremap'
        \)
  call denite#custom#map(
        \ 'insert',
        \ '<C-p>',
        \ '<denite:move_to_previous_line>',
        \ 'noremap'
        \)
  call denite#custom#map('insert', '<C-a>', '<Home>')
  call denite#custom#map('insert', '<C-e>', '<End>')
  call denite#custom#map('insert', '<C-f>', '<Right>')
  call denite#custom#map('insert', '<C-b>', '<Left>')

  call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
        \ [ '.git/', '.ropeproject/', '__pycache__/',
        \   'venv/', 'images/', '*.min.*', 'img/', 'fonts/'])
endif

" }}}


"----------------------------------------------------------------
" lexima.vim {{{
if dein#tap("lexima.vim")
  let g:lexima_no_default_rules = 1
  call lexima#set_default_rules()
endif
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
" NeoComplete設定 {{{
if dein#tap("neocomplete")
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
  let g:neocomplete#enable_auto_close_preview = 0
  call neocomplete#custom#source('file','rank','10')

  inoremap <expr><C-Tab> neocomplete#start_manual_complete()
  inoremap <expr><C-x><C-u> neocomplete#start_manual_complete('omni')
  "inoremap <expr><C-x><C-o> neocomplete#start_manual_complete('omni')
  inoremap <expr><C-x><C-f> neocomplete#start_manual_complete('file')
  inoremap <expr><C-x><C-t> neocomplete#start_manual_complete('tag')
  inoremap <expr><C-x><C-l> neocomplete#start_manual_complete('file/include')
  inoremap <expr><C-x><C-s> neocomplete#start_manual_complete('neosnippet')
  inoremap <expr><C-x><C-b> neocomplete#start_manual_complete('buffer')
  inoremap <expr><C-x><C-w> neocomplete#start_manual_complete('look')
  imap <C-u> <Plug>(neocomplete_start_unite_complete)
  imap <C-q> <Plug>(neocomplete_start_unite_quick_match)
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
        \ '_':['neosnippet', 'tag', 'file/include', 'omni', 'file', 'look', 'buffer'],
        \}

  if !exists('g:neocomplete#include_patterns')
    let g:neocomplete#include_patterns = {}
  endif

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
  " let g:neocomplete#force_omni_input_patterns.ruby   = '[^. *\t]\.\h\w*\|\h\w*::'
  let g:neocomplete#sources#omni#input_patterns.ruby = '[^. *\t]\.\h\w*\|\h\w*::'
  let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
  " let g:neocomplete#force_omni_input_patterns.c      = '[^.[:digit:] *\t]\%(\.\|->\)\w*'
  let g:neocomplete#force_omni_input_patterns.cpp    = '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
  let g:neocomplete#sources#snowdrop#enable = 1

  "In javascript buffers, completes from js and html buffers.
  " let g:neocomplete#sources#omni#functions.javascript = 'tern#Complete'
  let g:neocomplete#force_omni_input_patterns.javascript    = '[^. \t]\.\w*'

  let g:neocomplete#force_omni_input_patterns.elixir = '[^.[:digit:] *\t]\.'


  augroup omnifuncs
    autocmd!
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType css           setlocal omnifunc=csscomplete#CompleteCSS
    " autocmd FileType javascript    setlocal omnifunc=jscomplete#CompleteJS
    if dein#tap("tern_for_vim")
      autocmd FileType javascript  setlocal omnifunc=tern#Complete
    endif
    if dein#tap("jedi-vim")
      autocmd FileType python      setlocal omnifunc=jedi#completions
    endif
    autocmd FileType xml           setlocal omnifunc=xmlcomplete#CompleteTags
    if dein#tap("alchemist.vim")
      autocmd FileType elixir      setlocal omnifunc=elixircomplete#Complete
    endif
    if dein#tap("vim-erlang-omnicomplete")
      autocmd FileType erlang      setlocal omnifunc=erlang_complete#Complete
    endif
    if dein#tap("tsuquyomi")
      autocmd FileType typescript  setlocal omnifunc=tsuquyomi#complete
    endif

    " autocmd FileType ruby          setlocal omnifunc=rubycomplete#Complete
    " autocmd FileType java          set omnifunc=javacomplete#Complete
    " autocmd FileType java          set completefunc=javacomplete#Complete
  augroup END
endif
"}}}

"----------------------------------------------------------------
" quickrun設定 important unite_quickfix,shabadoubi {{{
  "実行中のquickrunセッションの終了
  "
  "   \   'outputter' : 'multi:buffer:quickfix',
  "   \   'hook/close_unite_quickfix/enable_hook_loaded' : 1,
  "   \   'hook/unite_quickfix/enable_failure' : 1,
  "   \   'hook/close_quickfix/enable_finish' : 1,
      " \   'outputter/buffer/split' : ':botright 8sp',
  "
nnoremap <expr><silent><C-c> quickrun#is_running() ? quickrun#sweep_sessions() : "\<C-c>"
let g:quickrun_config = {
      \ '_' : {
      \   'hook/quickfix_replace_tempname_to_bufnr' : 1,
      \   'hook/close_buffer/enable_failure' : 1,
      \   'hook/close_buffer/enable_empty_data' : 1,
      \   'hook/close_quickfix/enable_success': 1,
      \   'hook/echo/enable' : 1,
      \   'hook/echo/output_success' : 'success',
      \   'hook/echo/output_failure' : 'failure',
      \   'runner' : 'vimproc',
      \   'runner/vimproc/updatetime' : 40,
      \   'outputter' : 'multi:buffer:quickfix',
      \ },
      \ 'java' : {
      \   'exec' : ['javac %o %s:p', '%c %s:t:r %a'],
      \   'hook/sweep/files' : '%S:p:r.class', 
      \   'tempfile' : '%{expand("%")}',
      \ },
      \ 'html' : {
      \   'outputter' : 'browser',
      \ },
      \ 'javascript/babel' : {
      \   'command' : 'babel',
      \   'exec' : ['%c %o %s:p -o %s:p.babel', 'node %s:p.babel'],
      \   'hook/sweep/files': '%S:p.babel',
      \ },
      \ 'markdown' : {
      \   'type': 'markdown/gfm',
      \   'outputter' : 'browser',
      \ },
      \ 'tex' : {
      \   'type': 'tex/latexmk',
      \ },
      \ 'tex/platex' : {
      \   'type': 'tex/platex',
      \   'command' : 'platex',
      \   'exec' : ['%c %s:p', 'dvipdfmx %s:p:r.dvi'],
      \   'outputter' : 'quickfix',
      \ },
      \ 'tex/pdflatex' : {
      \   'type': 'tex/pdflatex',
      \   'command' : 'pdflatex',
      \   'exec' : ['%c %s', 'evince %s:p:r.pdf'],
      \   'outputter' : 'quickfix',
      \ },
      \ 'tex/latexmk' : {
      \   'type': 'tex/latexmk',
      \   'command' : 'latexmk',
      \   'exec' : ['%c %o %s'],
      \   'cmdopt': '-pdfdvi',
      \   'outputter' : 'quickfix',
      \ },
      \ 'c' : {
      \   'type' : 'c/default',
      \ },
      \ 'c/default' : {
      \   'command' : 'gcc',
      \   'cmdopt' : '-Wall -O2 -g',
      \   'exec' : ['%c %o %s -o %s:p:r', '%s:p:r %a'],
      \   'tempfile': '%{tempname()}.c',
      \   'hook/sweep/files': '%S:p:r'
      \ },
      \ 'c/bluetooth' : {
      \   'command' : 'gcc',
      \   'cmdopt' : '-Wall -O2 -g -lbluetooth -pthread',
      \   'exec' : ['%c %o %s -o %s:p:r', '%s:p:r %a'],
      \   'tempfile': '%{tempname()}.c',
      \   'hook/sweep/files': '%S:p:r'
      \ },
      \ 'c/gtk' : {
      \   'command' : 'gcc',
      \   'cmdopt' : '-Wall -O2 -g `pkg-config --cflags --libs gtk+-2.0`',
      \   'exec' : ['%c %o %s -o %s:p:r', '%s:p:r %a'],
      \   'tempfile': '%{tempname()}.c',
      \   'hook/sweep/files': '%S:p:r'
      \ },
      \ 'c/gtk3' : {
      \   'command' : 'gcc',
      \   'cmdopt' : '-Wall -O2 -g `pkg-config --cflags --libs gtk+-3.0`',
      \   'exec' : ['%c %o %s -o %s:p:r', '%s:p:r %a'],
      \   'tempfile': '%{tempname()}.c',
      \   'hook/sweep/files': '%S:p:r'
      \ },
      \ 'c/graphviz' : {
      \   'command' : 'gcc',
      \   'cmdopt' : '-Wall -O2 -g `pkg-config --cflags --libs libgvc`',
      \   'exec' : ['%c %o %s -o %s:p:r', '%s:p:r %a'],
      \   'tempfile': '%{tempname()}.c',
      \   'hook/sweep/files': '%S:p:r'
      \ },
      \ 'c/opencv' : {
      \   'command' : 'gcc',
      \   'cmdopt' : '-Wall -O2 -g `pkg-config --cflags --libs opencv`',
      \   'exec' : ['%c %o %s -o %s:p:r', '%s:p:r %a'],
      \   'tempfile': '%{tempname()}.c',
      \   'hook/sweep/files': '%S:p:r'
      \ },
      \ 'c/gstreamer-0.10' : {
      \   'command' : 'gcc',
      \   'cmdopt' : '-Wall -O2 -g `pkg-config --cflags --libs gstreamer-0.10`',
      \   'exec' : ['%c %o %s -o %s:p:r', '%s:p:r %a'],
      \   'tempfile': '%{tempname()}.c',
      \   'hook/sweep/files': '%S:p:r'
      \ },
      \ 'c/gstreamer' : {
      \   'command' : 'gcc',
      \   'cmdopt' : '-Wall -O2 -g `pkg-config --cflags --libs gstreamer`',
      \   'exec' : ['%c %o %s -o %s:p:r', '%s:p:r %a'],
      \   'tempfile': '%{tempname()}.c',
      \   'hook/sweep/files': '%S:p:r'
      \ },
      \ 'cpp/opencv' : {
      \   'command' : 'g++',
      \   'cmdopt' : '-Wall -O2 -g `pkg-config --cflags --libs opencv`',
      \   'exec' : ['%c %o %s -o %s:p:r', '%s:p:r %a'],
      \   'tempfile': '%{tempname()}.c',
      \   'hook/sweep/files': '%S:p:r'
      \ },
      \ 'arduino' : {
      \   'type' : 'arduino/ino',
      \ },
      \ 'arduino/ino' : {
      \   'command' : 'ino',
      \   'exec' : '%c build %s',
      \ },
      \ 'ruby/testunit' : {
      \ },
      \ 'ruby/rspec' : {
      \   'command' : 'rspec',
      \   'cmdopt' : '--format progress -I .',
      \   'filetype' : 'result.rspec',
      \ },
      \}

nmap <Leader><Space>r :<C-u>QuickRun 
" }}}
 
"----------------------------------------------------------------
" watchdogs {{{
if dein#tap("vim-watchdogs")
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
        " \   'hook/unite_quickfix/enable_failure' : 1,
  let g:quickrun_config["watchdogs_checker/_"] = {
        \   'hook/close_quickfix/enable_exit' : 1,
        \ }
  let g:quickrun_config["watchdogs_checker/gtk"] = {
        \   'type' : 'watchdogs_checker/gcc',
        \   'cmdopt' : '-Wall -fsyntax-only `pkg-config --cflags --libs gtk+-2.0`',
        \ }
  let g:quickrun_config["watchdogs_checker/gtk3"] = {
        \   'type' : 'watchdogs_checker/gcc',
        \   'cmdopt' : '-Wall -fsyntax-only `pkg-config --cflags --libs gtk+-3.0`',
        \ }
  let g:watchdogs_check_BufWritePost_enable_on_wq = 0
  call watchdogs#setup(g:quickrun_config)
  let g:watchdogs_check_BufWritePost_enable_on_wq = 0
endif
"}}}

"----------------------------------------------------------------
" Simple-Javascript-Indenter設定{{{
let g:SimpleJsIndenter_CaseIndentLevel = -1
"}}}
"----------------------------------------------------------------
" jedi-vim設定 {{{
    let g:jedi#completions_enabled = 0
    let g:jedi#auto_vim_configuration = 0
    let g:jedi#rename_command = ""
    let g:jedi#force_py_version = 3
" }}}
"----------------------------------------------------------------
" android-path設定 {{{
if has('mac')
  let g:android_sdk_path = 'XXX:EDIT'
elseif has('unix')
  let g:android_sdk_path = '~/android-sdk-linux'
endif
" }}}

"----------------------------------------------------------------
" vim-ruby設定 {{{
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_classes_in_global = 1
augroup rubySettings
  autocmd!
  autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
  autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
  autocmd User Rails let g:rubycomplete_rails = 1
augroup END
" }}}

"----------------------------------------------------------------
" monster設定 {{{
if dein#tap("vim-monster")
 let g:monster#completion#rcodetools#backend = 'async_rct_complete'
endif
" }}}

"----------------------------------------------------------------
" ruby_hl_lvar設定 {{{
let g:ruby_hl_lvar_auto_enable = 1
" }}}
"
"----------------------------------------------------------------
" quickhl設定 {{{
if dein#tap("vim-quickhl")
  nmap mm <Plug>(quickhl-manual-this)
  xmap mm <Plug>(quickhl-manual-this)
  nmap MM <Plug>(quickhl-manual-reset)
  xmap MM <Plug>(quickhl-manual-reset)
endif
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
        \ split(glob('/usr/include/*/c++/*/*'), '\n') +
        \ split(glob('/usr/include/*'), '\n') +
        \ split(glob('/usr/include/gtk-2.0/*'), '\n') +
        \ split(glob('/usr/include/gtk-2.0/*/*'), '\n') +
        \ split(glob('/usr/lib/jvm/default/include/*'), '\n') +
        \ split(glob('/usr/lib/jvm/default/include/linux/*'), '\n'), 
        \ 'isdirectory(v:val)')
  let g:snowdrop#libclang_directory = '/usr/lib' "archlinux
  " let g:snowdrop#libclang_directory = '/usr/lib/x86_64-linux-gnu/' "ubuntu
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
" jscomplete-vim設定 {{{
let g:jscomplete_use = ['dom', 'moz', 'es6th']
" }}}

" tern-vim設定 {{{
" }}}

"----------------------------------------------------------------
" neosnippet設定 {{{
let g:neosnippet#snippets_directory = "~/.vim/vimnewfiles/snippet"
let g:neosnippet#enable_completed_snippet = 1
imap <C-s> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_jump)
" imap <C-s> <Plug>(neosnippet_expand_or_jump)
" }}}

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
" vim-indent-guides設定 {{{
if dein#tap("vim-indent-guideline")
  let g:indent_guides_guide_size = 2
  let g:indent_guides_enable_on_vim_startup = 1
endif
" }}}

"----------------------------------------------------------------
" indentLine設定 {{{
if dein#tap("indentLine")
  " let g:indentLine_char = "︙"
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
" QfixHowm/qfixmemo設定 {{{
if dein#tap("qfixhowm")
  let QfixHowm_key = 'g'
  let howm_dir = expand('~/Dropbox') . '/editedByVim/howm'
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
  " openuri browser {{{
  if has('unix')
    let openuri_cmd = "call system('firefox %s &')"
  else
    " let openuri_cmd = '!start "rundll32.exe" url.dll,FileProtocolHandler %s'
    " Internet explorer
    " let openuri_cmd = '!start "C:/Program Files/Internet Explorer/iexplore.exe" %s'
    " firefox
    let openuri_cmd = '!start "C:/Program Files/Mozilla Firefox/firefox.exe" %s'
  endif
  " }}}
endif
" }}}

"----------------------------------------------------------------
" Easymotion設定 {{{
let g:EasyMotion_hl_group_target = 1
let g:EasyMotion_grouping = 1
let g:EasyMotion_do_shade = 1
let g:EasyMotion_keys ="asdfghjkl;'qwertyuiop[]"
let g:EasyMotion_hl_group_shade = 'Comment'
let g:EasyMotion_hl_group_target = 'Search'
" }}}

"----------------------------------------------------------------
" Ref設定 ref.vim ref-sources.vim {{{
" man
let g:ref_man_cmd = "man"
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
" }}}

"----------------------------------------------------------------
"unite-ruby-require.vim設定 {{{
let g:unite_source_ruby_require_ruby_command = '$HOME/.rbenv/shims/ruby'
" }}}
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
  nnoremap <buffer>[rails] <Nop>
  nmap     <buffer><Space>r [rails]
  nnoremap <buffer>[rails]r :R<CR>
  nnoremap <buffer>[rails]a :A<CR>
  nnoremap <buffer>[rails]m :Emodel<Space>
  nnoremap <buffer>[rails]c :Econtroller<Space>
  nnoremap <buffer>[rails]v :Eview<Space>
  nnoremap <buffer>[rails]p :Epreview<CR>
endfunction

aug MyAutoCmd
  au User Rails call SetUpRailsSetting()
aug END

aug RailsDictSetting
  au!
aug END
" }}}
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
"airline {{{
set laststatus=2
let g:airline_detect_modified=1
let g:ariline_detect_paste=1
" let g:airline_enable_branch=1
let g:airline_theme='dark'
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
" }}}

"----------------------------------------------------------------
" anzu {{{
if dein#tap("vim-anzu")
  nmap n <Plug>(anzu-n)
  nmap N <Plug>(anzu-N)
  nmap * <Plug>(anzu-star)
  nmap # <Plug>(anzu-sharp)
endif
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
if dein#tap("incsearch.vim")
  map / <Plug>(incsearch-forward)
  map ? <Plug>(incsearch-backward)
  map g/ <plug>(incsearch-stay)
endif
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
      \ 'ctagsbin' : 'jsctags'
      \}
" let g:tagbar_phpctags_bin = '/home/kaki/phpctags/phpctags'
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
let g:tagbar_type_armasm = {
    \ 'ctagsbin'  : 'ctags',
    \ 'ctagsargs' : '-f- --format=2 --excmd=pattern --fields=nksSa --extra= --sort=no --language-force=asm',
    \ 'kinds' : [
        \ 'm:macros:0:1',
        \ 't:types:0:1',
        \ 'd:defines:0:1',
        \ 'l:labels:0:1'
    \ ]
\}

let g:tagbar_show_linenumbers = 2
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
" Codic {{{
nmap <silent><space>en :Codic<Enter>
" }}}

"---------------------------------------------------------------
" Fugitive {{{
noremap <silent>[git]s :<C-u>Gstatus<Enter>
noremap <silent>[git]r :<C-u>Gread<Enter>
noremap <silent>[git]w :<C-u>Gwrite<Enter>
noremap <silent>[git]c :<C-u>Gcommit<Enter>
noremap <silent>[git]b :<C-u>Gblame<Enter>
noremap [git]d :<C-u>Gdiff 
noremap <silent>[git]l :<C-u>Glog<Enter>
" }}}

"---------------------------------------------------------------
" GitGutter {{{
noremap <silent>[git]n :<C-u>GitGutterNextHunk<Enter>
noremap <silent>[git]p :<C-u>GitGutterPrevHunk<Enter>
" }}}
"
"---------------------------------------------------------------
" Agit {{{
nmap <silent><space>gv :Agit<Enter>
" }}}

"---------------------------------------------------------------
" Ansible-yaml {{{
let g:ansible_options = {'ignore_blank_lines' : 0}
" }}}

"---------------------------------------------------------------
" Echodoc {{{
set cmdheight=2
let g:echodoc_enable_start_up=1
" }}}

"---------------------------------------------------------------
" Gundo {{{
let g:gundo_width = 60
let g:gundo_preview_height = 40
let g:gundo_right = 1
map [gundo] <Nop>
" }}}

"---------------------------------------------------------------
" JSX {{{
let g:jsx_ext_required = 1 "js拡張子でも有効にするときは0
" }}}

"---------------------------------------------------------------
" vivi {{{
" let g:vivi_enable_auto_warm_up_iex = 1
" let g:vivi_enable_omni_completion = 0 " Neocomplete
" }}}

"---------------------------------------------------------------
" alchemist.vim {{{
" }}}

"---------------------------------------------------------------
" rust {{{
let g:rust_conceal = 0
" let g:rustfmt_autosave = 1
" }}}

"---------------------------------------------------------------
" vim-racer {{{
let $RUST_SRC_PATH = expand('~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src')
" }}}

"----------------------------------------------------------------
" Filetype settings"
"----------------------------------------------------------------
" .vim/after/ftplugins/.

augroup filetype_settings
  autocmd!

  autocmd FileType text call s:text_filetype_settings()
  function! s:text_filetype_settings()
    setlocal iminsert=0 shiftround
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
    if has('gui_running')
      setlocal balloonexpr=snowdrop#ballonexpr_typeof()
      setlocal balloondelay=15
      setlocal ballooneval
    endif
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
    setlocal tabstop=2 shiftwidth=2 shiftround
  endfunction

  autocmd FileType ruby call s:ruby_filetype_settings()
  function! s:ruby_filetype_settings()
    setlocal tabstop=2 shiftround shiftwidth=2
    setlocal completeopt-=preview
    
    if dein#tap("neocomplete")
    endif
    noremap <buffer> <silent>[unite]i :Unite ruby/require<Enter>
    noremap <buffer> <silent><Leader>t :QuickRun
    noremap <buffer> <silent>[ide]v :Unite -start-insert -default-action=split ref/ri<Enter>
    setlocal keywordprg=ri
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
    setlocal keywordprg=pydoc
  endfunction

  autocmd FileType php call s:php_filetype_settings()
  function! s:php_filetype_settings()
    setlocal tabstop=4
    setlocal shiftwidth=4
  endfunction

  autocmd FileType elixir call s:elixir_filetype_settings()
  function! s:elixir_filetype_settings()
    setlocal tabstop=2
    setlocal shiftwidth=2
  endfunction

  autocmd BufWinEnter,BufNewFile *.cr set filetype=crystal
  autocmd FileType crystal call s:crystal_filetype_settings()
  function! s:crystal_filetype_settings()
    setlocal tabstop=2 shiftwidth=2
  endfunction

  autocmd BufWinEnter,BufNewFile *.kt set filetype=kotlin
  autocmd FileType kotlin call s:kotlin_filetype_settings()
  function! s:kotlin_filetype_settings()
    setlocal tabstop=2
    setlocal shiftwidth=2
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

  autocmd FileType ref-man call s:ref_filetype_settings()
  autocmd FileType ref-pydoc call s:ref_filetype_settings()
  autocmd FileType ref-ri call s:ref_filetype_settings()
  autocmd FileType ref-kotobank call s:ref_filetype_settings()
  function! s:ref_filetype_settings()
    nnoremap <buffer> q :q<Enter>
  endfunction

  autocmd BufWinEnter,BufNewFile *.epub set filetype=epub
  autocmd BufReadCmd *.epub call zip#Browse(expand("<amatch>"))
  autocmd FileType epub call s:epub_filetype_settings()
  function! s:epub_filetype_settings()
    nnoremap <buffer> q :q<Enter>
  endfunction

  autocmd FileType rust call s:rust_filetype_settings()
  function! s:rust_filetype_settings()
    nmap <silent><buffer> gd <Plug>(rust-def)
    nmap <silent><buffer> <C-w>gf <Plug>(rust-def)
    nmap <silent><buffer> K <Plug>(rust-doc)
    let g:quickrun_config["rust/cargo"] = {'exec': 'cargo run'}
  endfunction
  autocmd FileType rustdoc call s:ref_filetype_settings()

  autocmd FileType go call s:go_giletype_settings()
  function! s:go_giletype_settings()
    setlocal autowrite
    setlocal noexpandtab
  endfunction

  autocmd FileType gitconfig call s:gitconfig_filetype_settings()
  function! s:gitconfig_filetype_settings()
    setlocal tabstop=2
    setlocal shiftwidth=2
    setlocal noexpandtab
  endfunction

augroup END

"----------------------------------------------------------------
" My Functions"
"----------------------------------------------------------------
command! LinuxKernelReading call s:LinuxKernelReading()
function! s:LinuxKernelReading()
  let s:kernel_source_directory = "$HOME/src/linux"
  tabnew "LinuxKernel"
  execute "lcd ".s:kernel_source_directory
  execute "VimFilerSimple -auto-cd -explorer -winwidth=40 -focus -status -no-quit"
  let &path = 
        \ join(
        \ filter(split(glob(s:kernel_source_directory."/*/*/*"), '\n'), 'isdirectory(v:val)'),
        \ ',')
        \ . &path 
endfunction

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

command! Gentags call s:Gentags()
function! s:Gentags()
  execute ":silent !ctags -R"
  call s:Notification("ctags", "tags generated")
  execute ":redraw!"
endfunction

" TODO:implements
function! s:Notification(app, str)
  let s:notification_icon_path = expand('~/src/vim/src/vim.ico')
  let s:notification_opts = "-a " . a:app . " -i " . s:notification_icon_path . " "
  execute ":silent ! notify-send " . s:notification_opts . a:str
endfunction

command! VimrcEdit call s:VimrcEdit()
function! s:VimrcEdit()
  edit $MYVIMRC
endfunction

command! ShiftJISEdit call s:ShiftJIS()
function! s:ShiftJIS()
  edit ++enc=shift_jis
endfunction

command! CargoRun call s:CargoRun()
function! s:CargoRun()
  !cargo run
endfunction

command! CargoBuild call s:CargoBuild()
function! s:CargoBuild()
  !cargo build
endfunction

command! CargoTest call s:CargoTest()
function! s:CargoTest()
  !cargo test
endfunction

command! CargoCheck call s:CargoCheck()
function! s:CargoCheck()
  !cargo check
endfunction
