"GUIでメニューバーとツールバーを消す
set guioptions-=m
set guioptions-=T
"tablineをテキストで
set guioptions-=e

"set guifont=
set guifont=Inconsolata\ for\ Powerline\ 12
if has('mac')
  set guifont=Inconsolata\ for\ Powerline:h14
  set guifontwide=Ricty\ Regular:h14
elseif has('unix')
  set guifont=Inconsolata\ for\ Powerline\ 14
  set guifontwide=Ricty\ 14
endif
set ambiwidth=single
set encoding=utf-8

" 
" Windowの大きさを指定
set lines=50
set columns=120
"半透明にする
"autocmd guienter * set transparency=221


augroup gui-enter
  autocmd!
  autocmd GUIEnter * colorscheme molokai
augroup END

"ColorScheme moloaki
let g:molokai_original = 1
" molokai arange {{{
hi LineNr guifg=Yellow
" }}}

"visualbell
set visualbell
set t_vb=

if has('multi_byte_ime') || has('xim')
  highlight Cursor guifg=NONE guibg=White
  highlight CursorIM guifg=NONE guibg=DarkRed
endif

" AddPATH
" shellのPATHはどこに消えたのか
let tmp_PATH = $PATH
let $PATH = $HOME . "/.rbenv/bin:" . $HOME . "/.rbenv/shims:" . $PATH
if (!executable("rbenv"))
  let $PATH = tmp_PATH
endif
let tmp_PATH = $PATH
let $PATH = $HOME . "/.pyenv/bin:" . $HOME . "/.pyenv/shims:" . $PATH
if (!executable("pyenv"))
  let $PATH = tmp_PATH
endif
let tmp_PATH = $PATH
let $PATH = $HOME . "/.phpenv/bin:" . $HOME . "/.phpenv/shims:" . $PATH
if (!executable("phpenv"))
  let $PATH = tmp_PATH
endif
let tmp_PATH = $PATH
let $PATH = $HOME . "/.nodebrew/current/bin:" . $PATH
if (!executable("nodebrew"))
  let $PATH = tmp_PATH
endif
let tmp_PATH = $PATH
let $PATH = $HOME . "/.lein:" . $PATH
if (!executable("lein"))
  let $PATH = tmp_PATH
endif
unlet tmp_PATH

command! Life call s:lifeis()
function! s:lifeis()
  let s:hour = strftime("%H", localtime())
  if s:hour < 18 && s:hour > 5
    let g:solarized_termcolors=256    "default value is 16
    let g:solarized_contrast="high"    "default value is normal
    syntax enable
    set background=light
    colorscheme solarized
  else
    syntax enable
    set background=dark
    colorscheme molokai
  endif
  unlet s:hour
endfunction
