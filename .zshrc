if [ -f ~/.zshrc.local ]; then
  source ~/.zshrc.local
fi
########## Environment variables ##########
export EDITOR=vim

export ANDROID_SDK_ROOT="${HOME}/sdk/android-sdk-linux"
export ANDROID_HOME="${ANDROID_SDK_ROOT}"
export ANDROID_NDK="${ANDROID_SDK_ROOT}/ndk-bundle"
export ANDROID_NDK_HOME="${ANDROID_NDK}"
export NDK_HOME="${ANDROID_NDK}"

export GOPATH="${HOME}/golang"
########## PATH ##########
if [ -d $HOME/.anyenv ] ; then
    export PATH="$HOME/.anyenv/bin:$PATH"
    eval "$(anyenv init -)"
fi

export PATH="${ANDROID_SDK_ROOT}/platform-tools:${PATH}"
export PATH="${ANDROID_SDK_ROOT}/tools:${PATH}"
export PATH="${ANDROID_NDK}:${PATH}"
export PATH="${GOPATH}/bin:${PATH}"
export PATH="${HOME}/bin:${PATH}"

########## Alias ##########
alias la='ls -alFZ --show-control-char --color=always'
alias ls='ls --show-control-char --color=always'
alias tmux='tmux -2'

alias wireshark='LIBOVERLAY_SCROLLBAR=0 `which wireshark`'

alias rust-musl-builder='docker run --rm -it -v "$(pwd)":/home/rust/src ekidd/rust-musl-builder'

########## zsh options ##########
REPORTTIME=3
bindkey -e
bindkey "^[OH" beginning-of-line # HOME key
bindkey "^[OF" end-of-line       # END key
bindkey "^[[3~" delete-char      # Delete key

WORDCHARS='*?_-.[]~=&;!#$%^()<>' # When pressed Ctrl-w, delete words to the front space or / or { or }
########## History ##########
HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=$HISTSIZE

setopt extended_history
setopt hist_no_store
setopt hist_ignore_dups
setopt hist_ignore_space
setopt share_history
setopt no_flow_control
setopt auto_pushd

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end

########## Completion ##########
# reverse complete when press Shift-Tab
bindkey "^[[Z" reverse-menu-complete
# match uppercase and undercase when completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# separate section when completion manual(man)
zstyle ':completion:*:manuals' separate-sections true
autoload -U compinit
compinit -u

setopt auto_list
setopt auto_menu
setopt correct
setopt auto_param_keys
setopt auto_param_slash
setopt mark_dirs
setopt list_types
setopt extended_glob
setopt magic_equal_subst
setopt interactive_comments
setopt equals
setopt notify
setopt no_beep

########## zplug ##########
source ~/.zplug/init.zsh

zplug "chrissicool/zsh-256color", use:"zsh-256color.plugin.zsh"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"

zplug "junegunn/fzf-bin", \
  from:gh-r, \
  as:command, \
  rename-to:fzf, \
  use:"*darwin*amd64*"

zplug "simonwhitaker/gibo", \
  from:github, \
  as:command, \
  use:'gibo _gibo', \
  hook-build:'mv gibo-completion.zsh _gibo'

zplug "dracula/zsh", as:theme

zplug "zsh-users/zsh-syntax-highlighting", defer:2

# Add local completions if exists.
_local_completions="${HOME}/.zsh/completions"
if [ -d ${_local_completions} ]; then
  zplug ${_local_completions}, from:local
fi
# Install all plugins
if ! zplug check --verbose; then
  echo; zplug install
fi

zplug load

########## Peco ##########

if [ -x "$(command -v peco)" ]; then
  function _peco-select-history() {
    local tac
    if which tac > /dev/null; then
      tac="tac"
    else
      tac="tail -r"
    fi
    BUFFER=$(\
      \history -n 1 |\
      eval $tac |\
      peco --query "$LBUFFER"\
      )
    CURSOR=$#BUFFER
    zle clear-screen
  }
  zle -N peco-select-history _peco-select-history

  function _peco-select-docker-history() {
    local tac
    if which tac > /dev/null; then
      tac="tac"
    else
      tac="tail -r"
    fi
    BUFFER=$(\
      \history -n 1 |\
      eval $tac |\
      grep "docker run" |\
      peco --query "$LBUFFER"\
      )
    CURSOR=$#BUFFER
    zle clear-screen
  }
  zle -N peco-select-docker-history _peco-select-docker-history

  bindkey '^R' peco-select-history
  bindkey '^Rd' peco-select-docker-history
fi
