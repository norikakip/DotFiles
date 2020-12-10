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
if [ -d /usr/local/opt/asdf/ ]; then
  . /usr/local/opt/asdf/asdf.sh
fi
#if [ -d $HOME/.anyenv ] ; then
#    export PATH="$HOME/.anyenv/bin:$PATH"
#    eval "$(anyenv init - zsh --norehash)"
#fi

export PATH="${ANDROID_SDK_ROOT}/platform-tools:${PATH}"
export PATH="${ANDROID_SDK_ROOT}/tools:${PATH}"
export PATH="${ANDROID_SDK_ROOT}/tools/bin:${PATH}"
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

########## zinit ##########
source ~/.zinit/bin/zinit.zsh
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

zinit for \
  light-mode chrissicool/zsh-256color \
  light-mode zsh-users/zsh-autosuggestions \
  light-mode zdharma/fast-syntax-highlighting \
  light-mode dracula/zsh
zinit wait lucid for \
  light-mode zsh-users/zsh-history-substring-search \
  atload"zicompinit; zicdreplay" blockf zsh-users/zsh-completions

zinit wait lucid for \
 from"gh-r" as"program" junegunn/fzf-bin \
 from"gh" as"program" simonwhitaker/gibo \
 from"gh-r" as"program" x-motemen/ghq \
 from"gh" as"program" ryanmjacobs/c

# Add local completions if exists.
#zinit creinstall "${HOME}/.zsh/completions"

########## fzf ##########

if [ -x "$(command -v fzf)" ]; then
  function _fzf-select-history() {
    INITIAL_QUERY="'"
    BUFFER=$(\
      \history -n 1 |\
      awk '!a[$0]++' |\
      sed 's/\\n/\n/' |\
      fzf --layout reverse --height 40% --tac\
      )
    CURSOR=$#BUFFER
    zle clear-screen
  }
  zle -N fzf-select-history _fzf-select-history

  function _fzf-select-docker-history() {
    BUFFER=$(\
      \history -n 1 |\
      grep "docker run" |\
      fzf --tac\
      )
    CURSOR=$#BUFFER
    zle clear-screen
  }
  zle -N fzf-select-docker-history _fzf-select-docker-history

  bindkey '^R' fzf-select-history
  bindkey '^Rd' fzf-select-docker-history
fi

# Added by serverless binary installer
export PATH="$HOME/.serverless/bin:$PATH"
