# Ctrl+Dで抜けるのを防止
setopt IGNOREEOF

# 日本語設定
export LANG=ja_JP.UTF-8
case ${UID} in
    0)
LANG=C
	;;
esac

# PATH設定
export PATH="$HOME/go/bin:$HOME/bin/:/usr/local/go/bin:$PATH"

# 色設定
autoload -Uz colors
colors

# 補完
autoload predict-on
#predict-on
# https://github.com/zsh-users/zsh-completions
fpath=(${HOME}/.zsh_addon/zsh-completions/src $fpath)
autoload -Uz compinit
compinit
setopt list_packed
setopt auto_menu # [Tab]で補完メニュー
setopt auto_list
setopt auto_param_keys
setopt auto_param_slash
setopt globdots
setopt correct
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:descriptions' format '%BCompletion%b %U%d%u'
zstyle ':completion:*default' menu select=2
zstyle ':completion:*' ignore-parents parent pwd

# emacs key bind
bindkey -e

# history
setopt share_history
setopt histignorealldups
setopt hist_ignore_space
setopt hist_ignore_dups
setopt hist_no_store
setopt hist_expand
setopt inc_append_history
setopt bang_hist
setopt extended_history
setopt hist_ignore_space
setopt hist_verify
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^b" history-beginning-search-forward-end
bindkey '^r' history-incremental-pattern-search-backward
bindkey '^s' history-incremental-pattern-search-forward
bindkey '^h' zaw-history

HISTFILE=~/.zsh_history
HISTSIZE=500000
SAVEHIST=500000

# directory
DIRSTACKSIZE=100
setopt auto_cd
setopt auto_pushd # cd -[tab]でhistory
setopt pushd_ignore_dups
autoload -Uz add-zsh-hook
autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs
# cdrコマンドで履歴にないディレクトリにも移動可能に
zstyle ":chpwd:*" recent-dirs-default true

function chpwd(){ls -F --color=always}

# その他
setopt nolistbeep
setopt nohistbeep
setopt no_beep
setopt interactive_comments
setopt magic_equal_subst
setopt mark_dirs
#setopt print_exit_value
setopt rm_star_wait
bindkey '^ ' autosuggest-accept

# alias
setopt complete_aliases
alias ls="ls --color -F"
alias ll="ls -l --color -F"
alias la="ls -a --color -F"
alias lla="ls -la --color -F"
alias diff="diff -u"
alias memcheck="ps alx | awk '{printf (\"%d\t%s\n\", \$8,\$13)}' | sort -nr | head -10"
alias where="command -v"
alias j="jobs -l"
alias sudo="sudo -H"

# Prompt
setopt prompt_subst
PROMPT="%{$fg[green]%}[%n@%m:%~]%# %{${rest_color}%}"
SPROMPT="%{$fg[red]%}%{$suggest%}??? %B %r is correct? [n,y,a,e]:%{${reset_color}%}%b "
autoload -Uz vcs_info
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstaagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "%F{green}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd() { vcs_info }
RPROMPT=$RPROMPT'${vcs_info_msg_0_}'

function mkcd() {
  if [[ -d $1 ]]; then
    echo "$1 already exists!"
    cd $1
  else
    mkdir -p $1 && cd $1
  fi
}

## git clone git://github.com/zsh-users/zaw.git
source ~/.zsh_plugin/zaw/zaw.zsh
## git clone https://github.com/zsh-users/zsh-autosuggestions
source ~/.zsh_plugin/zsh-autosuggestions/zsh-autosuggestions.zsh
## git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
source ~/.zsh_plugin/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

zstyle ':filter-select' case-insensitive yes
bindkey '^xb' zaw-cdr
bindkey '^x^b' zaw-git-recent-branches
bindkey '^x^f' zaw-git-files
bindkey '^x^r' zaw-history
