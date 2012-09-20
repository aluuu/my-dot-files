PATH="/home/aluuu/bin:/usr/lib/cache/bin:${PATH}"
export PATH=${PATH}
export MPD_HOST=localhost
export MPD_PORT=6600
export CCACHE_DIR=/var/tmp/ccache
export DISTCC_DIR=/tmp/distcc
export HISTFILE=~/.zsh_history
export HISTSIZE=50000
export SAVEHIST=50000
export TERM=screen-256color
eval `dircolors ~/.dircolors.256dark`
autoload -Uz compinit && compinit
autoload -U promptinit && promptinit
autoload -U colors && colors
prompt_aluuu_prompt=${1:-'red'}
prompt_aluuu_user=${2:-'red'}
prompt_aluuu_root=${3:-'red'}
base_prompt="<%B%F{$prompt_aluuu_user}%n%b> in "
post_prompt="%b%f%k"
path_prompt="[%B%F{$prompt_aluuu_prompt}%1~%b]"
if [ "$USER" = 'root' ]
  then
    PS1="$base_prompt$path_prompt %F{red}!# $post_prompt"
  else
    PS1="$base_prompt$path_prompt $ $post_prompt"
  fi
PS2="$base_prompt$path_prompt > $post_prompt"
PS3="$base_prompt$path_prompt ?# $post_prompt"

if [[ "$TERM" == "dumb" ]]
then
    prompt gentoo
fi

autoload -U select-word-style
select-word-style bash

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list 'r:|[._-]=** r:|=**'
zstyle ':completion:*' verbose true
zstyle ':completion:*' hosts off
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle :compinstall filename '/home/aluuu/.zshrc'
zstyle ':completion::complete:*' use-cache 1

bindkey "\e[3~" delete-char # del
bindkey "^[[1;5D" backward-word # ctrl+left
bindkey "^[[1;5C" forward-word #ctrl+right
bindkey "^[[A" history-search-backward # up
bindkey "^[[B" history-search-forward # down
bindkey "^[OH" beginning-of-line # home
bindkey "^[OF" end-of-line # end
bindkey "\e[6~" end-of-history # PageDown

alias grep='grep -EHn --color'
alias ls='ls -al --color=auto'
alias unmerge='emerge --unmerge'
