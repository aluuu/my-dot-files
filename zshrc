if [[ -a $HOME/.oh-my-zsh ]]; then
    export ZSH=$HOME/.oh-my-zsh
    ZSH_THEME="ys"
    plugins=(git)
    source $ZSH/oh-my-zsh.sh
fi
if [[ -a $HOME/.opam/opam-init/init.sh ]]; then
    . $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

export PATH="$HOME/.bin:$PATH"
export EDITOR=emacsclient
