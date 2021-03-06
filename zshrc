if [[ -a $HOME/.oh-my-zsh ]]; then
    export ZSH=$HOME/.oh-my-zsh
    ZSH_THEME="lambda"
    plugins=(git rails ruby)
    source $ZSH/oh-my-zsh.sh
fi

# check if OCaml's OPAM is installed
if [[ -a $HOME/.opam/opam-init/init.sh ]]; then
    . $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

# check if cabal is installed
if [[ -d $HOME/.cabal/bin ]]; then
    export PATH="$HOME/.cabal/bin:$PATH"
fi

# check if Android SDK is installed
if [[ -d $HOME/android-sdk ]]; then
    export PATH="$HOME/android-sdk/tools:$PATH"
fi

# check if EC2 API tools are installed
if [[ -d $HOME/ec2/bin ]]; then
    export EC2_HOME=$HOME/ec2
    export PATH="$EC2_HOME/bin:$PATH"
    export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/jre/ # TODO: remove hardcoded jre path
fi

# check if rbenv is installed
if which rbenv > /dev/null 2>&1; then
    eval "$(rbenv init -)"
fi

# check if eclipse is installed
if [[ -d /opt/eclipse ]]; then
   export PATH="/opt/eclipse:$PATH"
fi

# ansible aliases
alias ave='ansible-vault edit --vault-password-file .vault_pass'
alias avv='ansible-vault view --vault-password-file .vault_pass'
alias gdc="git diff --cached"
alias ec=emacsclient

export PATH="$HOME/.bin:$PATH"
export PATH=".cabal-sandbox/bin:$PATH"
export PATH="bin:$PATH"

if [[ `uname` == "Darwin" ]]; then
    export PATH="/usr/local/bin:$PATH"
    export PATH="/usr/local/sbin:$PATH"
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
fi

export EDITOR=emacsclient
