if [[ -a $HOME/.oh-my-zsh ]]; then
    export ZSH=$HOME/.oh-my-zsh
    ZSH_THEME="ys"
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
if [[ -d $HOME/.rbenv/ && -x `which rbenv` ]]; then
   eval "$(rbenv init -)"
fi

# ruby-related aliases
alias gem=gem || rbenv rehash

# ansible aliases
alias ave='ansible-vault edit --vault-password-file .vault_pass'
alias avv='ansible-vault view --vault-password-file .vault_pass'
alias ec=emacsclient

export PATH="$HOME/.bin:$PATH"
export EDITOR=emacsclient
