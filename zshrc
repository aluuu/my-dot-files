if [[ -a $HOME/.oh-my-zsh ]]; then
    export ZSH=$HOME/.oh-my-zsh
    ZSH_THEME="ys"
    plugins=(git)
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
   export PATH="$HOME/ec2/bin:$PATH"
   export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/jre/ # TODO: remove hardcoded jre path
fi

export PATH="$HOME/.bin:$PATH"
export EDITOR=emacsclient
