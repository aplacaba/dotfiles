# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.

# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="sorin"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git ruby zsh-syntax-highlighting web-search spectrum)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH=$HOME/bin:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"
t2c () {
  echo $1 | xclip -sel clip
}

r_migrate () {
  be rake db:migrate RAILS_ENV=$1
}

r_rollback () {
  be rake db:rollback RAILS_ENV=$1
}

alias tl='tmux list-sessions'
alias gg='git grep --break --heading -n'
alias be='bundle exec'
alias migrate='r_migrate test; r_migrate development'
alias rollback='r_rollback test; r_rollback development'
alias define='/home/alacaba/Scripts/define/define.rb'
alias btp-connect='ssh tmux@192.168.43.212'
alias bo='bundle open'
alias rserver='be rails s -b 127.0.0.1'
alias agrep='ack-grep'
alias gs='git status'
alias pairprog='ssh tmux@192.168.43.212'
alias rails3='rails _3.2.19_'
alias rails30='rails _3.0_'
alias procs='ps -ef'
alias update-ruby-build='cd /home/a655321/.rbenv/plugins/ruby-build && git pull'
alias copy-sshkeys='cat ~/.ssh/id_rsa.pub | xclip -sel clip'
alias desktop-connect='ssh alacaba@192.168.1.5'
alias mount-desktop='sshfs alacaba@192.168.1.5:/home/alacaba ~/Local\ Desktop'
alias unmount-desktop='sudo umount ~/muh\ desktop'

# Tmux Aliases
alias t='tmux -2 new-session -s '
alias ta='tmux -2 attach-session -t'
alias tl='tmux list-sessions'
alias tx='tmux kill-session -t'
alias reload-tmux='tmux source-file ~/.tmux.conf'
alias copy-tbpass='cat ~/.tbpass.txt | xclip -sel clip'
alias venueey-staging='ssh -i ~/.ssh/venueey.pem ubuntu@ec2-52-74-249-98.ap-southeast-1.compute.amazonaws.com'
export EDITOR='vim'

export GOPATH="$HOME/go"
export PATH="$HOME/.rbenv/bin:$PATH"
export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"
export KERN_DIR=/usr/src/kernels/`uname -r`

eval "$(rbenv init -)"
stty stop ''

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
fortune -ae | cowsay -f daemon -n

# export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
