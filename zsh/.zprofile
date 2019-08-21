# git
alias g='git'
alias ga='git add'
alias gc='git commit -v'
alias gc!='git commit --amend'
alias gco='git checkout'
alias gd='git diff'
alias gb='git branch'
alias gs='git status'
alias gg='git grep --break --heading -n'

# tmux
alias t='tmux new-session -s'
alias ta='tmux attach-session -t'
alias tl='tmux list-sessions'
alias tx='tmux kill-session -t'

# ruby
alias be='bundle exec'

# bash
alias xcopy='xclip -sel clip'
alias tcopy='tmux show-buffer | xcopy'

export PATH=$HOME/.rbenv/bin:$PATH
export PATH=$HOME/.rbenv/plugins/ruby-build/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.local/bin:$PATH

# prompt settings
NVM_LAZY_LOAD=true
PURE_PROMPT_SYMBOL=λ
PURE_GIT_UP_ARROW=▲
PURE_GIT_DOWN_ARROW=▼

stty -ixon
