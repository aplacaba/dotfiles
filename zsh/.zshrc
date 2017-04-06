if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug ~/.zplug
    source ~/.zplug/init.zsh && zplug update --self
fi

autoload -U compinit && compinit

# Essential
source ~/.zplug/init.zsh

zplug 'zplug/zplug'
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"
zplug "robertzk/send.zsh"
#zplug "sindresorhus/pure"
zplug "mafredri/zsh-async"
zplug "caarlos0/zsh-pg"
zplug "cusxio/delta-prompt", use:delta.zsh

# ohmyzsh
zplug "plugins/git", from:oh-my-zsh
zplug "plugins/history", from:oh-my-zsh
zplug "plugins/command-not-found", from:oh-my-zsh
zplug "plugins/tmux", from:oh-my-zsh

# Install packages that have not been installed yet
if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  else
    echo
  fi
fi

bindkey -e

zplug load

[ -e "${HOME}/.zshrc_local" ] && source "${HOME}/.zshrc_local"
[ -e "${HOME}/.zsh_aliases" ] && source "${HOME}/.zsh_aliases"

