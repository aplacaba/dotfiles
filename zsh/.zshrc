if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug ~/.zplug
    source ~/.zplug/init.zsh && zplug update --self
fi

autoload -U compinit && compinit
source ~/.zplug/init.zsh

zplug zplug/zplug
zplug zsh-users/zsh-completions
zplug zsh-users/zsh-syntax-highlighting
zplug zsh-users/zsh-history-substring-search
zplug zsh-users/zsh-autosuggestions
zplug mafredri/zsh-async, from:github
zplug caarlos0/zsh-pg
zplug sindresorhus/pure, use:pure.zsh, from:github, as:theme

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
