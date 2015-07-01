[ -e "${HOME}/.zsh_aliases" ] && source "${HOME}/.zsh_aliases"
[ -e "${HOME}/.zshrc_local" ] && source "${HOME}/.zshrc_local"
source ~/.antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundles<<EOBUNDLES
  git
  heroku
  bundler
  npm
  node
  rbenv
  zsh-users/zsh-completions src
  zsh-users/zsh-syntax-highlighting
  command-not-found
  history
  tmux
  lein
EOBUNDLES

antigen theme sorin
antigen apply
