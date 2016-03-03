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
  command-not-found
  history
  tmux
  lein
  zsh-users/zsh-completions src
  zsh-users/zsh-syntax-highlighting
  zsh-users/zsh-history-substring-search
EOBUNDLES

[ -e "${HOME}/.zsh_aliases" ] && source "${HOME}/.zsh_aliases"

antigen theme pure
antigen apply
