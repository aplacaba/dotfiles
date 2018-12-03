set PATH $HOME/.rbenv/bin $PATH
set PATH $HOME/.rbenv/shims $PATH
set PATH $HOME/.cargo/bin $PATH
rbenv rehash >/dev/null ^&1
eval (python -m virtualfish)
set fish_greeting

set pure_symbol_prompt "λ"
