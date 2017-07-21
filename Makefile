DOTFILES := $(shell pwd)

all: zsh tmux urxvt vim git
.PHONY: zsh tmux urxvt vim git

shell:
	stow zsh

tmux:
	stow tmux

urxvt:
	stow urxvt

git:
	stow git

vim:
	stow vim
