DOTFILES := $(shell pwd)

all: tmux urxvt vim git fish zsh
.PHONY: tmux urxvt vim git fish zsh

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

fish:
	stow fish

zsh:
	stow zsh
