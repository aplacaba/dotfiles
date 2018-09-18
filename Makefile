DOTFILES := $(shell pwd)

all: tmux urxvt vim git fish
.PHONY: tmux urxvt vim git fish

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
