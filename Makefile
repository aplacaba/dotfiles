DOTFILES := $(shell pwd)

all: tmux urxvt vim git zsh fish
.PHONY: tmux urxvt vim git zsh fish

shell:
	stow --dotfiles zsh

tmux:
	stow --dotfiles tmux

urxvt:
	stow --dotfiles urxvt

git:
	stow --dotfiles git

vim:
	stow --dotfiles vim

zsh:
	stow --dotfiles zsh

fish:
	stow fish
