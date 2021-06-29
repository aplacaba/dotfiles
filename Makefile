DOTFILES := $(shell pwd)

all: git zsh emacs
.PHONY: git zsh emacs

git:
	stow --dotfiles git

zsh:
	stow --dotfiles zsh

emacs:
	stow --dotfiles emacs
