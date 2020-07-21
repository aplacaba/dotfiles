DOTFILES := $(shell pwd)

all: tmux urxvt git zsh emacs
.PHONY: tmux urxvt git zsh emacs


tmux:
	stow --dotfiles tmux

urxvt:
	stow --dotfiles urxvt

git:
	stow --dotfiles git

zsh:
	stow --dotfiles zsh

emacs:
	stow --dotfiles emacs

