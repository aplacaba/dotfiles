DOTFILES := $(shell pwd)

all: tmux urxvt vim git zsh emacs
.PHONY: tmux urxvt vim git zsh emacs


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


emacs:
	stow --dotfiles emacs

