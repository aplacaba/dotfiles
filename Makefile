DOTFILES := $(shell pwd)

all: tmux urxvt vim git zsh fish emacs
.PHONY: tmux urxvt vim git zsh fish emacs


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

