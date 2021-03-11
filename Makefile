DOTFILES := $(shell pwd)

all: git bash emacs
.PHONY: git bash emacs

git:
	stow --dotfiles git

bash:
	stow --dotfiles bash

emacs:
	stow --dotfiles emacs
