# Local Variables:
# mode: makefile
# End:

default:
	just --list

install:
	just emacs git zsh

emacs:
	stow --dotfiles emacs

git:
	stow --dotfiles git

zsh:
	stow --dotfiles zsh
