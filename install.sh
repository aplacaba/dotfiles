#!/usr/bin/env bash

# Recently tested on Fedora 42

sudo dnf install -y dnf-plugins-core
sudo dnf-3 config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
sudo dnf update -y
sudo dnf install -y \
     git-core patch make bzip2 libyaml-devel libffi-devel readline \
     readline-devel zlib zlib-devel gdbm gdbm-devel ncurses-devel \
     automake cmake autoconf gcc gcc-c++ ImageMagick libpng libpng-devel \
     bison sqlite-devel poppler-glib-devel libvterm ripgrep curl git clojure \
     libtool xclip xsel zsh eza util-linux-user inotify-tools stow \
     libtree-sitter libtree-sitter-devel emacs sudo docker-ce docker-ce-cli \
     containerd.io docker-buildx-plugin docker-compose-plugin awscli python3-pip just

# Install homebrew
read -p "Install Homebrew (y/n)? " answer
case ${answer:0:1} in
    y|Y )
        # Install brew and roswell
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        source ~/.zshrc
        brew install roswell
    ;;
    * )
    ;;
esac


# 3rd party software
read -p "Install 3rd party software from flathub (y/n)? " answer
case ${answer:0:1} in
    y|Y )
        echo "Installing 3rd party software"
        flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
        flatpak install -y flathub com.discordapp.Discord
        flatpak install -y flathub org.telegram.desktop
        flatpak install -y flathub com.slack.Slack
        flatpak install -y flathub io.gitlab.librewolf-community
        flatpak install -y flathub io.podman_desktop.PodmanDesktop
        flatpak install -y flathub com.spotify.Client
        flatpak install -y flathub com.google.Chrome
        flatpak install -y flathub com.valvesoftware.Steam
    ;;
    * )
    ;;
esac

make && make install

echo "Setup complete please restart your computer"
