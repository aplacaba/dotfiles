#!/usr/bin/env bash

# Recently tested on Fedora 41

sudo dnf install -y dnf-plugins-core
sudo dnf-3 config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
sudo dnf update -y
sudo dnf install -y \
     git-core patch make bzip2 libyaml-devel libffi-devel readline \
     readline-devel  zlib zlib-devel gdbm gdbm-devel ncurses-devel \
     automake cmake autoconf gcc gcc-c++ ImageMagick libpng libpng-devel \
     bison sqlite-devel poppler-glib-devel libvterm ripgrep curl git clojure \
     erlang libtool xclip xsel zsh exa util-linux-user inotify-tools stow libtree-sitter libtree-sitter-devel \
     sbcl emacs sudo docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

echo "Setting up asdf"
ASDF_VERSION="v0.14.1"
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch $ASDF_VERSION
. $HOME/.asdf/asdf.sh

RUBY_VERSION=3.3.2
NODE_VERSION=20.18.0

# ruby, node, elixir
asdf plugin-add ruby   https://github.com/asdf-vm/asdf-ruby.git
asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git

asdf install ruby $RUBY_VERSION
asdf install nodejs $NODE_VERSION

touch .tool-versions
echo -n "ruby $RUBY_VERSION" >> .tool-versions
echo -n "nodejs $NODE_VERSION" >> .tool-versions

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
    ;;
    * )
    ;;
esac

make && make install

echo "Setup complete please restart your computer"
