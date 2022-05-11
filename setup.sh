#!/usr/bin/env bash

os="$(uname -a)"

# For Fedora
if [[ $os == *"fedora"* ]]; then
    echo "Found Fedora"

    sudo dnf update -y

    sudo dnf install -y \
     git-core patch make bzip2 libyaml-devel libffi-devel readline readline-devel zlib zlib-devel \
     gdbm gdbm-devel ncurses-devel automake cmake autoconf gcc gcc-c++ ImageMagick libpng libpng-devel \
     bison sqlite-devel poppler-glib-devel libvterm ripgrep curl git clojure erlang docker libtool \
     openssl1.1 openssl1.1-devel xclip xsel zsh exa util-linux-user inotify-tools stow emacs

    echo "setup docker"
    sudo systemctl start docker

    # keybindings
    echo "setup keybindings"
    gsettings set org.gnome.desktop.wm.preferences num-workspaces 9
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['<Super>1']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['<Super>2']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['<Super>3']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['<Super>4']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-5 "['<Super>5']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-6 "['<Super>6']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-7 "['<Super>7']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-8 "['<Super>8']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-9 "['<Super>9']"
    gsettings set org.gnome.shell.keybindings switch-to-application-1 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-2 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-3 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-4 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-5 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-6 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-7 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-8 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-9 "[]"
fi

# For WSL
if [[ $os == *"microsoft"* ]]; then
    echo "Found WSL"

    sudo apt update -y && sudo apt upgrage -y
    sudo apt install git curl libssl-dev libreadline-dev zlib1g-dev autoconf bison build-essential \
         libyaml-dev libreadline-dev libncurses5-dev libffi-dev libgdbm-dev ripgrep make cmake \
         libpng libpng-dev libtool libtool-bin exa xsel xclip libvterm libvterm-dev zsh emacs stow
fi


# for wsl ensure docker for windows is up and running
echo "Setting up services"
POSTGRES_VERSION=14
MYSQL_LEGACY=5.7
REDIS_VERSION=6.2

sudo docker volume create pgdata
sudo docker run -d \
     --name postgresql \
     --restart always \
     -v pgdata:/var/lib/postgresql/data \
     -e POSTGRES_USER=postgres \
     -e POSTGRES_PASSWORD=password \
     postgres:$POSTGRES_VERSION

sudo docker volume create mysql_data
sudo docker run -d \
     --name mysql-legacy \
     --restart always \
     -v mysql_data:/var/lib/mysql \
     -e MYSQL_ROOT_PASSWORD=secret \
     -e MYSQL_USER=admin \
     -e MYSQL_PASSWORD=admin \
     mysql:$MYSQL_LEGACY

sudo docker run -d --restart always redis:$REDIS_VERSION

echo "Setting up asdf"
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.9.0
. $HOME/.asdf/asdf.sh

RUBY_VERSION=2.6.9
NODE_VERSION=16.14.2
EX_VERSION=1.13.4

# ruby, node, elixir
asdf plugin-add ruby   https://github.com/asdf-vm/asdf-ruby.git
asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git
bash -c '${ASDF_DATA_DIR:=$HOME/.asdf}/plugins/nodejs/bin/import-release-team-keyring'

asdf install ruby $RUBY_VERSION
asdf install nodejs $NODE_VERSION
asdf install elixir $EX_VERSION

# 3rd party software
read -p "Install 3rd party software from flathub (y/n)? " answer
case ${answer:0:1} in
    y|Y )
        echo "Installing 3rd party libraries"
        flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
        flatpak install -y flathub com.discordapp.Discord
        flatpak install -y flathub com.spotify.Client
        flatpak install -y flathub us.zoom.Zoom
        flatpak install -y flathub org.telegram.desktop
        flatpak install -y flathub com.slack.Slack
    ;;
    * )
    ;;
esac



echo "Setup complete please restart your computer"
