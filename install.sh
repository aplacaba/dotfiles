#!/usr/bin/env bash

os="$(uname -a)"

# For Fedora
if [[ $os == *"fedora"* ]]; then
    echo "Found Fedora. Installing dependencies"

    sudo dnf install -y dnf-plugins-core
    sudo dnf config-manager --add-repo https://rpm.releases.hashicorp.com/fedora/hashicorp.repo
    sudo dnf update -y
    sudo dnf install -y \
         git-core patch make bzip2 libyaml-devel libffi-devel readline \
         readline-devel  zlib zlib-devel gdbm gdbm-devel ncurses-devel \
         automake cmake autoconf gcc gcc-c++ ImageMagick libpng libpng-devel \
         bison sqlite-devel poppler-glib-devel libvterm ripgrep curl git clojure \
         erlang libtool openssl1.1 openssl1.1-devel xclip xsel zsh exa \
         util-linux-user inotify-tools stow emacs libtree-sitter libtree-sitter-devel \
         sbcl terraform

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

    wget -O- https://apt.releases.hashicorp.com/gpg | sudo gpg --dearmor -o /usr/share/keyrings/hashicorp-archive-keyring.gpg
    echo "deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/hashicorp.list

    sudo apt update -y && sudo apt upgrade -y
    sudo apt install git curl libssl-dev libreadline-dev zlib1g-dev autoconf \
         bison build-essential libyaml-dev libreadline-dev libncurses5-dev \
         libffi-dev libgdbm-dev ripgrep make cmake libpng libpng-dev libtool \
         libtool-bin exa xsel xclip libvterm libvterm-dev zsh  stow xset \
         libtree-sitter-dev sbcl build-essential autoconf m4 libwxgtk3.0-gtk3-dev \
         libwxgtk-webview3.0-gtk3-dev libgl1-mesa-dev libglu1-mesa-dev libpng-dev \
         libssh-dev unixodbc-dev xsltproc fop libxml2-utils libncurses-dev openjdk-11-jdk \
         libwxgtk-webview3.0-gtk3-dev terraform

fi

# for wsl ensure docker for windows is up and running
echo "Setting up services"
POSTGRES_VERSION="latest"
REDIS_VERSION="latest"
ASDF_VERSION="0.14.0"

docker volume create pgdata
docker run -d \
     --name postgresql \
     --restart always \
     -v pgdata:/var/lib/postgresql/data \
     -e POSTGRES_USER=postgres \
     -e POSTGRES_HOST_AUTH_METHOD=trust \
     -p 5432:5432 \
     postgres:$POSTGRES_VERSION

docker run -d \
       --name redis \
       --restart always \
       -p 6379:6379 \
       redis:$REDIS_VERSION

echo "Setting up asdf"
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.14.0
. $HOME/.asdf/asdf.sh

RUBY_VERSION=3.2.2
NODE_VERSION=18.17.1
EX_VERSION=1.14.2

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
        echo "Installing 3rd party software"
        flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
        flatpak install -y flathub com.discordapp.Discord
        flatpak install -y flathub us.zoom.Zoom
        flatpak install -y flathub org.telegram.desktop
        flatpak install -y flathub com.slack.Slack
    ;;
    * )
    ;;
esac

make && make install

echo "Setup complete please restart your computer"
