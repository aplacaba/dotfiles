#!/bin/env sh

sudo dnf update -y

# ruby deps
echo "Installing ruby deps"
sudo dnf install -y gcc make bzip2 openssl-devel libyaml-devel libffi-devel readline-devel zlib-devel gdbm-devel ncurses-devel

# postgres, redis
echo "Installing postgres, redis, erlang"
sudo dnf install -y postgresql postgresql-devel redis curl git erlang clojure

# vterm, pdf-tools
echo "Installing vterm, pdf-tools"
sudo dnf install -y make automake autoconf gcc gcc-c++ ImageMagick libpng-devel zlib-devel poppler-glib-devel libvterm

echo "Setting up erlang"
sudo dnf install -y erlang erlang-devel

echo "Hack fonts"
sudo dnf copr enable zawertun/hack-fonts
sudo dnf install hack-fonts -f

# Setting asdf
echo "Setting up asdf"
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.0
. $HOME/.asdf/asdf.sh

RUBY_VERSION=2.6.6
NODE_VERSION=14.15.4
EX_VERSION=1.11.3

# ruby
asdf plugin-add ruby https://github.com/asdf-vm/asdf-ruby.git

# nodejs
bash -c '${ASDF_DATA_DIR:=$HOME/.asdf}/plugins/nodejs/bin/import-release-team-keyring'
asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git

# elixir
asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git

asdf install ruby $RUBY_VERSION
asdf install nodejs $NODE_VERSION
asdf install elixir $EX_VERSION

# lein
sudo wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein /usr/bin/lein
sudo chmod a+x /usr/bin/lein
cd $HOME
lein
