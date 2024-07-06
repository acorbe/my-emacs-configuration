#!/usr/bin/env bash

apt install  -y  build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev gcc libpng-dev         libpoppler-dev libpoppler-glib-dev libz-dev make pkg-config
apt install -y  openjdk-17-jdk #  ltex-lsp
apt install  -y  libgnutls libgnutls-28-dev
apt install  -y  libjansson-dev
apt install  -y  librsvg2-dev
apt install  -y  libtree-sitter-dev
apt install  -y  libgccjit0
apt install  -y  libgccjit-12-dev
apt install  -y  texlive latexmk


             

              
              
              
              


# ./configure --with-native-compilation=aot --with-libtree-sitter
echo "emacs configure command"
echo "./configure --with-native-compilation=aot --with-ltree-sitter"
