x#!/usr/bin/env bash

apt install  -y  build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev gcc libpng-dev         libpoppler-dev libpoppler-glib-dev libz-dev make pkg-config

apt install -y  openjdk-17-jdk #  ltex-lsp

apt install  -y  libgnutls libgnutls-28-dev

apt install  -y  libjansson-dev # accelerated lsp
apt install  -y  librsvg2-dev
apt install  -y  libtree-sitter-dev 
apt install  -y  libgccjit0
apt install  -y  libgccjit-12-dev
apt install  -y  texlive latexmk
apt install  -y  libxaw7-dev
apt install  -y  xaw3dg-dev 

apt install -y git autoconf make texinfo gnutls-bin libgccjit-12-dev gcc libgtk2.0-dev libgnutls28-dev libxpm-dev libgif-dev


apt install -y fonts-roboto fonts-firacode # fonts e.g. for nano emacs


             

              
              
              
              


# ./configure --with-native-compilation=aot --with-libtree-sitter
echo "emacs configure command"
echo "./configure --with-native-compilation=aot --with-ltree-sitter"
