apt install  -y  build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev
apt install  -y  libgnutls
apt install  -y  libjansson-dev
apt install  -y  librsvg2-dev
apt install  -y  libtree-sitter-dev
apt install  -y  libgccjit0
apt install  -y  libgccjit-12-dev


./configure --with-native-compilation=aot --with-libtree-sitter

