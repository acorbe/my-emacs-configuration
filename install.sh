THIS_DIR=$(pwd)
cd
ln -s $THIS_DIR/literate_version_setup/emacs.el .emacs
mkdir -p .emacs.d
cd .emacs.d
ln -s $THIS_DIR/literate_version_setup/early-init.el
cd $THIS_DIR
