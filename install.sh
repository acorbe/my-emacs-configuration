THIS_DIR=$(pwd)
cd
#mv .emacs .emacs.prior_install
rm .emacs
ln -s $THIS_DIR/literate-version-setup/emacs.el .emacs
mkdir -p .emacs.d
cd .emacs.d
rm early-init.el
ln -s $THIS_DIR/literate-version-setup/early-init.el
cd $THIS_DIR
