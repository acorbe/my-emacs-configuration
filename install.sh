
#!/usr/bin/env bash

# THIS_DIR=$(pwd)
# https://stackoverflow.com/a/246128/1714661
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd
#mv .emacs .emacs.prior_install
rm .emacs
ln -s $SCRIPT_DIR/literate-version-setup/emacs.el .emacs
mkdir -p .emacs.d
cd .emacs.d
rm early-init.el
ln -s $SCRIPT_DIR/literate-version-setup/early-init.el
cd $SCRIPT_DIR
