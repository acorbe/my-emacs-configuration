
#!/usr/bin/env bash

# https://stackoverflow.com/a/246128/1714661
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
echo "script dir: $SCRIPT_DIR"

cd
#mv .emacs .emacs.prior_install


echo "The new .emacs file will be:"
echo "-----"
sed "s|GIT_REPO_DIR|$SCRIPT_DIR|g" $SCRIPT_DIR/literate-version-setup/emacs.el
echo "-----"

echo "trying to remove .emacs if exists"
rm .emacs || true

echo "writing custom .emacs"
sed "s|GIT_REPO_DIR|$SCRIPT_DIR|g" $SCRIPT_DIR/literate-version-setup/emacs.el > .emacs
#ln -s $SCRIPT_DIR/literate-version-setup/emacs.el .emacs

echo "removing .emacs.d if exists"
rm -Rf .emacs.d || true

echo "making new .emacs.d"
mkdir -p .emacs.d
cd .emacs.d
echo "removing early-init.el if necessary"
rm early-init.el || true

echo "linking early-init.el"
ln -s $SCRIPT_DIR/literate-version-setup/early-init.el
cd $SCRIPT_DIR
