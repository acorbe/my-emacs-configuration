## from: http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
## still not working

gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40
gpg --homedir ~/.emacs.d/elpa/gnupg \
          --quick-set-expire 474F05837FBDEF9B 1y
