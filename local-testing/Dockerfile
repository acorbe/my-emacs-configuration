FROM nixos/nix
ARG EMACS_CI


RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

#RUN nix-build -A pythonFull '<nixpkgs>'

RUN nix-env -iA cachix -f https://cachix.org/api/v1/install
RUN cachix use emacs-ci

ADD test-startup.sh /root/test-startup.sh
RUN nix-env -iA $EMACS_CI -f https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz
