FROM nixos/nix
ARG EMACS_CI


RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

#RUN nix-build -A pythonFull '<nixpkgs>'

RUN nix-env -iA cachix -f https://cachix.org/api/v1/install
RUN cachix use emacs-ci

RUN nix-env -iA $EMACS_CI -f https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz
ADD initialize-docker.sh /root/initialize-docker.sh
#test-startup.sh /root/test-startup.sh
ENV TRAVIS 1
