ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

test-all: test-24-4 test-24-5 test-25-1 test-25-2 test-25-3 test-26-1 test-26-2 test-26-3 test-27-1

test-startup.sh: ../test-startup.sh
	cp ../test-startup.sh . 

emacs-%: Dockerfile
	echo "building for $@"
	docker build -t emacs-testing:$@ --build-arg EMACS_CI=$@ .

run-%: emacs-%
	echo "running emacs-$*"
	docker run --rm -it emacs-testing:emacs-$* /bin/sh 

test-%: emacs-% ../emacs.el
	echo "running emacs-$*"
	docker run --rm -it -v ${ROOT_DIR}/..:/root/repo:ro emacs-testing:emacs-$* /bin/sh -c "cd /root; ./initialize-docker.sh" > $@
#2>&1 | tee $@

clean:
	rm test-??-?
