test-startup.sh: ../test-startup.sh
	cp ../test-startup.sh . 

emacs-%: test-startup.sh
	echo "building for $@"
	docker build -t emacs-testing:$@ --build-arg EMACS_CI=$@ .
