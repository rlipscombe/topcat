all: topcat

.PHONY: topcat

topcat:
	if [ -f topcat ]; then rm -f topcat; fi;
	./bootstrap
	chmod +x topcat
