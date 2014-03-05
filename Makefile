all: topcat

.PHONY: topcat

topcat:
	-rm topcat
	./bootstrap
	chmod +x topcat
