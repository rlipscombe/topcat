all: topcat

.PHONY: topcat

topcat:
	if [ -f topcat ]; then rm -f topcat; fi;
	cp src/topcat.app.src ebin/topcat.app
	./bootstrap
	chmod +x topcat
