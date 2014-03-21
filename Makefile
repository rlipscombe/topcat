all: topcat

.PHONY: topcat

topcat:
	if [ -f topcat ]; then rm -f topcat; fi;
	./bootstrap
	chmod +x topcat

test:
	-mkdir -p .eunit
	erlc -D TEST -o .eunit src/topcat_args.erl 
	erl -noshell -noinput -pa .eunit \
		-eval "eunit:test(topcat_args, [verbose])" -s init stop
