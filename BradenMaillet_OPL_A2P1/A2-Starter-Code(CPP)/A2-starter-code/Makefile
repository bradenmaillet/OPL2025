# Trivial makefile for the calculator scanner/parser.
# Depends on default (built-in) rules for C compilation.

# Note that rule for goal (parse) must be the first one in this file.

CC = g++
LANG = scan.hpp

%.o: %.cpp
	$(CC) -c $<
parse: parse.o scan.o
	$(CC) -o parse parse.o scan.o

scantest: scantest.o scan.o
	$(CC) -o scantest scantest.o scan.o

clean:
	rm *.o parse scantest

parse.o: $(LANG)
scan.o: $(LANG)
scantest.o: $(LANG)
