# Trivial makefile for the calculator scanner/parser.
# Depends on default (built-in) rules for C compilation.

# Note that rule for goal (parse) must be the first one in this file.

CC = gcc

parse: parse.o scan.o
	$(CC) -o parse parse.o scan.o

scantest: scantest.o scan.o
	$(CC) -o scantest scantest.o scan.o

clean:
	rm *.o parse scantest

parse.o: scan.h
scan.o: scan.h
scantest.o: scan.h
