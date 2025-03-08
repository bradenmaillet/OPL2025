Group 8
3/7/25
OPL

Description:
	The following programs implements syntax error recovery with the parser of the ECL. It does so primarily through C++ exceptions. If an unexpected token is detected in any of function an exception will be thrown and caught. The input token will be then referenced against the "first set" and re run if a match is found. The program then checks the "follow set" for any matches and if found will continue as normal. If neither of these sets are matched with the input token then the input token will be skipped and the syntax will continue to be parsed. There are "DEBUG:" statements within the output of the program to help enunciate this.
	
P.S.

the following can be compiled in a Linux terminal with:

make

the following program can be run in a Linux terminal with:

./parse < <fileNameHere>.cl