% Means.pl - Prolog implementation of the "Averages/Pythagorean means" task:
% 	https://rosettacode.org/wiki/Averages/Pythagorean_means
% For Programming Assignment 1, COMP3010 - OPL, Spring 2025
% T. Wilkes, January 2025

% To run:
%	prolog Means.pl
% ... then paste the following at the toplevel prompt:
%   testmeans([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0], M).
% Output:
% 	( 5.500000 , 4.528729 , 3.414172 )
% 	true.

% The SWI Prolog library does not define prodlist/2 .
% The definition below is simplistic (i.e., may not cover all corner cases),
% but will suffice for this exercise.
% Base case where the product of empty list is 1.
prodlist([],1).
prodlist([H|T], Product) :-     % recursive case where H is multiplied by product of T.
    prodlist(T, Product1),      % recursively compute product of T of the list.
    Product is H * Product1.    % multiply H with the computed product of T.

% Likewise, we define a predicate to sum the inverses of the elements of a list of floats.
suminvertedlist([],0).              % base case where sum of empty list is 0.
suminvertedlist([H|T], Inverse) :-  % the sum of the reciprocals [H/T] is (1/H) + the sum of reciprocals of T.
    suminvertedlist(T, Inverse1),   % recursively compute sum of reciprocals of T.
    Inverse is (1/H) + Inverse1.    % add reciprocal of H to sum.

% This implementation of the Arithmetic mean is the only one of the Pythagorean means
% available as a Prolog implementation on Rosetta Code:
% 	https://rosettacode.org/wiki/Averages/Arithmetic_mean#Prolog

arithmeticmean(List, A) :-
    length(List, Length),       % get the number of elements of the list.
    sumlist(List, Sum),         % get the sum of the list using sumlist
    A is Sum / Length.          % return is the sum of the list / number of elements

% These implementations of the Geometric and Harmonic means are original code:

geometricmean(List, G) :-
	length(List, Length),       % get the number of elements of the list.
	prodlist(List, Prod),		% see line 17 above for the definition of prodlist/2
	G is Prod ** (1/Length).    % return is Prod ^ (reciprocal of length)

harmonicmean(List, H) :-
	length(List, Length),       % get the number of elements of the list.
	suminvertedlist(List, Sum),	% see line 23 above for the definition of suminvertedlist/2
	H is Length / Sum.          % return number of elements / the result of suminvertedlist/2


% A predicate to try out our definitions.
testmeans(List, M) :-
	arithmeticmean(List, A),    % call functions...
	geometricmean(List, G),
	harmonicmean(List, H),
	format('( ~f ,', A),	    % print arithmetic mean as float.
	format(' ~f ,', G),         % print geometric mean as float.
	format(' ~f )', H).         % print harmonic mean as float.
