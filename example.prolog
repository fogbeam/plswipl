
:- module(example, [bar/2, baz/2, prolog_length/2]).

bar(X, Z) :-
        Z is X * 2 .

baz(_, 'bingo!').

break_chars(String, Char) :-
        atom_chars(String, Chars),
        member(Char, Chars).

prolog_length(X, L) :-
        atom_chars(X, A),
        length(A, L).