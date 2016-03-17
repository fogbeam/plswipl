
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

break_pairs(String, A, B) :-
        atom_chars(String, Chars),
        eat_pairs(Chars, A, B).

eat_pairs([A, B |_], A, B).
eat_pairs([_|T], A, B) :-
        eat_pairs(T, A, B).

prolog_spi_execute(Text, N, R) :-
        spi:connect,
        spi:execute(Text, false, 0, R),
        spi:processed(N),
        spi:finish.

factors_bad(_I, [foo(34/21)]).

text2term(Text, Term) :-
        read_term_from_atom(Text, Term, []).


factors(I, F) :-
        I >= 1,
        factors(I, 2, F).

factors(I, N, F) :-
        (   I =< N
        ->  F = [I]
        ;   (   0 is I mod N
            ->  (   F = [N|F1],
                    I1 is I / N,
                    factors(I1, N, F1) )
            ;   N1 is N + 1,
                factors(I, N1, F))).
