:- module(plswipl_low, []).

:- writeln("PL/SWI-Prolog boot code loaded").

do(Code) :-
        open_string(Code, Stream),
        do1(Stream).

do1(Stream) :-
        read_term(Stream, Term,
                  [ syntax_errors(error),
                    module(user) ]),
        (   Term = end_of_file
        ->  true
        ;   call(Term),
            do1(Stream) ).

