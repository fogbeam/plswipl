:- module(plswipl_low, []).

:- writeln("PL/SWI-Prolog boot code loaded").

do(Code) :-
        writeln(code=Code),
        open_string(Code, Stream),
        do1(Stream).

do1(Stream) :-
        writeln(reading),
        read_term(Stream, Term,
                  [ syntax_errors(fail),
                    module(user) ]),
        writeln(term=Term),
        (   Term = end_of_file
        ->  writeln(end_of_file),
            true
        ;   writeln(running=Term),
            call(Term),
            do1(Stream) ).

