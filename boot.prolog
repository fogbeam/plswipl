:- module(plswipl_low, []).

:- writeln("PL/SWI-Prolog boot code loaded").

handle_do(Code) :-
    open_string(Code, Stream),
    handle_do1(Stream).

handle_do1(Stream) :-
    read_term(Stream, Term,
              [ syntax_errors(error),
                module(user) ]),
    (   Term = end_of_file
     ->  true
     ;   call(Term),
         handle_do1(Stream) ).

handle_function(Call, ModuleString) :-
    functor(Call, Name, Arity),
    read_term_from_atom(ModuleString, Module, [syntax_error(error)]),
    writeln(handle_function(call/Call, module/Module)),
    use_module(Module, [Name/Arity]),
    call(Call),
    writeln(call(Call)).
