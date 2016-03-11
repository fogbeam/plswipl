# PL/SWI-Prolog

Use [SWI-Prolog](http://www.swi-prolog.org/) as a Procedural Language
inside [PostgreSQL](http://www.postgresql.org/)!

## Status

This is still a work in progress. It just barely works!

## Installation

    $ make
    $ sudo make install

Note that PostgreSQL development utilities and header files are required.

## Example

See the [example.prolog](./example.prolog) file which contains the following predicate definition:

```prolog
break_chars(String, Char) :-
        atom_chars(String, Chars),
        member(Char, Chars).
```

From `psql` shell as superuser:

    create extension plswipl;
    CREATE EXTENSION
    salva=# create or replace function break_chars(text) returns setof text as $$"/home/salva/g/pg/plswipl/example.prolog"$$ language PLSWIPL;
    CREATE FUNCTION
    salva=# select break_chars('all the letters');
     break_chars
    -------------
     a
     l
     l
      
     t
     h
     e
      
     l
     e
     t
     t
     e
     r
     s
    (15 rows)
    
    salva=# 

## Todo

- Investigate how to convert data from/to the Prolog side correctly.

- Add support for more data types, specifically arrays and json(b?)

- Learn how memory handling works under Pg and take advantage of it

- Add support for SPI

- Add support for doing FDW in Prolog?

- Store prolog modules inside the database.

- Add trusted version for non superusers.

- Learn how a procedural language interactuates with having more than
  one schema or changing the session user.

- Fix this alarm exceptions thing.

## Focus

My aim is getting PL/SWI-Prolog to be correct, flexible
an powerful Procedural Language backend.

At this point, structured, flexible and clear is the most important
thing. Optimizations making the code more complex are not welcomed
yet!

## License

PL/SWI-Prolog is distributed under the MIT license. See the LICENSE file.
