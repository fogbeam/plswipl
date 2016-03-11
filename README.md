= PL/SWI-Prolog

Use [SWI-Prolog](http://www.swi-prolog.org/) as a Procedural Language
inside [PostgreSQL](http://www.postgresql.org/)!

== Status

This is still a work in progress. Quering prolog barely works!

== Todo

- Investigate how to convert data from/to the Prolog side correctly.

- Add support for more data types, specifically arrays and json(b?)

- Learn how memory handling works under Pg and take advantage of it

- Add support for SPI

- Add support for doing FDW in Prolog?

== Focus

My aim is getting PL/SWI-Prolog to be correct, flexible
an powerful Procedural Language backend.

At this point, structured, flexible and clear is the most important
thing. Optimizations making the code more complex are not welcomed
yet!

== License

PL/SWI-Prolog is distributed under the MIT license. See the LICENSE file.
