\set ECHO all

\connect postgres

drop database if exists test;

create database test;
\connect test;

create extension plswipl;

\echo reloading prolog predicates...
DO $$make.$$ LANGUAGE PLSWIPL;

create function break_chars(in str text) returns setof text as $$"/home/salva/g/pg/plswipl/example.prolog"$$ language PLSWIPL;
select break_chars('hello world!');

create or replace function break_chars(in str text, out c text) returns setof text as $$"/home/salva/g/pg/plswipl/example.prolog"$$ language PLSWIPL;
select break_chars('hello world!');

create function break_pairs(in str text, out a text, out b text) returns record as $$"/home/salva/g/pg/plswipl/example.prolog"$$ language PLSWIPL;
select break_pairs('hello world!');

drop function break_pairs(text);
create function break_pairs(in str text, out a text, out b text) returns setof record as $$"/home/salva/g/pg/plswipl/example.prolog"$$ language PLSWIPL;
select break_pairs('hello world!');
select * from break_pairs('hello world!') order by b offset 3 limit 2;
select break_pairs('hello world!') offset 30 limit 1;

select count(*)
  from ( select distinct bp.a, bp.b
           from pg_proc,
                lateral (select * from break_pairs(proname)) as bp
           order by bp.b) as pairs;

create function factors(in n int) returns int[] as $$"/home/salva/g/pg/plswipl/example.prolog"$$ language plswipl;

select factors(4328476);

create function factors_bad(in n int) returns int[] as $$"/home/salva/g/pg/plswipl/example.prolog"$$ language plswipl;

select factors_bad(4328476);


create function text2term(in n text) returns text as $$"/home/salva/g/pg/plswipl/example.prolog"$$ language plswipl;

select text2term('[foo(34/23), 12]');

\set echo none
