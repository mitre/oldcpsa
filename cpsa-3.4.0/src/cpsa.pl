% -*- mode: prolog -*-

%% CPSA tools in Prolog

%% This module contains a Prolog implementation of the CPSA specific
%% S-expression pretty printer algorithm used in the Haskell program.
%% It also contains a translator between an S-expression and an
%% internal representation that eases transformations on CPSA data
%% structures.

%% Known to work in SWI-Prolog, but not with GNU Prolog.

%% Copyright (c) 2009 The MITRE Corporation
%%
%% This program is free software: you can redistribute it and/or
%% modify it under the terms of the BSD License as published by the
%% University of California.

:- module(cpsa, [cpsa_sexprs_pp/2, cpsa/2, sexprs_to_cpsas/2,
                 cpsas_to_sexprs/2]).

:- use_module(pp).
:- use_module(sexpr).

%% PRETTY PRINTER

%% cpsa_sexprs_pp(+Out, +Sexprs)
%% Pretty print a list of S-expressions to stream Out.
cpsa_sexprs_pp(_, []).
cpsa_sexprs_pp(Out, [Sexpr|Sexprs]) :-
	cpsa_sexpr_pp(Out, Sexpr),
	nl(Out),
	cpsa_sexprs_pp(Out, Sexprs).

cpsa_sexpr_pp(Out, Sexpr) :-
	cpsa_sexpr_to_pretty(Sexpr, Pretty),
	pp:pr(Out, 72, Pretty),
	nl(Out).

%% Create a pretty printing tree for each top-level CPSA form.
cpsa_sexpr_to_pretty([comment|X], Pretty) :-
	sexpr:sexpr_to_pretty([comment|X], Pretty).
cpsa_sexpr_to_pretty([herald|X], Pretty) :-
	sexpr:sexpr_to_pretty([herald|X], Pretty).
cpsa_sexpr_to_pretty([defskeleton|X], Pretty) :-
	sexpr_to_cpsa_pretty([defskeleton|X], Pretty).
cpsa_sexpr_to_pretty([defprotocol, Name, Alg|Roles], Pretty) :-
	pp:atm(defprotocol, X),
	pp:atm(Name, Y),
	pp:atm(Alg, Z),
	pp:atm('(', P),
	pp:atm(' ', S),
	sexpr_to_role_pretty(Roles, Pretties),
	pp:grp(2, [P, X, S, Y, S, Z, S|Pretties], Pretty).

%% CPSA role specific pretty printing support.
sexpr_to_role_pretty([[defrole|X]|Roles], [Space, Pretty|Pretties]) :-
	!,
	pp:brk(1, Space),
	sexpr_to_cpsa_pretty([defrole|X], Pretty),
	sexpr_to_role_pretty(Roles, Pretties).
sexpr_to_role_pretty(Sexprs, Pretties) :-
	sexprs_to_pretty(Sexprs, Pretties).

%% Creates a group in which breaks are allowed only before lists.
sexpr_to_cpsa_pretty(Thing, Pretty) :-
	atomic(Thing),
	!,
	sexpr:atom_to_pretty(Thing, Pretty).
sexpr_to_cpsa_pretty([Sexpr|Sexprs], Pretty) :-
	pp:atm('(', First),
	sexpr:sexpr_to_pretty(Sexpr, Second),
	sexprs_to_cpsa_pretty(Sexprs, Rest),
	pp:grp(2, [First, Second|Rest], Pretty).

sexprs_to_cpsa_pretty([], [Pretty]) :-
	pp:atm(')', Pretty).
sexprs_to_cpsa_pretty([Sexpr|Sexprs], [Space, Pretty|Pretties]) :-
	atomic(Sexpr),
	!,
	pp:atm(' ', Space),
	sexpr:sexpr_to_pretty(Sexpr, Pretty),
	sexprs_to_cpsa_pretty(Sexprs, Pretties).
sexprs_to_cpsa_pretty([Sexpr|Sexprs], [Space, Pretty|Pretties]) :-
	pp:brk(1, Space),
	sexpr:sexpr_to_pretty(Sexpr, Pretty),
	sexprs_to_cpsa_pretty(Sexprs, Pretties).

%% S-EXPRESSION TO INTERNAL FORM TRANSLATION

%% Programmer's hint--test translations with the query:
%%   cpsa:cpsa('../tst/blanchet.scm',A),
%%   cpsa:cpsas_to_sexprs(A, B),
%%   current_output(Out),
%%   cpsa:cpsa_sexprs_pp(Out, B).

%% Translate the contents of a file into the internal form
cpsa(File, Cpsas) :-
	sexpr:read_sexpr_list(File, Sexprs),
	sexprs_to_cpsas(Sexprs, Cpsas).

sexprs_to_cpsas([], []).
sexprs_to_cpsas([Sexpr|Sexprs], [Cpsa|Cpsas]) :-
	sexpr_to_cpsa(Sexpr, Cpsa),
	sexprs_to_cpsas(Sexprs, Cpsas).

%% Internal Form

%% There are four top-level forms in CPSA, comment, herald,
%% defprotocol, and defskeleton.
%%
%% Top ::= c(Comment)             -- Comment
%%      |  h(Name, Alist)         -- Herald
%%      |  p(Name, Alg, Roles)    -- Defprotocol
%%      |  k(Prot, Decls, Strands, Precedes, Nons, Uniqs).
%%                                -- Defskeleton
%% Role ::= r(Name, Decls, Trace, Nons, Uniqs).
%%                                -- Defrole
%% Decl ::= d(Sort, Vars).
%%
%% Trace ::= [Event].
%%
%% Event ::= send(Term) | recv(Term).
%%
%% Nons ::= [Term].
%%
%% Uniqs ::= [Term].
%%
%% Strand ::= s(RoleName, Height, Map).
%%
%% Height ::= Int.
%%
%% Map ::= [Maplet].
%%
%% Maplet ::= (Var, Term).
%%
%% Precede ::= ((Int, Int), (Int, Int)).
%%
%% Term ::= Var | Tag | invk(Term) | pubk(Term) | privk(Term)
%%       |  pubk(Term, Term) | privk(Term, Term) | ltk(Term, Term)
%%       |  cat(Term, Term) | enc(Term, Term) | hash(Term).
%%
%% Var ::= Atom.
%%
%% Tag ::= String.

sexpr_to_cpsa([comment|X], c(X)).
sexpr_to_cpsa([herald, Name|Alist], h(Name, Alist)).
sexpr_to_cpsa([defprotocol, Name, Alg|Rest],
	p(Name, Alg, Roles)) :-
	atom(Name), atom(Alg),
	sexpr_to_roles(Rest, Roles).
sexpr_to_cpsa([defskeleton, Prot, [vars|Vars]|Rest],
	k(Prot, Decls, Strands, Precedes, Nons, Uniqs)) :-
	atom(Prot),
	decls(Vars, Decls),
	sexpr_to_strands(Rest, Strands, Alist),
	lookup(precedes, Alist, Values),
	edges(Values, Precedes),
	lookup('non-orig', Alist, Snons),
	sexprs_to_terms(Snons, Nons),
	lookup('uniq-orig', Alist, Suniqs),
	sexprs_to_terms(Suniqs, Uniqs).

sexpr_to_roles([[defrole|Sexpr]|Rest], [Role|Roles]) :-
	!,
	sexpr_to_role(Sexpr, Role),
	sexpr_to_roles(Rest, Roles).
sexpr_to_roles(_, []).

sexpr_to_role([Name, [vars|Vars], [trace|Strace]|Alist],
	r(Name, Decls, Trace, Nons, Uniqs)) :-
	decls(Vars, Decls),
	sexprs_to_trace(Strace, Trace),
	lookup('non-orig', Alist, Snons),
	sexprs_to_terms(Snons, Nons),
	lookup('uniq-orig', Alist, Suniqs),
	sexprs_to_terms(Suniqs, Uniqs).

sexprs_to_trace([], []).
sexprs_to_trace([[send, Sexpr]|Strace], [send(Term)|Trace]) :-
	sexpr_to_term(Sexpr, Term),
	sexprs_to_trace(Strace, Trace).
sexprs_to_trace([[recv, Sexpr]|Strace], [recv(Term)|Trace]) :-
	sexpr_to_term(Sexpr, Term),
	sexprs_to_trace(Strace, Trace).

sexprs_to_terms([], []).
sexprs_to_terms([Sterm|Sterms], [Term|Terms]) :-
	sexpr_to_term(Sterm, Term),
	sexprs_to_terms(Sterms, Terms).

sexpr_to_term(Term, Term) :-
	atom(Term).
sexpr_to_term(Term, Term) :-
	string(Term).
sexpr_to_term([cat, Sterm], Term) :-
	sexpr_to_term(Sterm, Term).
sexpr_to_term([cat, Sterm1, Sterm2|Sterms], cat(Term1, Term)) :-
	sexpr_to_term(Sterm1, Term1),
	sexpr_to_term([cat, Sterm2|Sterms], Term).
sexpr_to_term([hash|Sterms], hash(Term)) :-
	sexpr_to_term([cat|Sterms], Term).
sexpr_to_term([enc|Sterms], enc(Term, Key)) :-
	split(Sterms, Cats, Sexpr),
	sexpr_to_term(Sexpr, Key),
	sexpr_to_term([cat|Cats], Term).
sexpr_to_term([privk|Sterms], invk(Term)) :-
	sexpr_to_term([pubk|Sterms], Term).
sexpr_to_term([Fun|Sterms], Term) :-
	atom(Fun),
	Fun \= cat,
	Fun \= hash,
	Fun \= enc,
	Fun \= privk,
	sexprs_to_terms(Sterms, Terms),
	Term =.. [Fun|Terms].

split([X, Y], [X], Y).
split([X, Y|Z], [X|U], V) :-
	split([Y|Z], U, V).

sexpr_to_strands([[defstrand|Sexpr]|Rest], [Strand|Strands], Alist) :-
	!,
	sexpr_to_strand([defstrand|Sexpr], Strand),
	sexpr_to_strands(Rest, Strands, Alist).
sexpr_to_strands([[deflistener|Sexpr]|Rest], [Strand|Strands], Alist) :-
	!,
	sexpr_to_strand([deflistener|Sexpr], Strand),
	sexpr_to_strands(Rest, Strands, Alist).
sexpr_to_strands(Alist, [], Alist).

sexpr_to_strand([defstrand, Role, Height|Sexprs], s(Role, Height, Map)) :-
	atom(Role),
	integer(Height),
	sexprs_to_map(Sexprs, Map).
sexpr_to_strand([deflistener, Sexpr], l(Term)) :-
	sexpr_to_term(Sexpr, Term).

decls([], []).
decls([Sexpr|Sexprs], [Decl|Decls]) :-
	decl(Sexpr, Decl),
	decls(Sexprs, Decls).

decl(Sexprs, d(Type, Vars)) :-
	decl_vars(Sexprs, Type, Vars).

decl_vars([Type], Type, []) :-
	!,
	atom(Type).
decl_vars([Var|Decl], Type, [Var|Vars]) :-
	atom(Var),
	decl_vars(Decl, Type, Vars).

sexprs_to_map([], []).
sexprs_to_map([[Key, Svalue]|Sexprs], [(Key, Value)|Map]) :-
	sexpr_to_term(Svalue, Value),
	sexprs_to_map(Sexprs, Map).

lookup(_, [], []).
lookup(Key, [[Key|Value]|Alist], Values) :-
	!,
	append(Value, Rest, Values),
	lookup(Key, Alist, Rest).
lookup(Key, [[_|_]|Alist], Values) :-
	lookup(Key, Alist, Values).

edges([], []).
edges([Sexpr|Sexprs], [Edge|Edges]) :-
	edge(Sexpr, Edge),
	edges(Sexprs, Edges).

edge([S0, S1], (N0, N1)) :-
	node(S0, N0),
	node(S1, N1).

node([S, P], (S, P)) :-
	integer(S),
	integer(P).

%% INTERNAL FORM TO S-EXPRESSION TRANSLATION

cpsas_to_sexprs([], []).
cpsas_to_sexprs([Cpsa|Cpsas], [Sexpr|Sexprs]) :-
	cpsa_to_sexpr(Cpsa, Sexpr),
	cpsas_to_sexprs(Cpsas, Sexprs).

cpsa_to_sexpr(c(X), [comment|X]).
cpsa_to_sexpr(h(Name, Alist), [herald, Name|Alist]).
cpsa_to_sexpr(p(Name, Alg, Roles), [defprotocol, Name, Alg|Rest]) :-
	roles_to_sexprs(Roles, Rest).
cpsa_to_sexpr(k(Prot, Decls, Strands, Precedes, Nons, Uniqs),
	[defskeleton, Prot, [vars|Vars]|Rest]) :-
	decls(Vars, Decls),
	preds(Precedes, Nons, Uniqs, Alist),
	strands_to_sexprs(Strands, Alist, Rest).

preds([], Nons, Uniqs, Alist) :-
	!,
	origs(Nons, Uniqs, Alist).
preds(Precedes, Nons, Uniqs, [[precedes|Sexprs]|Alist]) :-
	edges(Sexprs, Precedes),
	origs(Nons, Uniqs, Alist).

strands_to_sexprs([], Alist, Alist).
strands_to_sexprs([Strand|Strands], Alist, [Sexpr|Sexprs]) :-
	strand_to_sexpr(Strand, Sexpr),
	strands_to_sexprs(Strands, Alist, Sexprs).

strand_to_sexpr(s(Role, Height, Map), [defstrand, Role, Height|Sexprs]) :-
	map_to_sexprs(Map, Sexprs).
strand_to_sexpr(l(Term), [deflistener, Sexpr]) :-
	term_to_sexpr(Term, Sexpr).

map_to_sexprs([], []).
map_to_sexprs([(Key, Value)|Map], [[Key, Svalue]|Sexprs]) :-
	term_to_sexpr(Value, Svalue),
	map_to_sexprs(Map, Sexprs).

roles_to_sexprs([], []).
roles_to_sexprs([Role|Roles], [Sexpr|Sexprs]) :-
	role_to_sexpr(Role, Sexpr),
	roles_to_sexprs(Roles, Sexprs).

role_to_sexpr(r(Name, Decls, Trace, Nons, Uniqs),
	[defrole, Name, [vars|Vars], [trace|Strace]|Alist]) :-
	decls(Vars, Decls),
	traces_to_sexprs(Trace, Strace),
	origs(Nons, Uniqs, Alist).

traces_to_sexprs([], []).
traces_to_sexprs([send(Term)|Trace], [[send, Sterm]|Straces]) :-
	term_to_sexpr(Term, Sterm),
	traces_to_sexprs(Trace, Straces).
traces_to_sexprs([recv(Term)|Trace], [[recv, Sterm]|Straces]) :-
	term_to_sexpr(Term, Sterm),
	traces_to_sexprs(Trace, Straces).

origs([], Uniqs, Alist) :-
	!,
	uniq_origs(Uniqs, Alist).
origs(Nons, Uniqs, [['non-orig'|Snons]|Alist]) :-
	terms_to_sexprs(Nons, Snons),
	uniq_origs(Uniqs, Alist).

uniq_origs([], []) :- !.
uniq_origs(Uniqs, [['uniq-orig'|Suniqs]]) :-
	terms_to_sexprs(Uniqs, Suniqs).

terms_to_sexprs([], []).
terms_to_sexprs([Term|Terms], [Sexpr|Sexprs]) :-
	term_to_sexpr(Term, Sexpr),
	terms_to_sexprs(Terms, Sexprs).

term_to_sexpr(Term, Term) :-
	atom(Term).
term_to_sexpr(Term, Term) :-
	string(Term).
term_to_sexpr(cat(Term1, Term2), [cat, Sterm1|Sterms]) :-
	term_to_sexpr(Term1, Sterm1),
	term_to_sexprs(Term2, Sterms).
term_to_sexpr(hash(Term), [hash|Sterms]) :-
	term_to_sexprs(Term, Sterms).
term_to_sexpr(enc(Term, Key), [enc|All]) :-
	term_to_sexpr(Key, Sexpr),
	term_to_sexprs(Term, Sterms),
	split(All, Sterms, Sexpr).
term_to_sexpr(invk(Term), [privk|Sterms]) :-
	Term =.. [pubk|Args],
	!,
	terms_to_sexprs(Args, Sterms).
term_to_sexpr(Term, [Fun, Sterm|Sterms]) :-
	Term =.. [Fun, Arg|Args],
	Fun \= cat,
	Fun \= hash,
	Fun \= enc,
	term_to_sexpr(Arg, Sterm),
	terms_to_sexprs(Args, Sterms).

term_to_sexprs(cat(Term1, Term2), [Sterm1|Sterms]) :-
	!,
	term_to_sexpr(Term1, Sterm1),
	term_to_sexprs(Term2, Sterms).
term_to_sexprs(Term, [Sterm]) :-
	term_to_sexpr(Term, Sterm).
