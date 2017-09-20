% -*- mode: prolog -*-

%% Converts lists of character codes to Prolog style S-Expressions and
%% generates pretty printer trees for S-expressions for use with the
%% exports of the pretty printer module pp.

%% Known to work in SWI-Prolog, but not with GNU Prolog.  The program
%% makes use of the non-standard atomic data type string to represent
%% strings in S-expressions.  Atoms and integers in S-expressions are
%% mapped to Prolog standard atoms and numbers.

%% Copyright (c) 2009 The MITRE Corporation
%%
%% This program is free software: you can redistribute it and/or
%% modify it under the terms of the BSD License as published by the
%% University of California.

:- module(sexpr, [codes_sexpr/2, codes_sexpr_list/2, sexpr_term/2,
	  read_sexpr_list/2, read_sexpr_term_list/2, atom_to_pretty/2,
	  sexpr_to_pretty/2, sexprs_to_pretty/2,
	  sexpr_to_group_pretty/2]).

:- use_module(pp).

% codes_sexpr(+Codes, -Sexpr)

% Convert a list of character codes to a Prolog style S-Expression.

codes_sexpr(Codes, Sexpr) :-
	phrase(sexpr_top(Sexpr), Codes).

sexpr_top(S) -->
	sexpr(S),
	sexpr_white_spaces.

% codes_sexpr_list(+Codes, -Sexprs)

% Convert a list of character codes to a Prolog style list of
% S-Expressions.

codes_sexpr_list(Codes, Sexprs) :-
	phrase(sexpr_list_top(Sexprs), Codes).

sexpr_list_top(S) -->
	sexpr_list(S),
	sexpr_white_spaces.

% Supporting predicates follow.

% Parse an S-Expression.

sexpr(S) -->
	sexpr_white_space,
	!,
	sexpr(S).

sexpr(S) -->
	sexpr_open,
	!,
	sexpr_list(S),
	sexpr_close.

sexpr(_) -->
	sexpr_close,
	!,
	{ fail }.

sexpr(S) -->
	sexpr_digit(C),
	!,
	sexpr_number(C, S).

sexpr(S) -->
	sexpr_string_start,
	!,
	sexpr_string(S).

sexpr(S) -->
	sexpr_symbol(S).

% Parse a list minus the opening parenthesis.

sexpr_list(L) -->
	sexpr_white_space,
	!,
	sexpr_list(L).

sexpr_list([S|L]) -->
	sexpr(S),
	!,
	sexpr_list(L).

sexpr_list([]) -->
	[].

% Parse a number.

sexpr_number(C, S) -->
	sexpr_digits(L),
	{ number_codes(S, [C|L]) }.

sexpr_digits([]) -->
	sexpr_atom_end,
	!.

sexpr_digits([C|L]) -->
	sexpr_digit(C),
	sexpr_digits(L).

% Parse a string minus the opening quote.

sexpr_string(S) -->
	sexpr_string_rest(L),
	{ string_to_list(S, L) }.

sexpr_string_rest(L) -->
	sexpr_string_start,
	!,
	{ L = [] }.

sexpr_string_rest([C|L]) -->
	[C],
	sexpr_string_rest(L).

sexpr_string_start -->
	sexpr_quote.

% Symbols

sexpr_symbol(S) -->
	sexpr_symbol_code(C),
	sexpr_symbol_codes(L),
	{ atom_codes(S, [C|L]) }.

sexpr_symbol_codes([C|L]) -->
	sexpr_symbol_code(C),
	!,
	sexpr_symbol_codes(L).

sexpr_symbol_codes([]) -->
	sexpr_atom_end,
	!.

sexpr_symbol_code(C) -->
	sexpr_other(C).

% Character recognizers.

sexpr_white_space -->
	[C],
	{ sexpr_token(C, space) }.

sexpr_white_space -->
	sexpr_semicolon,
	!,
	sexpr_comment.

sexpr_semicolon -->
	[C],
	{ sexpr_token(C, semicolon) }.

sexpr_comment -->
	sexpr_comment_char,
	!,
	sexpr_comment.

sexpr_comment -->
	[].

sexpr_comment_char -->
	[C],
	{ \+ code_type(C, newline) }.

sexpr_open -->
	[C],
	{ sexpr_token(C, open) }.

sexpr_close -->
	[C],
	{ sexpr_token(C, close) }.

sexpr_digit(C) -->
	[C],
	{ code_type(C, digit) }.

sexpr_quote -->
	[C],
	{ sexpr_token(C, quote) }.

sexpr_other(C) -->
	[C],
	{ sexpr_token_other(C) }.

sexpr_token_other(C) :-
	sexpr_token(C, _),
	!,
	fail.

sexpr_token_other(_).

% Atomic S-Expressions end with EOF, white space, or a parenthesis.

sexpr_atom_end([], []) :-
	!.

sexpr_atom_end([C|L], [C|L]) :-
	sexpr_token(C, space),
	!.

sexpr_atom_end([C|L], [C|L]) :-
	sexpr_token(C, open),
	!.

sexpr_atom_end([C|L], [C|L]) :-
	sexpr_token(C, close).

% S-Expression character classes.

sexpr_token(Code, space) :-
	code_type(Code, space),
	!.

sexpr_token(Code, open) :-
	char_code('(', Code),
	!.

sexpr_token(Code, close) :-
	char_code(')', Code),
	!.

sexpr_token(Code, semicolon) :-
	char_code(';', Code),
	!.

sexpr_token(Code, quote) :-
	char_code('"', Code).

% Check for trailing white space in a list of character codes.

sexpr_white_spaces -->
	sexpr_white_space,
	!,
	sexpr_white_spaces.

sexpr_white_spaces -->
	[].

% Other predicates used in conjunction with the parser.

% sexpr_term(+Sexpr, -Term)

% Convert S-Expression into a more convenient form of term.  When the
% first element of a list is an atom, convert it to a function symbol
% applied to the remaining terms.

sexpr_term(A, A) :-
	atomic(A),
	!.

sexpr_term([F|L], T) :-
	atom(F),
	!,
	sexpr_term_list(L, S),
	T =.. [F|S].

sexpr_term(L, T) :-
	sexpr_term_list(L, T).

% sexpr_term_list(+Sexprs, -Terms)

sexpr_term_list(L, T) :-
	maplist(sexpr_term, L, T).

% --------------I/O--------------

% read_file(+Name, -Codes)

read_file(Name, Codes) :-
	open(Name, read, Stream),
	get_codes(Stream, Codes).

% Get characters from a stream and return them as a list.

get_codes(Stream, []) :-
	at_end_of_stream(Stream),
	!,
	close(Stream).

get_codes(Stream, [Code|Codes]) :-
	get_code(Stream, Code),
	get_codes(Stream, Codes).

% read_sexpr_list(+Name, -Sexprs)

% Intrepret the character codes from a file as a list of S-Expressions.

%% read_sexpr_list(+Name, -Sexprs)
read_sexpr_list(Name, Sexprs) :-
	read_file(Name, Codes),
	codes_sexpr_list(Codes, Sexprs).

% read_sexpr_term_list(+Name, -Terms)

% Intrepret the character codes from a file as a list of S-Expressions
% in which lists that start with an atom have been converted to a
% function application.

%% read_sexpr_list(+Name, -Terms)
read_sexpr_term_list(Name, Terms) :-
	read_sexpr_list(Name, Sexprs),
	sexpr_term_list(Sexprs, Terms).

%% Generates pretty printer trees for S-expressions.

%% atom_to_pretty(+Sexpr, -Pretty)
atom_to_pretty([], Pretty) :-
	!,
	pp:atm('()', Pretty).
atom_to_pretty(Thing, Pretty) :-
	string(Thing),
	!,
	atom_codes('"', Quote),
	string_to_list(Thing, Codes),
	append(Quote, Codes, X),
	append(X, Quote, String),
	pp:atm(String, Pretty).
atom_to_pretty(Thing, Pretty) :-
	atomic(Thing),
	pp:atm(Thing, Pretty).

%% Creates blocks for all lists
%% sexpr_to_pretty(+Sexpr, -Pretty)
sexpr_to_pretty(Thing, Pretty) :-
	atomic(Thing),
	!,
	atom_to_pretty(Thing, Pretty).
sexpr_to_pretty([Sexpr|Sexprs], Pretty) :-
	pp:atm('(', First),
	sexpr_to_pretty(Sexpr, Second),
	sexprs_to_pretty(Sexprs, Rest),
	pp:blo(2, [First, Second|Rest], Pretty).

%% sexprs_to_pretty(+Sexprs, -Pretties)
sexprs_to_pretty([], [Pretty]) :-
	pp:atm(')', Pretty).
sexprs_to_pretty([Sexpr|Sexprs], [Space, Pretty|Pretties]) :-
	pp:brk(1, Space),
	sexpr_to_pretty(Sexpr, Pretty),
	sexprs_to_pretty(Sexprs, Pretties).

%% Creates a group for the top-level list, and then blocks for all
%% interior lists.
%% sexpr_to_group_pretty(+Sexpr, -Pretty)
sexpr_to_group_pretty([Sexpr|Sexprs], Pretty) :-
	!,
	pp:atm('(', First),
	sexpr_to_pretty(Sexpr, Second),
	sexprs_to_pretty(Sexprs, Rest),
	pp:grp(2, [First, Second|Rest], Pretty).
sexpr_to_group_pretty(Sexpr, Pretty) :-
	sexpr_to_pretty(Sexpr, Pretty).
