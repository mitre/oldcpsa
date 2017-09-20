% -*- mode: prolog -*-

%% A simple pretty printer

%% Construct a pretty printer tree using the tree constructors atm,
%% brk, blo, and grb, and then print it with pr.

%% The alogithm is by Lawrence C. Paulson, who simplified an algorithm
%% by Derek C. Oppen.

%% Derek C. Oppen, Prettyprinting, ACM Transactions on Programming
%% Languages and Systems, Vol 2, No. 4, October 1980, Pages 465-483.

%% Copyright (c) 2009 The MITRE Corporation
%%
%% This program is free software: you can redistribute it and/or
%% modify it under the terms of the BSD License as published by the
%% University of California.

%% A pretty printer based on ML programs with the following copyright

%% (**** ML Programs from Chapter 8 of

%%   ML for the Working Programmer, 2nd edition
%%   by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
%%   (Cambridge University Press, 1996)

%% Copyright (C) 1996 by Cambridge University Press.
%% Permission to copy without fee is granted provided that this copyright
%% notice and the DISCLAIMER OF WARRANTY are included in any copy.

%% DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
%% warranty of any kind.  We make no warranties, express or implied, that the
%% programs are free of error, or are consistent with any particular standard
%% of merchantability, or that they will meet your requirements for any
%% particular application.  They should not be relied upon for solving a
%% problem whose incorrect solution could result in injury to a person or loss
%% of property.  If you do use the programs or functions in such a manner, it
%% is at your own risk.  The author and publisher disclaim all liability for
%% direct, incidental or consequential damages resulting from your use of
%% these programs or functions.
%% ****)

% Known to work in SWI-Prolog, but not with GNU Prolog.

:- module(pp, [atm/2, brk/2, blo/3, grp/3, pr/2, pr/3]).

%% Pretty printer tree constructors

%% Atoms (Strings str in the original code)
%% atm(+Atom, -Pretty)
atm(Atom, atm(Codes)) :-
	atomic(Atom),
	!,
	atom_codes(Atom, Codes).
atm(Codes, atm(Codes)).

%% Break points, where Len is the number of spaces to print when the
%% break is not turned into a newline break.
%% brk(+Len, -Pretty)
brk(Len, brk(Len)) :-
	integer(Len).

%% Indentation blocks (any number of breaks may be turned into a
%% newline break).
%% blk(+Indent, +Pretties, -Pretty)
blo(Indent, Pretties, blo(Pretties, Indent, Len)) :-
	len(Pretties, Len).

%% Indentation groups (all or none of the breaks are turned into
%% newline breaks)
%% grp(+Indent, +Pretties, -Pretty)
grp(Indent, Pretties, grp(Pretties, Indent, Len)) :-
	len(Pretties, Len).

% Compute the unbroken length of a list of pretty printing trees.
len(Pretties, Len) :-
	len(Pretties, 0, Len).

len([], Len, Len).
len([Pretty|Pretties], Start, Out) :-
	size(Pretty, Len),
	End is Start + Len,
	len(Pretties, End, Out).

size(atm(Codes), Len) :-
	length(Codes, Len).
size(brk(Len), Len).
size(blo(_, _, Len), Len).
size(grp(_, _, Len), Len).

%% After generating a pretty printing tree, print it

%% pr(+Margin, +Pretty)
pr(Margin, Pretty) :-
	current_output(Stream),
	pr(Stream, Margin, Pretty).

%% pr(+Stream, +Margin, +Pretty)
pr(Stream, Margin, Pretty) :-
	printing(Stream, Margin, [Pretty], Margin, 0, false, Margin, _).

%% Pretty print lists of trees
printing(_, _, [], _, _, _, Space, Space).
printing(Stream, Margin, [Pretty|Pretties],
	Blockspace, After, Force, Start, End) :-
	pretty_print(Stream, Margin, Pretty, Pretties,
	Blockspace, After, Force, Start, Mid),
	printing(Stream, Margin, Pretties,
	Blockspace, After, Force, Mid, End).

%% Print a string
pretty_print(Stream, _, atm(Codes), _, _,
	_, _, Start, End) :-
	put_codes(Stream, Codes),
	length(Codes, Len),
	End is Start - Len.
%% Print a break point without a newline break
pretty_print(Stream, _, brk(Len), Pretties, _,
	After, false, Start, End) :-
	break_dist(Pretties, After, Dist),
	Len + Dist =< Start,
	!,
	put_blanks(Stream, Len),
	End is Start - Len.
%% Print a break point with a newline break
pretty_print(Stream, Margin, brk(_), _, Blockspace,
	_, _, _, Blockspace) :-
	nl(Stream),
	End is Margin - Blockspace,
	put_blanks(Stream, End).
%% Print a block
pretty_print(Stream, Margin, blo(Body, Indent, _), Pretties, _,
	After, _, Start, End) :-
	New is Start - Indent,
	break_dist(Pretties, After, Dist),
	printing(Stream, Margin, Body, New, Dist, false, Start, End).
%% Print a group
pretty_print(Stream, Margin, grp(Body, Indent, Len), Pretties, _,
	After, _, Start, End) :-
	New is Start - Indent,
	break_dist(Pretties, After, Dist),
	(Len + Dist > Start -> Force = true; Force = false),
	printing(Stream, Margin, Body, New, Dist, Force, Start, End).

%% Find the distance to the next break point
break_dist([], After, After).
break_dist([atm(Codes)|Pretties], After, Dist) :-
	break_dist(Pretties, After, Mid),
	length(Codes, Len),
	Dist is Mid + Len.
break_dist([brk(_)|_], _, 0).
break_dist([blo(_, _, Len)|Pretties], After, Dist) :-
	break_dist(Pretties, After, Mid),
	Dist is Mid + Len.
break_dist([grp(_, _, Len)|Pretties], After, Dist) :-
	break_dist(Pretties, After, Mid),
	Dist is Mid + Len.

%% Printing primitives
put_blanks(Stream, Len) :-
	0 < Len,
	!,
	put_blank(Stream),
	Next is Len - 1,
	put_blanks(Stream, Next).
put_blanks(_, _).

put_blank(Stream) :-
	atom_codes(' ', Codes),
	put_codes(Stream, Codes).

put_codes(_, []).
put_codes(Stream, [Code|Codes]) :-
	put_code(Stream, Code),
	put_codes(Stream, Codes).
