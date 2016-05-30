%
% grzegorz.adam.kowalski@outlook.com
% grzegorz.kowalski@wit.edu.pl
%
%
:- module(conditionals, [
	      if_equal/3,
	      if_even/2,
	      if_greater/3,
	      if_greater_equal/3,
	      if_lesser/3,
	      if_lesser_equal/3,
	      if_nonzero/2,
	      if_odd/2,
	      if_zero/2
	  ]).

:- use_module(library(clpfd)).

% Basic building blocks:
% - if_between
% - if_lesser_equal
% - if_greater_equal
% - if_even
% - cplfd:max
% - clpfd:min
% = clpfd:abs

% ============================================
% LESSER, GREATER, LESSER_EQUAL, GREATER_EQUAL
% ============================================

if_lesser_equal(X, C, Result) :-
	zcompare(Ord, X, C),
	if_order_greater(Ord, Is_Greater),
	Result #= 1-Is_Greater.

if_greater_equal(X, C, Result) :-
	zcompare(Ord, X, C),
	if_order_lesser(Ord, Is_Lesser),
	Result #= 1-Is_Lesser.

if_lesser(X, C, Result) :-
	zcompare(Ord, X, C),
	if_order_lesser(Ord, Result).

if_greater(X, C, Result) :-
	zcompare(Ord, X, C),
	if_order_greater(Ord, Result).


% =====
% EQUAL
% =====

if_equal(X, V, Is_Equal) :-
	zcompare(Ord, X, V),
	if_order_equal(Ord, Is_Equal).

% ====
% ZERO
% ====

if_zero(X, Result) :-
	zcompare(Ord, X, 0),
	if_order_equal(Ord, Result).

if_nonzero(X, Result) :-
	if_zero(X, R),
	Result #= 1-R.

% =========
% EVEN, ODD
% =========

if_even(X, Result) :-
	if_even_raw(X, Is_Even),
	2 * Result #= 1 + Is_Even.

if_odd(X, Result) :-
	if_even_raw(X, Is_Even),
	2 * Result #= 1 - Is_Even.

if_even_raw(X, Result) :-
	% needs domain declaration for Result
	Result #= (-1)^X.

% =============================
% OTHER, NOT USED, EXPERIMENTAL
% =============================

internal_abs(X, Abs) :-
	% needs domain declaration for Abs if possible
	Abs #= max(X, -X).

if_bit_set(X, 1, Set) :-
	Set in 0..1,
	if_odd(X, Set).

if_bit_set(X, C, Set) :-
	C > 1,
	Set in 0..1,
	if_odd(X, Is_Odd),
        2*Y #= X - Is_Odd,
        D is C-1,
        if_bit_set(Y, D, Set).

internal_mod_2(X, Mod) :-
	Mod in 0..1,
	2 * Mod #= 1+(-1)^(X+1).

% ================
% ZCOMPARE HELPERS
% ================

if_order_equal(=, Then) :- Then #= 1.
if_order_equal(<, Then) :- Then #= 0.
if_order_equal(>, Then) :- Then #= 0.

if_order_lesser(<, Then) :- Then #= 1.
if_order_lesser(=, Then) :- Then #= 0.
if_order_lesser(>, Then) :- Then #= 0.

if_order_greater(<, Then) :- Then #= 0.
if_order_greater(=, Then) :- Then #= 0.
if_order_greater(>, Then) :- Then #= 1.








