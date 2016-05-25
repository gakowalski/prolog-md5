%
% grzegorz.adam.kowalski@outlook.com
% grzegorz.kowalski@wit.edu.pl
%
%
:- module(conditionals, [
	      if_between/4,
	      if_equal/3,
	      if_even/2,
	      if_greater/3,
	      if_greater_equal/3,
	      if_lesser/3,
	      if_lesser_equal/3,
	      if_nonzero/2,
	      if_odd/2,
	      if_outside/4,
	      if_zero/2
	  ]).

:- use_module(library(clpfd)).

% Basic building blocks:
% - if_between
% - if_lesser_equal
% - if_greater_equal
% - if_even

% ============================================
% LESSER, GREATER, LESSER_EQUAL, GREATER_EQUAL
% ============================================

if_lesser_equal(X, C, Result) :-
	Result in 0..1,
	T #= C - max(X,C),
	if_zero(T, Result).

if_greater_equal(X, C, Result) :-
	Result in 0..1,
	T #= C - min(X, C),
	if_zero(T, Result).

if_lesser(X, C, Result) :-
	Result in 0..1,
	if_greater_equal(X, C, Is_Greater),
	Result #= 1 - Is_Greater.

if_greater(X, C, Result) :-
	Result in 0..1,
	if_lesser_equal(X, C, Is_Lesser),
	Result #= 1 - Is_Lesser.

if_lesser_varian_0(X, C, Result) :-
	Result in 0..1,
	Result #= ((C - min(X,C))/(C-X+1*0^(C-X))).

if_lesser_variant_1(X, C, Result) :-
	Result in 0..1,
	Result * (C-X+1*0^(C-X)) #= (C - min(X,C)).

if_greater_variant_0(X, C, Result) :-
	Result in 0..1,
	Result #= ((C - max(X,C))/(C-X+1*0^(C-X))).

% =======
% BETWEEN
% =======
if_between(X, L, H, Result) :-
	if_between_raw(X, L, H, Raw_Result),
	if_nonzero(Raw_Result, Result).

if_between_raw(X, L, H, Result) :-
	Result #= (L-max(L,X))*(H-min(H,X)).

test_if_between_raw :-
	if_between(0, 1, 3, Outside),
	if_between(1, 1, 3, Outside),
	if_between(2, 1, 3, In),
	if_between(3, 1, 3, Outside),
	if_between(4, 1, 3, Outside),
	In #\= 0,
	Outside #= 0.

if_outside(X, L, H, Result) :-
	if_between(X, L, H, Is_Between),
	Result #= 1 - Is_Between.

% =====
% EQUAL
% =====

if_equal(X, V, Is_Equal) :-
	if_equal_raw(X, V, Result),
	Is_Equal #= Result * (-1).

if_equal_raw(X, V, Is_Equal) :-
	Is_Equal in -1..0,
	L #= V-1,
	H #= V+1,
	if_between_raw(X, L, H, Is_Equal).

if_equal_variant_0(X, C, Result) :-
	Result in 0..1,
	Result #= 0^(X-C).

test_if_equal :-
	if_equal(0, 0, Equal),
	if_equal(1, 1, Equal),
	if_equal(2, 3, NEqual),
	if_equal(0, 5, NEqual),
	Equal #= 1,
	NEqual #= 0.

% ====
% ZERO
% ====

if_zero(X, Result) :-
	if_equal(X, 0, Result).

if_nonzero(X, Result) :-
	if_equal(X, 0, Is_Equal),
	Result #= 1 - Is_Equal.

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
	% needs domain declaration
	Result #= (-1)^X.










