:- use_module(library(clpfd)).

% return 1 if X is 0 or return 0 if X is nonzero
if_zero(X, Result) :- Result in 0..1, Result #= 0^X.
if_equal(X, C, Result) :- Result in 0..1, Result #= 0^(X-C).
if_lesser(X, C, Result) :- Result in 0..1, Result * (C-X+1*0^(C-X)) #= (C - min(X,C)).
if_lesser_old1(X, C, Result) :-  Result in 0..1, Result #= ((C - min(X,C))/(C-X+1*0^(C-X))).
if_greater_old1(X, C, Result) :-  Result in 0..1, Result #= ((C - max(X,C))/(C-X+1*0^(C-X))).

nearest_powers_of_2(X, Low, High) :-
	X in 0..0xFFFFFFFF,
	Low in 0..31,
	High in 1..32,
	X #>= 2^Low,
	X #=< 2^High,
	Low + 1 #= High.

dword_and(A, B, And) :-
	And #=< A,
	And #=< B,
	dword_xor(A, B, Xor),
	And #= (A + B - Xor)//2.
dword_or(A, B, Or) :-
	Or #>= A,
	Or #>= B,
	dword_xor(A, B, Xor),
	Or #= (A + B + Xor)//2.
o_dword_xor(A, B, Xor) :-
	% specyficzna optymalizacja: predykat jest najwydajniejszy w postaci (var, nonvar, var)
	% oraz (nonvar, nonvar, var) niz w innych kombinacjach, a jego argumenty sa zamienne
	% wiec wystarczy je ulozyc w najlepszej kolejnosci
	var(B) -> ( nonvar(Xor) -> dword_xor1(A, Xor, B, 32) ; dword_xor1(B, A, Xor, 32));
	(   var(Xor) -> dword_xor1(A, B, Xor, 32) ; dword_xor1(Xor, B, A, 32) ).
dword_xor(A, B, Xor) :-
	dword_xor1(A, B, Xor, 32).
	%dword_xor32(A, B, Xor).
dword_not(A, NotA) :-
	NotA #= 0xFFFFFFFF - A.

dword_xor1(A, B, Xor, Bits) :-
	integer(Bits),
	Limit is 2^Bits - 1,
	[A, B, Xor] ins 0..Limit,
	Count is (Bits // 2) - 1,
	dword_xor1(A, B, 0, Count, Xor),
	!. % odciecie poniewaz pracujemy na ograniczeniach, a te sa wyliczane tylko raz
dword_xor1(A, B, Sum, 0, Xor) :-
	% Xor #= Sum + ((A + B*((-1)^A)) mod 4).
	xor0(A, B, Tmp),
	Xor #= Sum + Tmp.
dword_xor1(A, B, Sum, Count, Xor) :-
	Count > 0,
	P is 4^Count,
	X #= A // P,
	Y #= B // P,
	% NewSum #= Sum + P*((X + Y*((-1)^X)) mod 4),
	xor0(X, Y, Tmp),
	NewSum #= Sum + P*Tmp,
	NewCount is Count - 1,
	dword_xor1(A, B, NewSum, NewCount, Xor).

dword_xor32(A, B, X) :-
	Limit is 2^32-1,
	[A, B, X] ins 0..Limit,
	[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16] ins 0..3,
	[B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16] ins 0..3,
	[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16] ins 0..3,
	A #= A1 + A2*2 + A3*2^4 + A4*2^6 + A5*2^8 + A6*2^10 + A7*2^12 + A8*2^14
	+ A9*2^16 + A10*2^18 + A11*2^20 + A12*2^22 + A13*2^24 + A14*2^26
	+ A15*2^28 + A16*2^30,
	B #= B1 + B2*2 + B3*2^4 + B4*2^6 + B5*2^8 + B6*2^10 + B7*2^12 + B8*2^14
	+ B9*2^16 + B10*2^18 + B11*2^20 + B12*2^22 + B13*2^24 + B14*2^26
	+ B15*2^28 + B16*2^30,
	X #= X1 + X2*2 + X3*2^4 + X4*2^6 + X5*2^8 + X6*2^10 + X7*2^12 + X8*2^14
	+ X9*2^16 + X10*2^18 + X11*2^20 + X12*2^22 + X13*2^24 + X14*2^26
	+ X15*2^28 + X16*2^30,
	xor0(A1, B1, X1),
	xor0(A2, B2, X2),
	xor0(A3, B3, X3),
	xor0(A4, B4, X4),
	xor0(A5, B5, X5),
	xor0(A6, B6, X6),
	xor0(A7, B7, X7),
	xor0(A8, B8, X8),
	xor0(A9, B9, X9),
	xor0(A10, B10, X10),
	xor0(A11, B11, X11),
	xor0(A12, B12, X12),
	xor0(A13, B13, X13),
	xor0(A14, B14, X14),
	xor0(A15, B15, X15),
	xor0(A16, B16, X16).

% w przegladzie 0 do 0x2FFFF inferencji 17 m
% preferowane wywolanie: (v,n,v) oraz (n,n,v)
% test_md5 przy pelnej optymalizacji: 163 k
% test_md5_reverse przy pelnej optymalizacji: 5 333 k
xor0(A, B, Xor) :-
	Xor #= ((A + B*((-1)^A)) mod 4).

% w przegladzie 0 do 0x2FFFF inferencji 21 m
% preferowane wywolanie: (n,v,v) oraz (n,n,v)
% test_md5 przy pelnej optymalizacji: ?
% test_md5_reverse przy pelnej optymalizacji: ?
o_xor0(A, B, Xor) :-
	Xor #= ((1 + (A mod 2)*2)*B - 3*A) mod 4.

o_xor0(A, B, Xor) :-
	3*Xor #= 3*A + 3*B - 83*A*B + 81*A*(B^2) - 18*A*(B^3) + 81*(A^2)*B - 81*(A^2)*(B^2) + 18*(A^2)*(B^3) - 18*(A^3)*B + 18*(A^3)*(B^2) - 4*(A^3)*(B^3).

o_xor0(A, B, Xor) :-
	6*Xor #= (1-A)*(2-A)*(3-A)*(A+B) - 3*A*(1-A)*(3-A)*(A+B) + A*(1-A)*(2-A)*(A-B) + 3*A*(2-A)*(3-A)*(A-B) - 3*B*(1-B)*(3-B)*A*(3-A)*(6-4*A) + B*(1-B)*(2-B)*A*(3-A)*(6-4*A).

o_xor0(A, B, Xor) :-
	2*Xor #= abs(A-B) * (2+A*A*B*B - 3*A*B*B - 3*A*A*B + 9*A*B).

test_xor :-
	time(test_xor_1),
	time(test_xor_2),
	time(test_xor_3),
	time(test_xor_4),
	time(test_xor_5),
	time(test_xor_6).

test_xor_1 :-
	[A,B] ins 0..0x2FFF,
	dword_xor(A, B, 5),
	findall(_, labeling([bisect], [A, B]), _).
test_xor_2 :-
	[A,B] ins 0..0x2FFF,
	dword_xor(A, 5, B),
	findall(_, labeling([bisect], [A, B]), _).
test_xor_3 :-
	[A,B] ins 0..0x2FFF,
	dword_xor(5, A, B),
	findall(_, labeling([bisect], [A, B]), _).
test_xor_4 :-
	X in 0..0xFFFF,
	dword_xor(0xFFF, 5, X),
	findall(_, labeling([bisect], [X]), _).
test_xor_5 :-
	X in 0..0x2FFF,
	dword_xor(0xFFF, X, 5),
	findall(_, labeling([bisect], [X]), _).
test_xor_6 :-
	X in 0..0x2FFF,
	dword_xor(X, 0xFFF, 5),
	findall(_, labeling([bisect], [X]), _).


% czasy bez "bisect" dla dwoch nieznanych:
% 1,211,356 inferences, 0,266 CPU in 0,250 seconds (106% CPU, 4560399 Lips)
% 528,134 inferences, 0,063 CPU in 0,078 seconds (80% CPU, 8450144 Lips)
% 490,866 inferences, 0,078 CPU in 0,063 seconds (125% CPU, 6283085 Lips)
%
% czasy z bisect dla dwoch nieznanych (X,Y,5), (X,5,Y), (5,X,Y):
% % 434,156 inferences, 0,125 CPU in 0,109 seconds (114% CPU, 3473248 Lips)
% 329,505 inferences, 0,063 CPU in 0,063 seconds (100% CPU, 5272080 Lips)
% 402,798 inferences, 0,047 CPU in 0,063 seconds (75% CPU, 8593024 Lips)
%
% czasy z bisect dla jednej nieznanej:
% 66,198 inferences, 0,016 CPU in 0,016 seconds (100% CPU, 4236672 Lips)
% 56,437 inferences, 0,016 CPU in 0,016 seconds (100% CPU, 3611968 Lips)
% 63,021 inferences, 0,016 CPU in 0,031 seconds (50% CPU, 4033344 Lips)
%
% dysproporcje dla starego dword_and:
% 3,224,747,117 inferences, 490,469 CPU in 500,932 seconds (98% CPU, 6574827 Lips)
% 18,230,375 inferences, 2,438 CPU in 2,594 seconds (94% CPU, 7479128 Lips)
% 20,433,484 inferences, 2,969 CPU in 3,031 seconds (98% CPU, 6882858 Lips)

%%
% uwaga do samego siebie: istnieje relacji oznacza, ze rzecz musi byc
% zapisywalna takze jako dane, jako "tabelka relacji"
%%
domain(buffer, Buffer) :-
	length(Buffer, 64),
	Buffer ins 0..255.
domain(states, [S0, S1, S2, S3]) :-
	[S0, S1, S2, S3] ins 0..0xFFFFFFFF.
var_length(List, Length) :- var(List), label([Length]), length(List, Length).
var_length(List, Length) :- nonvar(List), length(List, Length).

% glowna funkcja md5
% parametry:
% MsgStr - lista kodow znakowych, np. "Test"
% Digest - Wynik
md5(MsgStr, Digest) :-
	% domain declarations
	domain(states, Digest),
	Length in 0..56,
	var_length(MsgStr, Length),
	MsgStr ins 20..125,

	% creating some constants
	States = [ 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476 ],
	domain(buffer, Buffer),
	maplist(=(0), Buffer),

	% add padding zeros
	dword_align(MsgStr, Aligned, Length),

	% process message
	md5_update(States, NewStates, Buffer, NewBuffer, Aligned, Length, 0, BitCount), % true rel
	md5_final(NewStates, NewBuffer, BitCount, Digest). % true rel

md5_final(States, Buffer, BitCount, Digest) :-
	maplist(domain(states), [States, Digest]),
	domain(buffer, Buffer),
	BitCount in 0..65535,
	%label([BitCount]),
	[HighBitCount, LowBitCount] ins 0..255,
	HighBitCount #= BitCount // 256,
	LowBitCount #= BitCount mod 256,
	Bits = [LowBitCount,HighBitCount,0,0,0,0,0,0],
	length(Padding, 63), % one byte to be added later
	maplist(=(0), Padding),
	Index in 0..63,
	Index #= (BitCount // 8) mod 64,
	md5_final_padlen(Index, PadLen), % PadLen in 1..64
	md5_update(States, NewStates, Buffer, NewBuffer, [128 | Padding], PadLen, BitCount, NewBC), % true rel
	md5_update(NewStates, Digest, NewBuffer, _, Bits, 8, NewBC, _). %true rel
% (index < 56) ? (56 - index) : (120 - index);
% true relation
md5_final_padlen(Index, PadLen) :-
	Index in 0..63,
	PadLen in 1..64,
	if_lesser(Index, 56, Lesser),
	PadLen #= Lesser*(56-Index) + (1-Lesser)*(120-Index).
dword_align(Input, Output, Length) :-
	% dodac domeny dla wszystkich argumentow
	PaddingLen in 0..24,
	PaddingLen #= ((4 - Length mod 4) mod 4),
	var_length(Padding, PaddingLen),
	maplist(=(0), Padding),
	append(Input, Padding, Output).


% #define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
% #define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
% #define H(x, y, z) ((x) ^ (y) ^ (z))
% #define I(x, y, z) ((y) ^ ((x) | (~z)))

% md5_transform(type, x, y, z, result).
md5_transform(f, X, Y, Z, Result) :-
	dword_and(X, Y, XandY),
	dword_not(X, NotX),
	dword_and(NotX, Z, NXandZ),
	dword_or(XandY, NXandZ, Result).
md5_transform(g, X, Y, Z, Result) :-
	md5_transform(f, Z, X, Y, Result).
md5_transform(h, X, Y, Z, Result) :-
	dword_xor(X, Y, XxorY),
	dword_xor(XxorY, Z, Result).
md5_transform(i, X, Y, Z, Result) :-
	dword_not(Z, NotZ),
	dword_or(NotZ, X, NZorX),
	dword_xor(NZorX, Y, Result).
md5_transform_list(Trans, A, B, C, D, X, S, AC, Result) :-
	S in 0..31,
	[A,B,C,D,Result,X,AC,Rotated] ins 0..4294967295,
	md5_transform(Trans, B, C, D, F),
	Sum #= (A + F + X + AC) mod 4294967296,
	S2 #= 32 - S,
	Rotated #= (Sum * 2^S) mod 4294967296 + (Sum // 2^S2), % rotate left S times
	Result #= (Rotated + B) mod 4294967296.
md5_bytes_to_dwords([], []).
md5_bytes_to_dwords([D3,D2,D1,D0 | Bytes], [Dword | Dwords]) :-
	Dword #= 16777216*D0 + 65536*D1 + 256*D2 + D3,
	md5_bytes_to_dwords(Bytes, Dwords).
% md5_transform_states/3
% prawdziwa relacja
% TRUE RELATION!
% States = stany inicjalne
% Bytes = kodowany komunikat
% NewStates = stany koncowe
md5_transform_states(States, Bytes, NewStates) :-
	maplist(domain(states), [States, NewStates]),
	domain(buffer, Bytes),
	md5_bytes_to_dwords(Bytes, Dwords),
	States = [S1,S2,S3,S4],
	Result = [T1,T2,T3,T4],
	NewStates = [O1,O2,O3,O4],
	O1 #= (S1 + T1) mod 4294967296,
	O2 #= (S2 + T2) mod 4294967296,
	O3 #= (S3 + T3) mod 4294967296,
	O4 #= (S4 + T4) mod 4294967296,
	md5_transform_states(1, States, Dwords, Result).

md5_transform_states(65, States, _, States).
md5_transform_states(Round, [ A, B, C, D ], Dwords, NewStates) :-
	md5_round_constant(Round, Trans, Rotation, AC, Index),
	md5_rotate_constant(Rotation, RotValue),
	nth0(Index, Dwords, X),
	md5_transform_list(Trans, A, B, C, D, X, RotValue, AC, Result),
	NewRound is Round + 1,
	md5_transform_states(NewRound, [ D, Result, B, C ], Dwords, NewStates).
test_md5_transform :-
	L = [1732584193,4023233417,2562383102,271733878],
	M = [ 84, 69, 83, 84, 128, 0, 0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      32,  0,  0,  0,  0,  0,  0,  0],
	md5_transform_states(L, M, N),
	labeling([bisect], N),
	N = [1272527619,3839322129,3276068592,3207945929].
test_md5_transform_reverse :-
	N = [1272527619,3839322129,3276068592,3207945929],
	md5_transform_states(L, M, N),
	labeling([bisect], L),
	L = [1732584193,4023233417,2562383102,271733878],
	labeling([bisect], M),
	M = [ 84, 69, 83, 84, 128, 0, 0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      0,  0,  0,  0,  0,  0,  0,  0,
	      32,  0,  0,  0,  0,  0,  0,  0].
test_md5_transform_list :-
	A = 1732584193,
	B = 4023233417,
	C = 2562383102,
	D = 271733878,
	X = 1414743380,
	RV = 7,
	AC = 3614090360,
	md5_transform_list(f, A, B, C, D, X, RV, AC, Result),
	Result = 3468857630.
test_md5_transform_list_reverse :-
	Result = 3468857630,
	RV = 7,
	AC = 3614090360,
	md5_transform_list(f, A, B, C, D, X, RV, AC, Result),
	labeling([ffc, bisect], [A,B,C,D,X]),
	A = 1732584193,
	B = 4023233417,
	C = 2562383102,
	D = 271733878,
	X = 1414743380.
test_md5_transform_list_reverse_2 :-
	Result = 3468857630,
	RV = 7,
	AC = 3614090360,
	md5_transform_list(f, A, B, C, D, X, RV, AC, Result),
	A = 1732584193,
	B = 4023233417,
	C = 2562383102,
	D = 271733878,
	labeling([ffc, bisect], [A,B,C,D,X]),
	X = 1414743380.






% md5_update(
%    States, NewStates,
%    Buffer, NewBuffer,
%    Input, InputLen,
%    InCount0, OutCount0,
%    InCount1, OutCount1).
%
% Prawdziwa relacja!

md5_update(States, NewStates, Buffer, NewBuffer, Input, InputLen, BitCount, NewBitCount) :-
	maplist(domain(states), [States, NewStates]),
	maplist(domain(buffer), [Buffer, NewBuffer]),
	% TO DO: domena dla Input
	[BitCount, NewBitCount] ins 0..512, % 512 czy 511?
	md5_update0(States, NewStates, Buffer, NewBuffer, Input, InputLen, BitCount, NewBitCount).
md5_update0(States, States, Buffer, NewBuffer, Input, InputLen, BitCount, NewBitCount) :-
	Index in 0..63,
	PartLen in 1..64,
	Index #= (BitCount // 8) mod 64,
	PartLen #= 64 - Index,
	InputLen #< PartLen,
	NewBitCount #= BitCount + InputLen * 8,
	byte_copy(Buffer, Index, Input, InputLen, NewBuffer).
md5_update0(States, NewStates, Buffer, NewBuffer, Input, InputLen, BitCount, NewBitCount) :-
	Index in 0..63,
	PartLen in 1..64,
	Index #= (BitCount // 8) mod 64,
	PartLen #= 64 - Index,
	InputLen #>= PartLen,
	NewBitCount #= BitCount + InputLen * 8,
	byte_copy(Buffer, Index, Input, PartLen, TmpBuffer), % true rel
	md5_transform_states(States, TmpBuffer, NewStates), % true rel
	var_length(Ignore, PartLen),
	append(Ignore, NewInput, Input),
	Len #= InputLen - PartLen,
	byte_copy(TmpBuffer, 0, NewInput, Len, NewBuffer). % true rel
byte_copy(Buffer, _, [], _, Buffer).
byte_copy(Buffer, Start, Data, DataLen, NewBuffer) :-
	maplist(domain(buffer), [Buffer, NewBuffer]),
	Start in 0..63,
	[DataLen, TrueDataLen] ins 0..64,
	TrueDataLen #>= DataLen,
	var_length(Data, TrueDataLen),
	byte_copy(Buffer, Start, Data, DataLen, NewBuffer, 0).
byte_copy([], _, _, _, [], _).
byte_copy([ B | Buffer ], Start, Data, DataLen, [ NB | NewBuffer ], Counter) :-
	if_lesser(Counter, Start, L1),
	End #= Start + DataLen,
	if_lesser(Counter, End, L2),
	Position #= (1-L1)*(L2)*(Counter-Start),
	nth0(Position, Data, D),
	NB #= (1-L1)*(L2)*D + (1-(1-L1)*(L2))*B,
	NewCounter is Counter + 1,
	byte_copy(Buffer, Start, Data, DataLen, NewBuffer, NewCounter).
demo_md5 :-
	print('Type text to be hashed: '),
	current_input(Stream),
	read_line_to_codes(Stream, Codes),
	md5(Codes, Digest),
	print('MD5 hash: '),
	labeling([bisect], Digest),
	maplist(format('~16r'), Digest).
test_md5 :-
	md5(`TEST`, [0x4bd93b03, 0xe4d76811, 0xc344d6f0, 0xbf355ec9]).
test_md5_reverse :-
	md5(Word, [0x4bd93b03, 0xe4d76811, 0xc344d6f0, 0xbf355ec9]),
	Word = [84, 69, 83, 84].
	%labeling([bisect], [X]),
	%print(X), nl.
test_md5_reverse_true :-
	length(Word, Len),
	Len < 16,
	Word ins 32..126,
	md5(Word, [0x4bd93b03, 0xe4d76811, 0xc344d6f0, 0xbf355ec9]),
	labeling([bisect], Word),
	print(Word), nl.

% OPIS: nazwa stalej, wartosc
% Min = 4
% Max = 23
md5_rotate_constant(s11, 7).
md5_rotate_constant(s12, 12).
md5_rotate_constant(s13, 17).
md5_rotate_constant(s14, 22).
md5_rotate_constant(s21, 5).
md5_rotate_constant(s22, 9).
md5_rotate_constant(s23, 14).
md5_rotate_constant(s24, 20).
md5_rotate_constant(s31, 4).
md5_rotate_constant(s32, 11).
md5_rotate_constant(s33, 16).
md5_rotate_constant(s34, 23).
md5_rotate_constant(s41, 6).
md5_rotate_constant(s42, 10).
md5_rotate_constant(s43, 15).
md5_rotate_constant(s44, 21).

% OPIS: nr rundy, nazwa funkcji, symbol rotacji, stala specjalna, numer
% bloku
%  findall(Z, md5_round_constant(_,_,_,Z,_), Z), min_list(Z, Min).
%  Min = 38016083
%  Max = 4294925233
md5_round_constant(1, f, s11, 0xd76aa478, 0).
md5_round_constant(2, f, s12, 0xe8c7b756, 1).
md5_round_constant(3, f, s13, 0x242070db, 2).
md5_round_constant(4, f, s14, 0xc1bdceee, 3).
md5_round_constant(5, f, s11, 0xf57c0faf, 4).
md5_round_constant(6, f, s12, 0x4787c62a, 5).
md5_round_constant(7, f, s13, 0xa8304613, 6).
md5_round_constant(8, f, s14, 0xfd469501, 7).
md5_round_constant(9, f, s11, 0x698098d8, 8).
md5_round_constant(10, f, s12, 0x8b44f7af, 9).
md5_round_constant(11, f, s13, 0xffff5bb1, 10).
md5_round_constant(12, f, s14, 0x895cd7be, 11).
md5_round_constant(13, f, s11, 0x6b901122, 12).
md5_round_constant(14, f, s12, 0xfd987193, 13).
md5_round_constant(15, f, s13, 0xa679438e, 14).
md5_round_constant(16, f, s14, 0x49b40821, 15).
md5_round_constant(17, g, s21, 0xf61e2562, 1).
md5_round_constant(18, g, s22, 0xc040b340, 6).
md5_round_constant(19, g, s23, 0x265e5a51, 11).
md5_round_constant(20, g, s24, 0xe9b6c7aa, 0).
md5_round_constant(21, g, s21, 0xd62f105d, 5).
md5_round_constant(22, g, s22, 0x02441453, 10).
md5_round_constant(23, g, s23, 0xd8a1e681, 15).
md5_round_constant(24, g, s24, 0xe7d3fbc8, 4).
md5_round_constant(25, g, s21, 0x21e1cde6, 9).
md5_round_constant(26, g, s22, 0xc33707d6, 14).
md5_round_constant(27, g, s23, 0xf4d50d87, 3).
md5_round_constant(28, g, s24, 0x455a14ed, 8).
md5_round_constant(29, g, s21, 0xa9e3e905, 13).
md5_round_constant(30, g, s22, 0xfcefa3f8, 2).
md5_round_constant(31, g, s23, 0x676f02d9, 7).
md5_round_constant(32, g, s24, 0x8d2a4c8a, 12).
md5_round_constant(33, h, s31, 0xfffa3942, 5).
md5_round_constant(34, h, s32, 0x8771f681, 8).
md5_round_constant(35, h, s33, 0x6d9d6122, 11).
md5_round_constant(36, h, s34, 0xfde5380c, 14).
md5_round_constant(37, h, s31, 0xa4beea44, 1).
md5_round_constant(38, h, s32, 0x4bdecfa9, 4).
md5_round_constant(39, h, s33, 0xf6bb4b60, 7).
md5_round_constant(40, h, s34, 0xbebfbc70, 10).
md5_round_constant(41, h, s31, 0x289b7ec6, 13).
md5_round_constant(42, h, s32, 0xeaa127fa, 0).
md5_round_constant(43, h, s33, 0xd4ef3085, 3).
md5_round_constant(44, h, s34, 0x04881d05, 6).
md5_round_constant(45, h, s31, 0xd9d4d039, 9).
md5_round_constant(46, h, s32, 0xe6db99e5, 12).
md5_round_constant(47, h, s33, 0x1fa27cf8, 15).
md5_round_constant(48, h, s34, 0xc4ac5665, 2).
md5_round_constant(49, i, s41, 0xf4292244, 0).
md5_round_constant(50, i, s42, 0x432aff97, 7).
md5_round_constant(51, i, s43, 0xab9423a7, 14).
md5_round_constant(52, i, s44, 0xfc93a039, 5).
md5_round_constant(53, i, s41, 0x655b59c3, 12).
md5_round_constant(54, i, s42, 0x8f0ccc92, 3).
md5_round_constant(55, i, s43, 0xffeff47d, 10).
md5_round_constant(56, i, s44, 0x85845dd1, 1).
md5_round_constant(57, i, s41, 0x6fa87e4f, 8).
md5_round_constant(58, i, s42, 0xfe2ce6e0, 15).
md5_round_constant(59, i, s43, 0xa3014314, 6).
md5_round_constant(60, i, s44, 0x4e0811a1, 13).
md5_round_constant(61, i, s41, 0xf7537e82, 4).
md5_round_constant(62, i, s42, 0xbd3af235, 11).
md5_round_constant(63, i, s43, 0x2ad7d2bb, 2).
md5_round_constant(64, i, s44, 0xeb86d391, 9).










