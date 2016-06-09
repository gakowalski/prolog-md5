:- use_module(library(clpfd)).
:- use_module('conditionals.pl', [
		  if_lesser/3,
		  if_greater_equal/3
	      ]).

xor_3var_1bit_0(A, B, C, Xor) :-
	[A, B, C, Xor] ins 0..1,
	Sum0 #= A+B,
	Sum1 #= Sum0*(2-Sum0) + C,
	Xor #= Sum1*(2-Sum1).

xor_3var_1bit_1(A, B, C, Xor) :-
	[A, B, C, Xor] ins 0..1,
	Xor #= (-A^2-2*A*B+2*A-B^2+2*B+C)*(A^2+2*A*B-2*A+B^2-2*B-C+2).

xor_3var_1bit_2(A, B, C, Xor) :-
	[A, B, C, Xor] ins 0..1,
	%Xor #= (-2*A*B+A+B+C)*(2*A*B-A-B-C+2).
	Sum #= A+B+C,
	Mul #= 2*A*B,
	Xor #= (Sum-Mul)*(Mul-Sum+2).

xor_3var_1bit_3(A, B, C, Xor) :-
	[A, B, C, Xor] ins 0..1,
	Xor #= (A-B-C)^2-4*B*C*(1-A).

% the best now?
xor_3var_1bit_4(A, B, C, Xor) :-
	[A, B, C, Xor] ins 0..1,
	Xor #= (A-B)^2*(1-2*C)+C.

d_number_to_dword_bits(Number, Bits, 0) :-
	integer(Number),
	\+ground(Bits),
	Number < 0xFFFFffff,
	binary_number(B, Number),
	length(Bits, 32),
	append([_, B], Bits).

number_to_dword_bits(Number, Bits, Overflow) :-
	Number #>= 0,
	length(Bits, 32),
	Bits ins 0..1,
	Overflow #>= 0,
	T = [ 0x100000000,
	     1, 2, 4, 8, 16, 32, 64, 128,
	     256, 512, 1024, 2048, 4096, 8192, 16384, 32768,
	     65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608,
	     16777216, 33554432, 67108864, 134217728,
	     268435456, 536870912, 1073741824, 2147483648],
	scalar_product(T, [Overflow | Bits], #=, Number).

% from http://stackoverflow.com/a/28339379/925196
binary_number(Bs, N) :- var(N) -> foldl(shift, Bs, 0, N) ; bitgen(N, Rs), reverse(Rs, Bs).
shift(B, C, R) :- R is (C << 1) + B.
bitgen(N, [B|Bs]) :- B is N /\ 1 , ( N > 1 -> M is N >> 1, bitgen(M, Bs) ; Bs = [] ).

transform_1bit(Trans, AB, BB, CB, X) :-
	number_to_dword_bits(X, XB, 0),
	maplist(Trans, AB, BB, CB, XB).

transform_f_1bit(A, B, C, X) :-
	transform_1bit(f_1bit_1, A, B, C, X).

f_1bit_0(A, B, C, Result) :-
	[A, B, C, Result] ins 0..1,
	Result #= max(min(A,B),min(1-A,C)).

f_1bit_1(A, B, C, Result) :-
	[A, B, C, Result] ins 0..1,
	Result #= max(A*B,(1-A)*C).

f_1bit_2(A, B, C, Result) :-
	X #= A*B,
	Y #= (1-A)*C,
	if_greater_equal(X, Y, Is_Greater),
	Result #= Is_Greater*X + (1-Is_Greater)*Y.

transform_i_1bit(A, B, C, X) :-
	transform_1bit(i_1bit_0, A, B, C, X).

i_1bit_0(A, B, C, Result) :-
	[A, B, C, Result] ins 0..1,
	T #= max(A, 1-C),
	xor_variant_1bit_5(T, B, Result).

transform_h_1bit(A, B, C, X) :-
	transform_1bit(xor_3var_1bit_0, A, B, C, X).

xor_variant_1bit_0(A, B, Xor) :- Xor #= abs(A-B).
xor_variant_1bit_1(A, B, Xor) :- Xor #= (A+B)*(2-A-B).
xor_variant_1bit_2(A, B, Xor) :- Xor #= 2*(A+B)-(A+B)^2.
xor_variant_1bit_3(A, B, Xor) :-
	[A, B, Xor] ins 0..1,
	Sum #= A+B,
	Xor #= Sum*(2-Sum).
xor_variant_1bit_4(A, B, Xor) :-
	Xor #= A+B-2*A*B.
xor_variant_1bit_5(A, B, Xor) :-
	Xor #= (A-B)^2.

dword_xor_2bit(A, B, X) :-
	Limit is (2^32)-1,
	[A, B, X] ins 0..Limit,
	length(AParts, 16),
	length(BParts, 16),
	length(XParts, 16),
	AParts ins 0..3,
	BParts ins 0..3,
	XParts ins 0..3,
	T = [1, 4, 16, 64, 256, 1024, 4096, 16384, 65536, 262144, 1048576, 4194304, 16777216, 67108864, 268435456, 1073741824],
	scalar_product(T, AParts, #=, A),
	scalar_product(T, BParts, #=, B),
	scalar_product(T, XParts, #=, X),
	maplist(xor_variant_2bit_0, AParts, BParts, XParts).

xor_variant_2bit_0(A, B, Xor) :-
	Xor #= ((A + B*((-1)^A)) mod 4).

test_benchmark(Test) :-
	print(Test), time(Test).

md5_benchmark :-
	Battery = [
	    test_md5,
	    test_md5_reverse
	],
	maplist(test_benchmark, Battery),
	!.

%%
% uwaga do samego siebie: istnienie relacji oznacza, ze rzecz musi byc
% zapisywalna takze jako dane, jako "tabelka relacji"
%%
domain(byte, Byte) :-
	Byte in 0..255.
domain(dword, Dword) :-
	Dword in 0..0xFFFFffff.
domain(buffer, Buffer) :-
	length(Buffer, 64),
	Buffer ins 0..255.
domain(states, [A, B, C, D]) :-
	A #>= 0,
	B #>= 0,
	C #>= 0,
	D #>= 0.
domain(digest, [S0, S1, S2, S3]) :-
	[S0, S1, S2, S3] ins 0..0xFFFFFFFF.

var_length(List, Length) :- var(List), label([Length]), length(List, Length).
var_length(List, Length) :- nonvar(List), length(List, Length).

md5(MsgStr, Digest) :-
	% constants
	States = [ 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476 ],
	domain(buffer, Buffer),
	domain(buffer, NewBuffer),
	domain(states, NewStates),
	maplist(=(0), Buffer),

	% domain declarations
	domain(digest, Digest),
	Length in 0..56,
	PaddingLen in 0..24,

	% variable length lists
	var_length(MsgStr, Length),
	MsgStr ins 20..125,

	% message padding
	PaddingLen #= ((4 - Length mod 4) mod 4),
	var_length(Padding, PaddingLen),
	maplist(=(0), Padding),
	append(MsgStr, Padding, Aligned),

	% process message
	md5_update(States, NewStates, Buffer, NewBuffer, Aligned, Length, 0), % true rel
	md5_final(NewStates, NewBuffer, Length, [D1, D2, D3, D4]),

	% truncate output to dword values
	O1 #= D1 mod 0x100000000,
	O2 #= D2 mod 0x100000000,
	O3 #= D3 mod 0x100000000,
	O4 #= D4 mod 0x100000000,

	% final hash values
	Digest = [O1, O2, O3, O4]. % true rel

md5_final(States, Buffer, Length, Digest) :-
	% domain declarations
	maplist(domain(states), [States, Digest]),
	domain(buffer, Buffer),
	PadLen in 1..64,
	Index in 0..63,
	[HighBitCount, LowBitCount] ins 0..255,

	% some constants
	length(Padding, 63), % one byte to be added later
	maplist(=(0), Padding),

	% equations
	HighBitCount #= Length // 16,
	LowBitCount #= (Length mod 16) * 8,

	Bits = [LowBitCount,HighBitCount,0,0,0,0,0,0],
	Index #= Length mod 64,
	if_greater_equal(Index, 56, Is_Greater_Equal),
	PadLen #= (56 + 64*Is_Greater_Equal)-Index,

	md5_update(States, NewStates, Buffer, NewBuffer, [128 | Padding], PadLen, Length), % true rel

	New_Length #= Length + PadLen,
	md5_update(NewStates, Digest, NewBuffer, _, Bits, 8, New_Length). %true rel

md5_transform(f, X, Y, Z, Result) :- transform_f_1bit(X, Y, Z, Result).
md5_transform(g, X, Y, Z, Result) :- transform_f_1bit(Z, X, Y, Result).
md5_transform(h, X, Y, Z, Result) :- transform_h_1bit(Z, X, Y, Result).
md5_transform(i, X, Y, Z, Result) :- transform_i_1bit(X, Y, Z, Result).

md5_bytes_to_dwords([], []).
md5_bytes_to_dwords([D3,D2,D1,D0 | Bytes], [Dword | Dwords]) :-
	maplist(domain(byte), [D0, D1, D2, D3]),
	%domain(dword, Dword),
	Dword #= 16777216*D0 + 65536*D1 + 256*D2 + D3,
        %scalar_product([16777216,65536,256,1],[D0,D1,D2,D3],#=,Dword),
	md5_bytes_to_dwords(Bytes, Dwords).

divide_list(C, List, Left, Right) :-
	append([Left, Right], List),
	length(Left, C).

dword_rotate_left(C, Input, Output) :-
	(   nonvar(Input)
	->
	divide_list(C, Input, Left, Right),
	append(Right, Left, Output)
	;
	D is 32 - C,
	divide_list(D, Output, Left, Right),
	append(Right, Left, Input)).

md5_transform_states(States, Bytes, New_States) :-
	maplist(domain(states), [States, New_States]),
	domain(buffer, Bytes),
	md5_bytes_to_dwords(Bytes, Dwords),
	States = [S1,S2,S3,S4],
	Result = [T1,T2,T3,T4],
	New_States = [O1,O2,O3,O4],
	O1 #= S1 + T1,
	O2 #= S2 + T2,
	O3 #= S3 + T3,
	O4 #= S4 + T4,
	number_to_dword_bits(S2, S2B, _),
	number_to_dword_bits(S3, S3B, _),
	number_to_dword_bits(S4, S4B, _),
	md5_transform_states(1, States, [S2B, S3B, S4B], Dwords, Result).

md5_transform_states(65, States, _,  _, States).

md5_transform_states(Round, [ A, B, C, D ], [BB, CB, DB], Dwords, New_States) :-
	Result #> 0,
	maplist(domain(dword), Dwords),
	domain(dword, X),
	domain(states, [A, B, C, D]),
	domain(states, New_States),
	BB ins 0..1,
	CB ins 0..1,
	DB ins 0..1,

	md5_round_constant(Round, Trans, Rotation, AC, Index),
	md5_rotate_constant(Rotation, S),

	New_Index is Index + 1,
	element(New_Index, Dwords, X),

	md5_transform(Trans, BB, CB, DB, F),

	Sum #= A + F + X + AC,

	%Result #= B + Sum * 2^S + (Sum // 2^(32-S)) mod (2^S), % rotate left S times
	%Result #= B + Sum * 2^S + Sum // 2^(32-S) - (2^S)*((Sum // 2^(32-S))//(2^S)), % rotate left S times
	%Result #= B + Sum * 2^S + Sum // 2^(32-S) - (2^S)*(Sum // 2^32), % rotate left S times
	%Result #= B + (2^S)*(Sum - Sum // 2^32) + Sum // 2^(32-S), % rotate left S times
	%Result #= (B*2^(32-S) + (2^S)*(Sum - Sum// 2^32)*(2^(32-S)) + Sum) // 2^(32-S), % rotate left S times
	%Result #= (B*2^(32-S) + (2^32)*(Sum - Sum// 2^32) + Sum) // 2^(32-S), % rotate left S times
	%Result #= B + ((2^32+1)*Sum - (2^32)*(Sum//2^32)) // 2^(32-S), % rotate left S times
	%Result #= B + ((2^32+1)*Sum - (Sum - (Sum mod 2^32))) // 2^(32-S), % rotate left S times
	%Result #= B + ((2^32)*Sum + Sum mod 2^32) // 2^(32-S), % rotate left S times
	Result #= B + (2^S)*Sum + (Sum mod 2^32) // 2^(32-S), % rotate left S times

	number_to_dword_bits(Result, RB, _),

	Next_Round is Round + 1,
	md5_transform_states(Next_Round, [ D, Result, B, C ], [RB, BB, CB],  Dwords, New_States).

% Prawdziwa relacja!

md5_update(States, NewStates, Buffer, NewBuffer, Input, InputLen, Byte_Count) :-
	maplist(domain(states), [States, NewStates]),
	maplist(domain(buffer), [Buffer, NewBuffer]),
	InputLen in 0..64,
	Index in 0..63,
	PartLen in 1..64,

	Index #= Byte_Count mod 64,
	PartLen #= 64 - Index,

	zcompare(Order, InputLen, PartLen),
	md5_update0(Order, Index, States, NewStates, Buffer, NewBuffer, Input, InputLen).

md5_update0(<, Index, States, States, Buffer, NewBuffer, Input, InputLen) :-
	byte_copy(Buffer, Index, Input, InputLen, NewBuffer).

md5_update0(=, Index, S, NS, B, NB, I, IL) :- md5_update0(>, Index, S, NS, B, NB, I, IL).

md5_update0(>, Index, States, NewStates, Buffer, NewBuffer, Input, InputLen) :-
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
	%TrueDataLen = DataLen + Some_Value,
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










