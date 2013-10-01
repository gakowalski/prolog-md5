% md5
% wersja 1 - prolog bez CLP, implementacja raczej niepelna relacja
% wersja 2 - prolog z CLP, pelne relacje, ale trudne etykietowanie
% wersja 3 - dalsze zmiany w logice

start :-
	use_module(library(clpfd)).

o_dword_dec2bin(Dec, Dword) :-
	length(Dword, 16),
	Dword ins 0..3,
	Dec in 0..4294967295,
	Positions = [1073741824, 268435456, 67108864, 16777216, 4194304, 1048576, 262144, 65536, 16384, 4096, 1024, 256, 64, 16, 4, 1],
	scalar_product(Positions, Dword, #=, Dec).
o2_dword_dec2bin(Dec, Dword) :-
	length(Dword, 11),
	Dword ins 0..7,
	Dec in 0..4294967295,
	Positions = [1073741824, 134217728, 16777216, 2097152, 262144, 32768, 4096, 512, 64, 8, 1],
	scalar_product(Positions, Dword, #=, Dec).
dword_dec2bin(Dec, Dword) :-
	length(Dword, 8),
	Dword ins 0..15,
	Dec in 0..4294967295,
	Positions = [268435456, 16777216, 1048576, 65536, 4096, 256, 16, 1],
	scalar_product(Positions, Dword, #=, Dec).
dword_and(A, B, And) :-
	And #=< A,
	And #=< B,
	dword_dec2bin(A, AD),
	dword_dec2bin(B, BD),
	dword_dec2bin(And, AndD),
	maplist(dword_and0, AD, BD, AndD).
dword_and0(A, B, And) :-
	dword_xor0(A, B, Xor),
	And #= (A + B - Xor)/2.
dword_xor0(A, B, Xor) :-
	X #= A / 4, % important: integer division!
	Y #= B / 4,
	Xor #= ((A + B*((-1)^A)) mod 4) + 4*((X + Y*((-1)^X)) mod 4).
z_dword_xor0(A, B, Xor) :-
	X #= A / 4, % important: integer division!
	Y #= B / 4,
	Xor #= ((A + B*((-1)^A)) mod 4) + 4*(X+Y) - 8*X*Y.
%=MOD($C24+D$23*(-1)^$C24;4)+
%4*MOD(LICZBA.CA£K($C24/4)+
%LICZBA.CA£K(D$23/4)*(-1)^(LICZBA.CA£K($C24/4));4)
a_dword_and0(A, B, And) :- And #= min(A,B) + (A*B*(A-3)*(B-3)*((A-1)*(B-2)+(A-2)*(B-1)))/4.
x_dword_and0(A, B, And) :-
	AB #= A * B,
	Tmp #= 3*A + 3*B,
	And #= min(A,B) + (2*AB^3 - (3*Tmp-22)*AB^2 + (36-13*Tmp+Tmp^2)*AB)/4.
	%And #= min(A,B) + (2*AB^3 - 2*AB*AB*Tmp - AB*AB*Tmp + AB*Tmp*Tmp + 22*AB*AB - 13*AB*Tmp + 36*AB)/4.
	%And #= min(A,B) + (AB*((2*AB-Tmp)*(AB-Tmp)+4*(AB-Tmp)+9*(2*AB-Tmp)+36))/4.
b_dword_and0(A, B, And) :- And #= (A+B-abs(A-B))/2 + (A*B*(A-3)*(B-3)*((A-1)*(B-2)+(A-2)*(B-1)))/4.
dword_or(A, B, Or) :-
	Or #>= A,
	Or #>= B,
	dword_and(A, B, And),
	Or #= A + B - And.
dword_xor(A, B, Xor) :-
	dword_and(A, B, And),
	Xor #= A + B - 2*And.
dword_not(A, NotA) :-
	NotA #= 0xFFFFFFFF - A.

% uwaga do samego siebie: istnieje relacji oznacza, ze rzecz musi byc
% zapisywalna takze jako dane, jako "tabelka relacji"
%
domain(buffer, Buffer) :-
	length(Buffer, 64),
	Buffer ins 0..255.
domain(dword, Dword) :-
	length(Dword, 32),
	Dword ins 0..1.
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
	MsgStr ins 20..105,

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
	label([BitCount]),
	HighBitCount in 0..255,
	HighBitCount #= BitCount / 256,
	LowBitCount in 0..255,
	LowBitCount #= BitCount mod 256,
	Bits = [LowBitCount,HighBitCount,0,0,0,0,0,0],
	length(Padding, 63), % one byte to be added later
	maplist(=(0), Padding),
	Index in 0..63,
	Index #= (BitCount / 8) mod 64,
	md5_final_padlen(Index, PadLen), % PadLen in 1..64
	md5_update(States, NewStates, Buffer, NewBuffer, [128 | Padding], PadLen, BitCount, NewBC), % true rel
	md5_update(NewStates, Digest, NewBuffer, _, Bits, 8, NewBC, _). %true rel
% (index < 56) ? (56 - index) : (120 - index);
% true relation
md5_final_padlen(Index, PadLen) :-
	Index in 0..55,
	PadLen in 1..56,
	PadLen #= 56 - Index.
md5_final_padlen(Index, PadLen) :-
	Index in 56..63,
	PadLen in 57..64,
	PadLen #= 120 - Index.
dword_align(Input, Output, Length) :-
	% dodac domeny dla wszystkich argumentow
	PaddingLen in 0..24,
	PaddingLen #= ((4 - Length mod 4) mod 4),
	var_length(Padding, PaddingLen),
	maplist(=(0), Padding),
	append(Input, Padding, Output).

% OPIS: nazwa stalej, wartosc
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
	dword_and(X, Z, XandZ),
	dword_not(Z, NotZ),
	dword_and(NotZ, Y, NZandY),
	dword_or(XandZ, NZandY, Result).
md5_transform(h, X, Y, Z, Result) :-
	dword_xor(X, Y, XxorY),
	dword_xor(XxorY, Z, Result).
md5_transform(i, X, Y, Z, Result) :-
	dword_not(Z, NotZ),
	dword_or(NotZ, X, NZorX),
	dword_xor(NZorX, Y, Result).
md5_transform_list(Trans, A, B, C, D, X, S, AC, Result) :-
	S in 0..31,
	[A,B,C,D,Result,X,AC] ins 0..4294967295,
	md5_transform(Trans, B, C, D, F),
	Sum #= (A + F + X + AC) mod 4294967296,
	S2 #= 32 - S,
	Rotated #= (Sum * 2^S) mod 4294967296 + (Sum / 2^S2), % rotate left S times
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
	md5_transform_states(1, States, Dwords, Result),
	States = [S1,S2,S3,S4],
	Result = [T1,T2,T3,T4],
	NewStates = [O1,O2,O3,O4],
	O1 #= (S1 + T1) mod 4294967296,
	O2 #= (S2 + T2) mod 4294967296,
	O3 #= (S3 + T3) mod 4294967296,
	O4 #= (S4 + T4) mod 4294967296.
md5_transform_states(65, States, _, States).
md5_transform_states(Round, [ A, B, C, D ], Dwords, NewStates) :-
	md5_round_constant(Round, Trans, Rotation, AC, Index),
	md5_rotate_constant(Rotation, RotValue),
	nth0(Index, Dwords, X),
	md5_transform_list(Trans, A, B, C, D, X, RotValue, AC, Result),
	NewRound is Round + 1,
	md5_transform_states(NewRound, [ D, Result, B, C ], Dwords, NewStates).

% md5_update(
%    States, NewStates,
%    Buffer, NewBuffer,
%    Input, InputLen,
%    InCount0, OutCount0,
%    InCount1, OutCount1).
%
% Prawdziwa relacja!

md5_update(States, NewStates, Buffer, NewBuffer, InputBytes, InputByteLen, BitCount, NewBitCount) :-
	maplist(domain(states), [States, NewStates]),
	maplist(domain(buffer), [Buffer, NewBuffer]),
	% TO DO: domena dla InputBytes
	[BitCount, NewBitCount] ins 0..512, % 512 czy 511?
	md5_update0(States, NewStates, Buffer, NewBuffer, InputBytes, InputByteLen, BitCount, NewBitCount).
md5_update0(States, States, Buffer, NewBuffer, Input, InputLen, BitCount, NewBitCount) :-
	Index in 0..63,
	PartLen in 1..64,
	Index #= (BitCount / 8) mod 64,
	PartLen #= 64 - Index,
	InputLen #< PartLen,
	NewBitCount #= BitCount + InputLen * 8,
	byte_copy(Buffer, Index, Input, InputLen, NewBuffer).
md5_update0(States, NewStates, Buffer, NewBuffer, Input, InputLen, BitCount, NewBitCount) :-
	Index in 0..63,
	PartLen in 1..64,
	Index #= (BitCount / 8) mod 64,
	PartLen #= 64 - Index,
	InputLen #>= PartLen,
	NewBitCount #= BitCount + InputLen * 8,
	byte_copy(Buffer, Index, Input, PartLen, TmpBuffer), % true rel
	md5_transform_states(States, TmpBuffer, NewStates), % true rel
	var_length(Ignore, PartLen),
	append(Ignore, NewInput, Input),
	Len #= InputLen - PartLen,
	byte_copy(TmpBuffer, 0, NewInput, Len, NewBuffer). % true rel
byte_copy(ByteList, ByteIndex, ByteData, ByteLen, NewBuffer) :-
	[ByteIndex, ByteLen] ins 0..63,
	maplist(domain(buffer), [ByteList, NewBuffer]),
	(   var(ByteData) -> TrueDataLen in 0..63, TrueDataLen #>= ByteLen, label([TrueDataLen]), length(ByteData, TrueDataLen)
	;	length(ByteData, TrueDataLen), TrueDataLen #>= ByteLen),
	label([ByteIndex, ByteLen]),
	foldl(buffer(ByteIndex, ByteData, ByteLen), ByteList, NewBuffer, 0, _).
buffer(Index, _, _, Value, Value, Position, NewPosition) :-
	Position < Index,
	NewPosition is Position + 1.
buffer(Index, _, DataLen, Value, Value, Position, NewPosition) :-
	Position >= Index + DataLen,
	NewPosition is Position + 1.
buffer(Index, Data, DataLen, _, Value, Position, NewPosition) :-
	Position >= Index,
	Position < Index + DataLen,
	Pointer is Position - Index,
	nth0(Pointer, Data, Value),
	NewPosition is Position + 1.
demo_md5 :-
	print('Type text to be hashed: '),
	current_input(Stream),
	read_line_to_codes(Stream, Codes),
	md5(Codes, Digest),
	print('MD5 hash: '),
	maplist(format('~16r'), Digest).
test_md5 :-
	md5("TEST", [0x4bd93b03, 0xe4d76811, 0xc344d6f0, 0xbf355ec9]).
test_md5_reverse :-
	md5(Word, [0x4bd93b03, 0xe4d76811, 0xc344d6f0, 0xbf355ec9]),
	%Word = [84, 69, 83, X],
	%label([X]),
	print(Word), nl.











