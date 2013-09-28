start :-
	use_module(library(clpfd)).

% add (A, B, Sum, NextCarry) realizuje sume dwoch bitow
add(A, B, S, C) :-
	[A, B, S, C] ins 0..1,
	S #= (A + B) mod 2,
	C #= (A + B) / 2.
% add (A, B, PrevCarry, Sum, NextCarry) realizuje sume dwoch bitow
add(A, B, PC, S, C) :-
	[A, B, S, C] ins 0..1,
	Sum #= A + B + PC,
	S #= Sum mod 2,
	C #= Sum / 2.
% true relation
add_list( [ A ], [ B ], [ S ], C ) :- add(A, B, S, C).
add_list( [ A | AT ], [ B | BT ], [ S | ST ], C ) :-
	add(A, B, Prev, S, C),
	add_list(AT, BT, ST, Prev).
% hex_digit(digit, char_code, bit_list)
% cyfry
hex_digit(D, CC, [B3,B2,B1,B0]) :-
	D in 0..9,
	CC in 48..57,
	[B3,B2,B1,B0] ins 0..1,
	CC #= D + 48,
	scalar_product([8,4,2,1], [B3,B2,B1,B0], #=, D).
% duze litery
hex_digit(D, CC, [B3,B2,B1,B0]) :-
	D in 10..15,
	CC in 65..70,
	[B3,B2,B1,B0] ins 0..1,
	CC #= D + 55,
	scalar_product([8,4,2,1], [B3,B2,B1,B0], #=, D).
% male litery
hex_digit(D, CC, [B3,B2,B1,B0]) :-
	D in 10..15,
	CC in 97..102,
	[B3,B2,B1,B0] ins 0..1,
	CC #= D + 87,
	scalar_product([8,4,2,1], [B3,B2,B1,B0], #=, D).
% byte_dec2bin/2
% byte_dec2bin(Decimal, Binary)
% Pelna relacja.
% Relacja istnieje, gdy Decimal i Binary reprezentuja ta sama wartosc
% bajtu.
byte_dec2bin(Dec, [B7,B6,B5,B4,B3,B2,B1,B0]) :-
	[B7,B6,B5,B4,B3,B2,B1,B0] ins 0..1,
	Dec in 0..255,
        scalar_product([128,64,32,16,8,4,2,1], [B7,B6,B5,B4,B3,B2,B1,B0], #=, Dec).
% uwaga do samego siebie: istnieje relacji oznacza, ze rzecz musi byc
% zapisywalna takze jako dane, jako "tabelka relacji"
test_byte_dec2bin :-
	byte_dec2bin(54, [ 0, 0, 1, 1, 0, 1, 1, 0 ]),
	not(byte_dec2bin(256, _)),
	not(byte_dec2bin(_, [0,1,0,1,0,1,0,1,0,1,0])).

conv_hex_to_dword(HexStr, List) :-
	string_to_list(HexStr, [C0, C1, C2, C3, C4, C5, C6, C7]),
	hex_digit(_, C0, B0),
	hex_digit(_, C1, B1),
	hex_digit(_, C2, B2),
	hex_digit(_, C3, B3),
	hex_digit(_, C4, B4),
	hex_digit(_, C5, B5),
	hex_digit(_, C6, B6),
	hex_digit(_, C7, B7),
	append( [ B0, B1, B2, B3, B4, B5, B6, B7 ], List).

conv_hex_to_dword_reverse(HexStr, List) :-
	string_to_list(HexStr, [C0, C1, C2, C3, C4, C5, C6, C7]),
	hex_digit(_, C0, B0),
	hex_digit(_, C1, B1),
	hex_digit(_, C2, B2),
	hex_digit(_, C3, B3),
	hex_digit(_, C4, B4),
	hex_digit(_, C5, B5),
	hex_digit(_, C6, B6),
	hex_digit(_, C7, B7),
	append( [ B6, B7, B4, B5, B2, B3, B0, B1 ], List).
conv_bytes_to_hex([]).
conv_bytes_to_hex([ H1, H2, H3, H4, H5, H6, H7, H8 | List ]) :-
	hex_digit(_, C1, [H1, H2, H3, H4]),
	hex_digit(_, C2, [H5, H6, H7, H8]),
	put_char(C1),
	put_char(C2),
	conv_bytes_to_hex(List),
	!.
conv_bytes_to_hex_reverse([]).
conv_bytes_to_hex_reverse([ H1, H2, H3, H4, H5, H6, H7, H8 | List ]) :-
	hex_digit(_, C1, [H1, H2, H3, H4]),
	hex_digit(_, C2, [H5, H6, H7, H8]),
	conv_bytes_to_hex_reverse(List),
	put_char(C1),
	put_char(C2),
	!.

run_tests :-
	run_test(test_byte_dec2bin),
	run_test(test_char_to_dword),
	run_test(test_conv_hex_to_dword),
	run_test(test_conv_hex_to_dword_reverse),
	run_test(test_md5_transform_list),
	run_test(test_md5_transform_list_f),
	run_test(test_md5_transform_list_g),
	run_test(test_md5_transform_list_h),
	run_test(test_md5_transform_list_i),
	run_test(test_md5_transform_states),
	run_test(test_md5_transform_states_reverse),
	run_test(test_buffer),
	run_test(test_byte_copy),
	run_test(test_decode_list),
	run_test(test_md5_update),
	run_test(test_md5_final),
	run_test(test_md5_final_reverse),
	run_test(test_md5),
	!.

run_test(Test) :- print(Test), nl, call(Test), !.

domain(buffer, Buffer) :-
	length(Buffer, 512),
	Buffer ins 0..1.
domain(dword, Dword) :-
	length(Dword, 32),
	Dword ins 0..1.
domain(states, [S0, S1, S2, S3]) :-
	length(S0, 32),
	length(S1, 32),
	length(S2, 32),
	length(S3, 32),
	S0 ins 0..1,
	S1 ins 0..1,
	S2 ins 0..1,
        S3 ins 0..1.

% glowna funkcja md5
% parametry:
% MsgStr - lista kodow znakowych, np. "Test"
% Digest - Wynik
md5(MsgStr, Digest) :-
	% domain declarations
	domain(states, Digest),
	Length in 0..56,
	length(MsgStr, Length),

	% creating some constants
	md5_init(States),
	length(Buffer, 512),
	maplist(=(0), Buffer),

	% convert message to specially prepared list of bits
	decode_string_align(MsgStr, ByteList, ByteLength),
	append(ByteList, BitList),

	% process message
	md5_update(States, NewStates, Buffer, NewBuffer, BitList, ByteLength, 0, NewBitCount), % true rel
	md5_final(NewStates, NewBuffer, NewBitCount, Digest). % true rel
test_md5_label :-
	md5(M, D),
	label(M),
	maplist(label, D),
	print(M), nl,
	maplist(print, D).
test_md5_label_2 :-
	md5(M, D),
	maplist(label, D),
	label(M),
	print(M), nl,
	maplist(print, D).
test_md5_final_label :-
	md5_init(S),
	md5_final(S, B, BC, D),
	label(B),
	label([BC]),
	maplist(label, D).
md5_final(States, Buffer, BitCount, Digest) :-
	maplist(domain(states), [States, Digest]),
	domain(buffer, Buffer),
	BitCount in 0..65535,
	label([BitCount]),
	HighBitCount in 0..255,
	HighBitCount #= BitCount / 256,
	LowBitCount in 0..255,
	LowBitCount #= BitCount mod 256,
	byte_dec2bin(HighBitCount, HighByte),
	byte_dec2bin(LowBitCount, LowByte),
	Z = [0,0,0,0,0,0,0,0],
	append([LowByte,HighByte,Z,Z, Z,Z,Z,Z], Bits), % length(Bits, 64).
	length(Padding, 511), % it is 511 and not 512 because later a leading '1' is added
	maplist(=(0), Padding),
	Index in 0..63,
	Index #= (BitCount / 8) mod 64,
	md5_final_padlen(Index, PadLen), % PadLen in 1..64
	md5_update(States, NewStates, Buffer, NewBuffer, [ 1 | Padding ], PadLen, BitCount, NewBC), % true rel
	md5_update(NewStates, Digest, NewBuffer, _, Bits, 8, NewBC, _). %true rel

print_states([S0, S1, S2, S3]) :-
	conv_bytes_to_hex(S0), nl,
	conv_bytes_to_hex(S1), nl,
	conv_bytes_to_hex(S2), nl,
	conv_bytes_to_hex(S3), nl.

print_digest([S0, S1, S2, S3]) :-
	conv_bytes_to_hex_reverse(S0),
	conv_bytes_to_hex_reverse(S1),
	conv_bytes_to_hex_reverse(S2),
	conv_bytes_to_hex_reverse(S3), nl.

test_md5_final :-
	md5_init(States),
	char_to_dword('TSET', Test),
	conv_hex_to_dword('00000000', Zs),  % padding
	append([Test,Zs,Zs,Zs, Zs,Zs,Zs,Zs, Zs,Zs,Zs,Zs, Zs,Zs,Zs,Zs], Buffer),
	md5_final(States, Buffer, 32, [S0, S1, S2, S3]),
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3).
	% dokonczyc test
	%print_digest(Result).

test_md5_final_reverse :-
	md5_init(States),
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3),
	md5_final(States, Buffer, 32, [S0, S1, S2, S3]),
	char_to_dword('TSET', Test),
	conv_hex_to_dword('00000000', Zs),  % padding
	append([Test,Zs,Zs,Zs, Zs,Zs,Zs,Zs, Zs,Zs,Zs,Zs, Zs,Zs,Zs,Zs], Buffer).

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

decode_string_align(CharList, ByteList, Length) :-
	maplist(byte_dec2bin, CharList, BL),
	PaddingLen in 0..24,
	PaddingLen #= ((4 - Length mod 4) mod 4),
	label([PaddingLen]),
	length(Padding, PaddingLen),
	maplist(=([0,0,0,0,0,0,0,0]), Padding),
	append(BL, Padding, ByteList).

md5_init([ S0, S1, S2, S3 ]) :-
	conv_hex_to_dword( '67452301' , S0 ),
	conv_hex_to_dword( 'efcdab89' , S1 ),
	conv_hex_to_dword( '98badcfe' , S2 ),
	conv_hex_to_dword( '10325476' , S3 ).

% true rel
decode_list([], []).
decode_list(List, [ Dword | ListDwords ]) :-
	length(Dword, 32),
	Dword ins 0..1,
	append(Dword, NewList, List),
	decode_list(NewList, ListDwords).

test_decode_list :-
	conv_hex_to_dword('01010101', A),
	conv_hex_to_dword('20202020', B),
	append([A,B],Test),
	decode_list(Test, [A,B]).

char_to_dword(String, Dword) :-
	%string_length(String, Length),
	%Length is 4,
	string_to_list(String, [ C1, C2, C3, C4 ]),
	byte_dec2bin(C1, B1),
	byte_dec2bin(C2, B2),
	byte_dec2bin(C3, B3),
	byte_dec2bin(C4, B4),
	append([B4, B3, B2, B1], Dword),
	!.

test_char_to_dword :-
	char_to_dword('TEST', X),
	conv_hex_to_dword('54534554', X).

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
md5_round_constant(1, f, s11, 'd76aa478', 0).
md5_round_constant(2, f, s12, 'e8c7b756', 1).
md5_round_constant(3, f, s13, '242070db', 2).
md5_round_constant(4, f, s14, 'c1bdceee', 3).
md5_round_constant(5, f, s11, 'f57c0faf', 4).
md5_round_constant(6, f, s12, '4787c62a', 5).
md5_round_constant(7, f, s13, 'a8304613', 6).
md5_round_constant(8, f, s14, 'fd469501', 7).
md5_round_constant(9, f, s11, '698098d8', 8).
md5_round_constant(10, f, s12, '8b44f7af', 9).
md5_round_constant(11, f, s13, 'ffff5bb1', 10).
md5_round_constant(12, f, s14, '895cd7be', 11).
md5_round_constant(13, f, s11, '6b901122', 12).
md5_round_constant(14, f, s12, 'fd987193', 13).
md5_round_constant(15, f, s13, 'a679438e', 14).
md5_round_constant(16, f, s14, '49b40821', 15).
md5_round_constant(17, g, s21, 'f61e2562', 1).
md5_round_constant(18, g, s22, 'c040b340', 6).
md5_round_constant(19, g, s23, '265e5a51', 11).
md5_round_constant(20, g, s24, 'e9b6c7aa', 0).
md5_round_constant(21, g, s21, 'd62f105d', 5).
md5_round_constant(22, g, s22, '02441453', 10).
md5_round_constant(23, g, s23, 'd8a1e681', 15).
md5_round_constant(24, g, s24, 'e7d3fbc8', 4).
md5_round_constant(25, g, s21, '21e1cde6', 9).
md5_round_constant(26, g, s22, 'c33707d6', 14).
md5_round_constant(27, g, s23, 'f4d50d87', 3).
md5_round_constant(28, g, s24, '455a14ed', 8).
md5_round_constant(29, g, s21, 'a9e3e905', 13).
md5_round_constant(30, g, s22, 'fcefa3f8', 2).
md5_round_constant(31, g, s23, '676f02d9', 7).
md5_round_constant(32, g, s24, '8d2a4c8a', 12).
md5_round_constant(33, h, s31, 'fffa3942', 5).
md5_round_constant(34, h, s32, '8771f681', 8).
md5_round_constant(35, h, s33, '6d9d6122', 11).
md5_round_constant(36, h, s34, 'fde5380c', 14).
md5_round_constant(37, h, s31, 'a4beea44', 1).
md5_round_constant(38, h, s32, '4bdecfa9', 4).
md5_round_constant(39, h, s33, 'f6bb4b60', 7).
md5_round_constant(40, h, s34, 'bebfbc70', 10).
md5_round_constant(41, h, s31, '289b7ec6', 13).
md5_round_constant(42, h, s32, 'eaa127fa', 0).
md5_round_constant(43, h, s33, 'd4ef3085', 3).
md5_round_constant(44, h, s34, '04881d05', 6).
md5_round_constant(45, h, s31, 'd9d4d039', 9).
md5_round_constant(46, h, s32, 'e6db99e5', 12).
md5_round_constant(47, h, s33, '1fa27cf8', 15).
md5_round_constant(48, h, s34, 'c4ac5665', 2).
md5_round_constant(49, i, s41, 'f4292244', 0).
md5_round_constant(50, i, s42, '432aff97', 7).
md5_round_constant(51, i, s43, 'ab9423a7', 14).
md5_round_constant(52, i, s44, 'fc93a039', 5).
md5_round_constant(53, i, s41, '655b59c3', 12).
md5_round_constant(54, i, s42, '8f0ccc92', 3).
md5_round_constant(55, i, s43, 'ffeff47d', 10).
md5_round_constant(56, i, s44, '85845dd1', 1).
md5_round_constant(57, i, s41, '6fa87e4f', 8).
md5_round_constant(58, i, s42, 'fe2ce6e0', 15).
md5_round_constant(59, i, s43, 'a3014314', 6).
md5_round_constant(60, i, s44, '4e0811a1', 13).
md5_round_constant(61, i, s41, 'f7537e82', 4).
md5_round_constant(62, i, s42, 'bd3af235', 11).
md5_round_constant(63, i, s43, '2ad7d2bb', 2).
md5_round_constant(64, i, s44, 'eb86d391', 9).

% #define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
% #define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
% #define H(x, y, z) ((x) ^ (y) ^ (z))
% #define I(x, y, z) ((y) ^ ((x) | (~z)))

md5_transform(f).
md5_transform(g).
md5_transform(h).
md5_transform(i).

% md5_transform(type, x, y, z, result).
md5_transform(f, X, Y, Z, Result) :-
	[X, Y, Z, Result] ins 0..1,
	Result #<==> (X #/\ Y) #\/ ((#\X) #/\ Z).
md5_transform(g, X, Y, Z, Result) :-
	[X, Y, Z, Result] ins 0..1,
	Result #<==> (X #/\ Z) #\/ ((#\Z) #/\ Y).
md5_transform(h, X, Y, Z, Result) :-
	[X, Y, Z, Result] ins 0..1,
	Result #= (X + Y + Z) mod 2.
md5_transform(i, X, Y, Z, Result) :-
	[X, Y, Z, Result, Tmp] ins 0..1,
	Tmp #<==> X #\/ (#\Z),
	Result #= (Tmp + Y) mod 2.
md5_transform_list(Trans,X,Y,Z,R) :-
	maplist(md5_transform(Trans), X, Y, Z, R).

add(A, B, C, D, PC2, PC1, S2, S1, S0) :-
	[A, B, C, D, PC2, PC1, S2, S1, S0] ins 0..1,
	Sum #= A + B + C + D + 2*PC2 + PC1,
	S0 #= Sum mod 2,
	S1 #= (Sum / 2) mod 2,
	S2 #= Sum / 4.

add_list( [ I1 ], [ I2 ], [ I3 ], [ I4 ], C1, C2, [ S ]) :- add(I1, I2, I3, I4, 0, 0, C1, C2, S).
add_list( [ I1 | L1 ], [ I2 | L2 ], [ I3 | L3 ], [ I4 | L4 ], C1, C2, [ S | ST ]) :-
	add(I1, I2, I3, I4, PC1, PC2, C1, C2, S),
	add_list(L1, L2, L3, L4, PC1, PC2, ST).

md5_transform_list(Trans, A, B, C, D, X, S, AC, Result) :-
	S in 0..31,
	maplist(domain(dword),[A,B,C,D,X,AC,Result]),
	md5_transform(Trans),
	md5_transform_list(Trans, B, C, D, F),
	add_list(A, F, X, AC, _, _, Sum),
	label([S]),
	dword_rotate_left(S, Sum, Rotated),
	add_list(Rotated, B, Result, _).

% dword_rotate_left/4
% dword_rotate_left(C, InputList, OutputList)
% Prawdziwa relacja: OutputList to InputList o C obroconych elementow
dword_rotate_left(C, Input, Output) :-
	length(Input, 32),
	length(Output, 32),
	Input ins 0..1,
	Output ins 0..1,
	C in 0..32,
	D #= 32 - C,
	label([C, D]),
	length(Left, C),
	length(Right, D),
	Left ins 0..1,
	Right ins 0..1,
	append(Left, Right, Input),
	append(Right, Left, Output).
% prawdziwa relacja
md5_transform_states(States, BlockStr, NewStates) :-
        decode_list(BlockStr, X),
	md5_reverse_dwordlist(X, Y),
	md5_transform_states_decoded(States, Y, NewStates).
test_md5_transform_states_label :-
	md5_transform_states(S, BS, NS),
	maplist(label, S),
	label(BS),
	maplist(label, NS).
% prawdziwa relacja
md5_reverse_dwordlist([], []).
md5_reverse_dwordlist([ Dword | DwordList ], [ R | Reversed ]) :-
	md5_reverse_dword(Dword, R),
	md5_reverse_dwordlist( DwordList, Reversed ).
% md5_reverse_dword/2
% Prawdziwa relacja: Reversed to Dword z odwrotnym porzadkiem bajtow.
md5_reverse_dword(Dword, Reversed) :-
	maplist(domain(dword), [Dword, Reversed]),
	length(B0, 8),
	length(B1, 8),
	length(B2, 8),
	length(B3, 8),
	append([B0, B1, B2, B3], Dword),
	append([B3, B2, B1, B0], Reversed).
% md5_transform_states_decoded/3
% TRUE RELATION!
% States = stany inicjalne
% Dwords = kodowany komunikat
% NewStates = stany koncowe
md5_transform_states_decoded(States, Dwords, NewStates) :-
	maplist(domain(states), [States, NewStates]),
	maplist(domain(dword), Dwords),
	md5_transform_states(1, States, Dwords, Result),
	md5_add_states(States, Result, NewStates).
test_md5_transform_states_decoded_label :-
	md5_transform_states_decoded(S, D, NS),
	maplist(label, S),
	maplist(label, D),
	maplist(label, NS).
md5_add_states([S1,S2,S3,S4], [T1,T2,T3,T4], [O1,O2,O3,O4]) :-
	add_list(S1, T1, O1, _),
	add_list(S2, T2, O2, _),
	add_list(S3, T3, O3, _),
	add_list(S4, T4, O4, _).
md5_transform_states(65, States, _, States).
md5_transform_states(Round, [ A, B, C, D ], Dwords, NewStates) :-
	md5_round_constant(Round, Trans, Rotation, AC, Index),
	md5_rotate_constant(Rotation, RotValue),
	length(Dwords, 16),
	nth0(Index, Dwords, XValue),
	conv_hex_to_dword(AC, DwordAC),
	md5_transform_list(Trans, A, B, C, D, XValue, RotValue, DwordAC, Result),
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

md5_update(States, NewStates, Buffer, NewBuffer, InputBits, InputByteLen, BitCount, NewBitCount) :-
	maplist(domain(states), [States, NewStates]),
	maplist(domain(buffer), [Buffer, NewBuffer]),
	[BitCount, NewBitCount] ins 0..512, % 512 czy 511?
	md5_update0(States, NewStates, Buffer, NewBuffer, InputBits, InputByteLen, BitCount, NewBitCount).
md5_update0(States, States, Buffer, NewBuffer, Input, InputLen, BitCount, NewBitCount) :-
	Index in 0..63,
	PartLen in 1..64,
	Index #= (BitCount / 8) mod 64,
	PartLen #= 64 - Index,
	InputLen #< PartLen,
	NewBitCount #= BitCount + InputLen * 8,
	byte_copy(Buffer, Index, Input, InputLen, NewBuffer),
	!.
md5_update0(States, NewStates, Buffer, NewBuffer, Input, InputLen, BitCount, NewBitCount) :-
	Index in 0..63,
	PartLen in 1..64,
	Index #= (BitCount / 8) mod 64,
	PartLen #= 64 - Index,
	InputLen #>= PartLen,
	NewBitCount #= BitCount + InputLen * 8,
	byte_copy(Buffer, Index, Input, PartLen, TmpBuffer), % true rel
	md5_transform_states(States, TmpBuffer, NewStates), % true rel
	IgnoreLen #= PartLen * 8,
	label([IgnoreLen]),
	length(Ignore, IgnoreLen),
	append(Ignore, NewInput, Input),
	Len #= InputLen - PartLen,
	byte_copy(TmpBuffer, 0, NewInput, Len, NewBuffer), % true rel
	!.
test_md5_update :-
	test_md5_update_1,
	test_md5_update_1_reverse,
	test_md5_update_2.
test_md5_update_label :-
	md5_update(S, NS, B, NB, I, IL, BC, NBC),
	maplist(label, S), maplist(label, NS), label(B), label(NB), label(I), label([IL, BC, NBC]).
test_md5_update_label_2 :-
	md5_update(S, NS, B, NB, I, IL, BC, NBC),
	label([IL, BC, NBC]), label(I), label(NB), label(B), maplist(label, NS), maplist(label, S).
test_md5_update_1 :-
	char_to_dword('TEST', Test),
	conv_hex_to_dword('00000000', Zs),  % padding
	append([Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], Buffer),
	append([Test,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], NewBuffer),
	md5_update(_, _, Buffer, NewBuffer, Test, 4, 0, 32).
% niedoskonaly test
% sprawdza tylko dzialanie odwrotne dla braku bufora
% a nie sprawdza dzialania przy braku licznikow
test_md5_update_1_reverse :-
	char_to_dword('TEST', Test),
	conv_hex_to_dword('00000000', Zs),  % padding
	append([Test,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], NewBuffer),
	md5_update(_, _, Buffer, NewBuffer, Test, 4, 0, 32),
	append([Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], Buffer).
test_md5_update_2 :-
	md5_init(States),
	char_to_dword('TSET', Test),
	conv_hex_to_dword('00000000', Zs),
	conv_hex_to_dword('80000000', Temp),
	conv_hex_to_dword('20000000', Bits),
	append([Bits, Zs], AddBits),
	append([Test,Temp,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,  Zs,Zs], Buffer),
	append([Test,Temp,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Bits,Zs], NewBuffer),
	md5_update(States, [S0, S1, S2, S3], Buffer, NewBuffer, AddBits, 8, 448, 512),
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3).
byte_copy(BitList, ByteIndex, Data, ByteLen, NewBuffer) :-
	[ByteIndex, ByteLen] ins 0..63,
	maplist(domain(buffer), [BitList, NewBuffer]),
	(   var(Data) -> TrueDataLen in 0..63, TrueDataLen #>= ByteLen,	label([TrueDataLen]), length(Data, TrueDataLen)
	;	length(Data, TrueDataLen), TrueDataLen #>= ByteLen),
	Index #= ByteIndex * 8,
	Len #= ByteLen * 8,
	label([ByteIndex, ByteLen, Index, Len]),
	foldl(buffer(Index, Data, Len), BitList, NewBuffer, 0, _).
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
test_buffer :-
	foldl(buffer(1, [0,1,0], 2), [1,1,1,1,1], X, 0, _),
	X = [1,0,1,1,1].
test_byte_copy :-
	conv_hex_to_dword('00000000', Zs),
	conv_hex_to_dword('80000000', Temp),
	conv_hex_to_dword('20000000', Bits),
	append([Test,Temp,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,  Zs,Zs], Buffer),
	append([Test,Temp,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Bits,Zs], NewBuffer),
	byte_copy(Buffer, 56, Bits, 4, NewBuffer),
	byte_copy(Buffer, 56, Bits, T1, NewBuffer), T1 = 4,
	byte_copy(Buffer, T2, Bits, T1, NewBuffer), T2 = 56,
	byte_copy(Buffer, T2, T3, T1, NewBuffer), T3 = Bits,
	byte_copy(T4, T2, T3, T1, NewBuffer), T4 = Buffer.
test_byte_copy_label :-
	byte_copy(A, B, C, D, E),
	label(A),label([B,D]),label(C),label(E),
	byte_copy(A, B, C, D, E),
	label(E),label(C),label([B,D]),label(A).
test_md5_transform_list_f :-
	conv_hex_to_dword('67452301', A),
	conv_hex_to_dword('efcdab89', B),
	conv_hex_to_dword('98badcfe', C),
	conv_hex_to_dword('10325476', D),
	conv_hex_to_dword('54534554', X),
	conv_hex_to_dword('d76aa478', AC),
	md5_rotate_constant(s11, S),
	md5_transform_list(f, A, B, C, D, X, S, AC, Result),
	%conv_bytes_to_hex(Result),
	conv_hex_to_dword('cec2911e', Result),
	!.

% bardzo wazne - budulec calej funkcji MD5!
test_md5_transform_list_f_reverse :-
	conv_hex_to_dword('d76aa478', AC),
	conv_hex_to_dword('cec2911e', Result),
	conv_hex_to_dword('67452301', A1),
	conv_hex_to_dword('efcdab89', B1),
	conv_hex_to_dword('98badcfe', C1),
	conv_hex_to_dword('10325476', D1),
	conv_hex_to_dword('54534554', X1),
        md5_rotate_constant(s11, S),
	length(A, 32),
	%length(B, 32),
	%length(C, 32),
	%length(D, 32),
	md5_transform_list(f, A, B1, C1, D1, X1, S, AC, Result),
	A = A1,
	%B = B1,
	%C = C1,
	%D = D1,
	%X = X1,
	true.

test_md5_transform_list_g :-
	conv_hex_to_dword('f24947ec', A),
	conv_hex_to_dword('b2afe275', B),
	conv_hex_to_dword('55a9265b', C),
	conv_hex_to_dword('6c2db7db', D),
	conv_hex_to_dword('00000080', X),
	conv_hex_to_dword('f61e2562', AC),
	md5_rotate_constant(s21, S),
	md5_transform_list(g, A, B, C, D, X, S, AC, Result),
	%conv_bytes_to_hex(Result),
	conv_hex_to_dword('f551e658', Result),
	!.

test_md5_transform_list_h :-
	conv_hex_to_dword('9618ecbf', A),
	conv_hex_to_dword('644f7032', B),
	conv_hex_to_dword('d7308312', C),
	conv_hex_to_dword('bb370c61', D),
	conv_hex_to_dword('00000000', X),
	conv_hex_to_dword('fffa3942', AC),
	md5_rotate_constant(s31, S),
	md5_transform_list(h, A, B, C, D, X, S, AC, Result),
	%conv_bytes_to_hex(Result),
	conv_hex_to_dword('4a11c45b', Result),
	!.

test_md5_transform_list_i :-
	conv_hex_to_dword('677459b5', A),
	conv_hex_to_dword('b9e8c88b', B),
	conv_hex_to_dword('c0c65283', C),
	conv_hex_to_dword('8c0c96a7', D),
	conv_hex_to_dword('54534554', X),
	conv_hex_to_dword('f4292244', AC),
	md5_rotate_constant(s41, S),
	md5_transform_list(i, A, B, C, D, X, S, AC, Result),
	%conv_bytes_to_hex(Result),
	conv_hex_to_dword('8587f205', Result),
	!.
test_md5_transform_states :-
	md5_init(States),
	char_to_dword('TEST', Test),
	conv_hex_to_dword('00000000', Zs),  % padding
	conv_hex_to_dword('00000080', One), % terminator tekstu
	conv_hex_to_dword('00000020', Two), % 32 bity slowa 'TEST'
	DwordList = [Test,One,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Two,Zs],
	md5_transform_states_decoded(States, DwordList, [S0,S1,S2,S3]),
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3).
test_md5_transform_states_reverse :-
	md5_init(States),
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3),
	md5_transform_states_decoded(States, DwordList, [S0,S1,S2,S3]),
	char_to_dword('TEST', Test),
	conv_hex_to_dword('00000000', Zs),  % padding
	conv_hex_to_dword('00000080', One), % terminator tekstu
	conv_hex_to_dword('00000020', Two), % 32 bity slowa 'TEST'
	%label(DwordList),
	%print(DwordList).
	DwordList = [Test,One,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Two,Zs].
test_output(Output) :-
	md5_init(States),
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3),
	md5_transform_states_decoded(States, DwordList, [S0,S1,S2,S3]),
	char_to_dword('TEST', _Test),
	conv_hex_to_dword('00000000', Zs),  % padding
	conv_hex_to_dword('00000080', One), % terminator tekstu
	conv_hex_to_dword('00000020', Two), % 32 bity slowa 'TEST'

	%maplist(label, DwordList),
	%DwordList = [Test,One,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Two,Zs].
	DwordList = [Output,One,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Two,Zs].
	%Output = DwordList.
test_md5_transform_list :-
	Trans = f,
	Result = [0,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,0,1,1,1,1,0,1,0,1,1,1,1,0],
	char_to_dword('TEST', T),
	% forward
	md5_transform_list(Trans, T, T, T, T, T, 5, T, Result),
	% reverse
	md5_transform_list(Trans, A, B, C, D, X, S, AC, Result),
	A = B,
	B = C,
	C = D,
	D = X,
	X = AC,
	S = 5.
test_conv_hex_to_dword :-
	conv_hex_to_dword('67452301', A),
	conv_hex_to_dword('efcdab89', B),
	add_list(A, B, C, 1),
	%conv_bytes_to_hex(A),
	%conv_bytes_to_hex(B),
	%conv_bytes_to_hex(C),
	conv_hex_to_dword('5712ce8a', C).
test_conv_hex_to_dword_reverse :-
	conv_hex_to_dword_reverse('0789ABcd', A),
	conv_hex_to_dword_reverse('12345678', B),
	add_list(A, B, C, 1),
	%conv_bytes_to_hex_reverse(A),
	%conv_bytes_to_hex_reverse(B),
	%conv_bytes_to_hex_reverse(C),
	conv_hex_to_dword_reverse('19BD0146', C).
demo_md5 :-
	print('Type text to be hashed: '),
	current_input(Stream),
	read_line_to_codes(Stream, Codes),
	md5(Codes, Digest),
	print('MD5 hash: '),
	print_digest(Digest),
	!.
test_md5 :-
	md5("TEST", [S0, S1, S2, S3]),
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3).
	%print_digest(States).
test_md5_reverse :-
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3),
	md5(Word, [S0, S1, S2, S3]),
	print(Word), nl.











