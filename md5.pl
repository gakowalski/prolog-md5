% md5
% wersja 1 - prolog bez CLP, implementacja raczej niepelna relacja
% wersja 2 - prolog z CLP, pelne relacje, ale trudne etykietowanie
% wersja 3 - dalsze zmiany w logice

start :-
	use_module(library(clpfd)).

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

dword_dec2bin(Dec, Dword) :-
	domain(dword, Dword),
	Dec in 0..4294967295,
	Positions = [2147483648, 1073741824, 536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608,
		    4194304, 2097152, 1048576, 524288, 262144, 131072, 65536, 32768, 16384, 8192, 4096, 2048, 1024,
		    512, 256, 128, 64, 32, 16, 8, 4, 2 ,1],
	scalar_product(Positions, Dword, #=, Dec).
dword_and(A, B, And) :-
	dword_dec2bin(A, AD),
	dword_dec2bin(B, BD),
	maplist(dword_and0, AD, BD, AndD),
	dword_dec2bin(And, AndD).
dword_and0(A, B, And) :- A #/\ B #= And.
dword_or(A, B, Or) :-
	dword_and(A, B, And),
	Or #= A + B - And.
dword_xor(A, B, Xor) :-
	dword_and(A, B, And),
	Xor #= A + B - 2*And.
dword_not(A, NotA) :-
	NotA #= 0xFFFFFFFF - A.

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

dword_dec2hexstr(Dec, HexStr) :-
	string_to_list(HexStr, [C0, C1, C2, C3, C4, C5, C6, C7]),
	hex_digit(D0, C0, _),
	hex_digit(D1, C1, _),
	hex_digit(D2, C2, _),
	hex_digit(D3, C3, _),
	hex_digit(D4, C4, _),
	hex_digit(D5, C5, _),
	hex_digit(D6, C6, _),
	hex_digit(D7, C7, _),
	Positions = [268435456, 16777216, 1048576, 65536, 4096, 256, 16, 1],
	scalar_product(Positions, [D0, D1, D2, D3, D4, D5, D6, D7], #=, Dec).

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

domain(byte, Byte) :-
	Byte ins 0..255.
domain(buffer, Buffer) :-
	length(Buffer, 64),
	Buffer ins 0..255.
domain(dword, Dword) :-
	length(Dword, 32),
	Dword ins 0..1.
domain(states, [S0, S1, S2, S3]) :-
	[S0, S1, S2, S3] ins 0..0xFFFFFFFF.
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
	% md5_init(States),
	States = [ 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476 ],
	domain(buffer, Buffer),
	maplist(=(0), Buffer),

	% add padding zeros
	dword_align(MsgStr, Aligned, Length),

	% process message
	md5_update(States, NewStates, Buffer, NewBuffer, Aligned, Length, 0, NewBitCount), % true rel
	md5_final(NewStates, NewBuffer, NewBitCount, Digest). % true rel
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

dword_align(Input, Output, Length) :-
	% dodac domeny dla wszystkich argumentow
	PaddingLen in 0..24,
	PaddingLen #= ((4 - Length mod 4) mod 4),
	label([PaddingLen]),
	length(Padding, PaddingLen),
	maplist(=(0), Padding),
	append(Input, Padding, Output).

md5_init([ S0, S1, S2, S3 ]) :-
	dword_dec2bin(0x67452301, S0),
	dword_dec2bin(0xefcdab89, S1),
	dword_dec2bin(0x98badcfe, S2),
	dword_dec2bin(0x10325476, S3).

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
md5_transform_list(Trans,Xx,Yy,Zz,Rr) :-
	dword_dec2bin(Xx, X),
	dword_dec2bin(Yy, Y),
	dword_dec2bin(Zz, Z),
	maplist(md5_transform(Trans), X, Y, Z, R),
	dword_dec2bin(Rr, R).
md5_transform_list(Trans, A, B, C, D, X, S, AC, Result) :-
	S in 0..31,
	[A,B,C,D,Result,X,AC] ins 0..4294967295,
	md5_transform(Trans),
	md5_transform_list(Trans, B, C, D, F),
	Sum #= (A + F + X + AC) mod 4294967296,
	S2 #= 32 - S,
	label([S, S2]),
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
	% TO DO: domena dla Bytes
	md5_bytes_to_dwords(Bytes, Dwords),
	md5_transform_states(1, States, Dwords, Result),
	md5_add_states(States, Result, NewStates).
md5_add_states([S1,S2,S3,S4], [T1,T2,T3,T4], [O1,O2,O3,O4]) :-
	O1 #= (S1 + T1) mod 4294967296,
	O2 #= (S2 + T2) mod 4294967296,
	O3 #= (S3 + T3) mod 4294967296,
	O4 #= (S4 + T4) mod 4294967296.
md5_transform_states(65, States, _, States).
md5_transform_states(Round, [ A, B, C, D ], Dwords, NewStates) :-
	md5_round_constant(Round, Trans, Rotation, AC, Index),
	md5_rotate_constant(Rotation, RotValue),
	length(Dwords, 16),
	nth0(Index, Dwords, XValue),
	md5_transform_list(Trans, A, B, C, D, XValue, RotValue, AC, Result),
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
	label([PartLen]),
	length(Ignore, PartLen),
	append(Ignore, NewInput, Input),
	Len #= InputLen - PartLen,
	byte_copy(TmpBuffer, 0, NewInput, Len, NewBuffer), % true rel
	!.
test_md5_update :-
	test_md5_update_1,
	test_md5_update_1_reverse.
	%test_md5_update_2.
test_md5_update_label :-
	md5_update(S, NS, B, NB, I, IL, BC, NBC),
	maplist(label, S), maplist(label, NS), label(B), label(NB), label(I), label([IL, BC, NBC]).
test_md5_update_label_2 :-
	md5_update(S, NS, B, NB, I, IL, BC, NBC),
	label([IL, BC, NBC]), label(I), label(NB), label(B), maplist(label, NS), maplist(label, S).
test_md5_update_1 :-
	char_to_dword('TEST', Test),
	bits_to_bytes(Test, BTest),
	conv_hex_to_dword('00000000', Zs),  % padding
	append([Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], Buffer),
	append([Test,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], NewBuffer),
	bits_to_bytes(Buffer, B1),
	bits_to_bytes(NewBuffer, B2),
	md5_update(_, _, B1, B2, BTest, 4, 0, 32).
% niedoskonaly test
% sprawdza tylko dzialanie odwrotne dla braku bufora
% a nie sprawdza dzialania przy braku licznikow
test_md5_update_1_reverse :-
	char_to_dword('TEST', Test),
	bits_to_bytes(Test, BTest),
	conv_hex_to_dword('00000000', Zs),  % padding
	append([Test,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], NewBuffer),
	bits_to_bytes(NewBuffer, B2),
	md5_update(_, _, B1, B2, BTest, 4, 0, 32),
	append([Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], Buffer),
	bits_to_bytes(Buffer, B1).
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
bits_to_bytes([], []).
bits_to_bytes([ A,B,C,D,E,F,G,H | BitList ], [ [A,B,C,D,E,F,G,H] | ByteList ]) :-
	bits_to_bytes(BitList, ByteList).
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
test_buffer :-
	foldl(buffer(1, [0,1,0], 2), [1,1,1,1,1], X, 0, _),
	X = [1,0,1,1,1].
test_byte_copy :-
	conv_hex_to_dword('00000000', Zs),
	conv_hex_to_dword('80000000', Temp), % trzeba zamienic na bajty!
	conv_hex_to_dword('20000000', Bits),
	Z = [0,0,0,0, 0,0,0,0],

	append([Temp,Temp,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,  Zs,Zs], Buffer),
	append([Temp,Temp,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Bits,Zs], NewBuffer),
	!,
	byte_copy(Buffer, 56, [[0,0,1,0, 0,0,0,0],Z,Z,Z], 4, NewBuffer),
	byte_copy(Buffer, 56, [[0,0,1,0, 0,0,0,0],Z,Z,Z], T1, NewBuffer), T1 = 4,
	byte_copy(Buffer, T2, [[0,0,1,0, 0,0,0,0],Z,Z,Z], T1, NewBuffer), T2 = 56,
	byte_copy(Buffer, T2, T3, T1, NewBuffer), T3 = [[0,0,1,0, 0,0,0,0],Z,Z,Z],
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
	print(Word), nl.











