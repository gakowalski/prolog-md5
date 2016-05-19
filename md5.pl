:- use_module(library(clpb)).

byte(Var) :- length(Var, 8).
word(Var) :- length(Var, 16).
dword(Var) :- length(Var, 32).

combine(H, L, V) :-
	byte(H), byte(L), word(V),
	append([H, L], V).
combine(H, L, V) :-
	word(H), word(L), dword(V),
	append([H, L], V).

% add (A, B, Sum, NextCarry) realizuje sume dwoch bitow
add(A, B, S, C) :- sat(A#B=:=S), sat(A*B=:=C).
% add (A, B, PrevCarry, Sum, NextCarry) realizuje sume dwoch bitow
add(A, B, T, S, C) :- add(A, B, S1, C1), add(S1, T, S, C2), sat(C1+C2=:=C).

% true relation
add_list( [ A ], [ B ], [ S ], C ) :- add(A, B, S, C).
add_list( [ A | AT ], [ B | BT ], [ S | ST ], C ) :-
	add(A, B, Prev, S, C),
	add_list(AT, BT, ST, Prev).

% true relation
and_list([ X ], [ Y ], [ Z ]) :- sat(X*Y=:=Z).
and_list([ X | XT ], [ Y | YT ], [ Z | ZT ]) :-
	sat(X*Y=:=Z), and_list(XT, YT, ZT).

% true relation
or_list([ X ], [ Y ], [ Z ]) :- sat(X+Y=:=Z).
or_list([ X | XT ], [ Y | YT ], [ Z | ZT ]) :-
	sat(X+Y=:=Z), or_list(XT, YT, ZT).

% true relation
xor_list([ X ], [ Y ], [ Z ]) :- sat(X#Y=:=Z).
xor_list([ X | XT ], [ Y | YT ], [ Z | ZT ]) :-
	sat(X#Y=:=Z), xor_list(XT, YT, ZT).

% true relation
not_list([ X ], [ Y ]) :- sat(X =:= ~Y).
not_list([ X | XT ], [ Y | YT ]) :-
	sat(X =:= ~Y), not_list(XT, YT).

eq_list([], []).
eq_list([A | AT], [B | BT]) :- sat(A=:=B), eq_list(AT, BT).

gt_list([A | _], [B | _]) :- sat(A > B).
gt_list([A | AT], [B | BT]) :- sat(A=:=B), gt_list(AT, BT).

lt_list([A | _], [B | _]) :- sat(A < B).
lt_list([A | AT], [B | BT]) :- sat(A=:=B), lt_list(AT, BT).


% hex_digit(digit, char_code, bit_list)
% cyfry
hex_digit(0, 48, [ 0, 0, 0, 0 ]).
hex_digit(1, 49, [ 0, 0, 0, 1 ]).
hex_digit(2, 50, [ 0, 0, 1, 0 ]).
hex_digit(3, 51, [ 0, 0, 1, 1 ]).
hex_digit(4, 52, [ 0, 1, 0, 0 ]).
hex_digit(5, 53, [ 0, 1, 0, 1 ]).
hex_digit(6, 54, [ 0, 1, 1, 0 ]).
hex_digit(7, 55, [ 0, 1, 1, 1 ]).
hex_digit(8, 56, [ 1, 0, 0, 0 ]).
hex_digit(9, 57, [ 1, 0, 0, 1 ]).
% duze litery
hex_digit(10, 65, [ 1, 0, 1, 0 ]).
hex_digit(11, 66, [ 1, 0, 1, 1 ]).
hex_digit(12, 67, [ 1, 1, 0, 0 ]).
hex_digit(13, 68, [ 1, 1, 0, 1 ]).
hex_digit(14, 69, [ 1, 1, 1, 0 ]).
hex_digit(15, 70, [ 1, 1, 1, 1 ]).
% male litery
hex_digit(10, 97, [ 1, 0, 1, 0 ]).
hex_digit(11, 98, [ 1, 0, 1, 1 ]).
hex_digit(12, 99, [ 1, 1, 0, 0 ]).
hex_digit(13, 100, [ 1, 1, 0, 1 ]).
hex_digit(14, 101, [ 1, 1, 1, 0 ]).
hex_digit(15, 102, [ 1, 1, 1, 1 ]).

% byte_dec2bin/2
% byte_dec2bin(Decimal, Binary)
% true relation
% Relacja istnieje, gdy Decimal i Binary reprezentuja ta sama wartosc
% bajtu.
byte_dec2bin(Decimal, Binary) :-
	between(0, 255, Decimal),
	length(Binary, 8),
	labeling(Binary),
	Binary = [A, B, C, D, E, F, G, H],
	Decimal is A*128+B*64+C*32+D*16+E*8+F*4+G*2+H.

word_dec2bin(Decimal, Binary) :-
	between(0, 0xffff, Decimal),
	word(Binary),
	labeling(Binary),
	combine(B2, B1, Binary),
	byte_dec2bin(D1, B1),
	byte_dec2bin(D2, B2),
	Decimal is D2*256+D1.

test_byte_dec2bin :- byte_dec2bin(54, [ 0, 0, 1, 1, 0, 1, 1, 0 ]).

% uwaga do samego siebie: istnieje relacji oznacza, ze rzecz musi byc
% zapisywalna takze jako dane, jako "tabelka relacji"

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

conv_hex_to_dword2(HexStr, List) :-
	length(Chars, 8),
	length(List, 32),
	labeling(List),
	maplist(between(48, 102), Chars),
%	forall(member(M, Chars), hex_digit(_, M, B)),
	string_to_list(HexStr, Chars).


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
	run_test(test_md5_transform_list_f),
	run_test(test_md5_transform_list_g),
	run_test(test_md5_transform_list_h),
	run_test(test_md5_transform_list_i),
	run_test(test_md5_transform_states),
	run_test(test_buffer),
	run_test(test_byte_copy),
	run_test(test_decode_list),
	run_test(test_md5_update),
	run_test(test_md5_final),
	run_test(test_md5),
	!.

run_test(Test) :- print(Test), nl, call(time(Test)), !.

test_conv_hex_to_dword :-
	conv_hex_to_dword('67452301', A),
	conv_hex_to_dword('efcdab89', B),
	add_list(A, B, C, 1),
	conv_hex_to_dword('5712ce8a', C).

test_conv_hex_to_dword_reverse :-
	conv_hex_to_dword_reverse('0789ABcd', A),
	conv_hex_to_dword_reverse('12345678', B),
	add_list(A, B, C, 1),
	conv_hex_to_dword_reverse('19BD0146', C).

% glowna funkcja md5
% parametry:
% MsgStr - lista kodow znakowych, np. "Test"
% Digest - Wynik
md5(MsgStr, Digest) :-
	length(Buffer, 512),
	maplist(=(0), Buffer),
	word(BitCount),
	maplist(=(0), BitCount),

	decode_string_align(MsgStr, ByteList, Length),
	append(ByteList, BitList),
	md5_init(States),
	md5_update(States, NewStates, Buffer, NewBuffer, BitList, Length, BitCount, NewBitCount),
	md5_final(NewStates, NewBuffer, NewBitCount, Digest),
	!.

exp_md5 :-
	md5(`TEST`, States),
	print_digest(States).


demo_md5 :-
	print('Type text to be hashed: '),
	current_input(Stream),
	read_line_to_codes(Stream, Codes),
	md5(Codes, Digest),
	print('MD5 hash: '),
	print_digest(Digest),
	!.

test_md5 :-
	md5(`TEST`, [S0, S1, S2, S3]),
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3).
	%print_digest(States).

md5_reverse_bytelist([], []).
md5_reverse_bytelist([ B0,B1,B2,B3 | ByteList ], [ B3,B2,B1,B0 | Reversed ]) :-
	md5_reverse_bytelist(ByteList, Reversed).


md5_final(States, Buffer, BitCount, Digest) :-
	length(BitsPadding, 48),
	maplist(=(0), BitsPadding),
	length(Padding, 512),
	length(PaddingZeros, 511),
	maplist(=(0), PaddingZeros),
	append([1], PaddingZeros, Padding),
	HighBitCount is BitCount // 256,
	LowBitCount is BitCount mod 256,
	byte_dec2bin(HighBitCount, HighByte),
	byte_dec2bin(LowBitCount, LowByte),
	append([LowByte,HighByte,BitsPadding], Bits),
	Index is (BitCount // 8) mod 64,
	md5_final_padlen(Index, PadLen),
	byte_dec2bin(PadLen, PadLenB),
	md5_update(States, NewStates, Buffer, NewBuffer, Padding, PadLenB, BitCount, NewBC),
	md5_update(NewStates, Digest, NewBuffer, _, Bits, [0,0,0,0, 1,0,0,0], NewBC, _).

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
	append([Test,Zs,Zs,Zs, Zs,Zs,Zs,Zs, Zs,Zs,Zs,Zs, Zs,Zs,Zs,Zs], Buffer)
	.

% (index < 56) ? (56 - index) : (120 - index);
md5_final_padlen(Index, PadLen) :-
	Index < 56,
	PadLen is 56 - Index.
md5_final_padlen(Index, PadLen) :-
	Index >= 56,
	PadLen is 120 - Index.

decode_string([], [], Length) :-
	byte(Length),
	maplist(=(0), Length).
decode_string([ Char | CharList ], [ Byte | ByteList ], Length) :-
	byte_dec2bin(Char, Byte),
	decode_string(CharList, ByteList, TmpLength),
	add_list(TmpLength, [0,0,0,0, 0,0,0,1], Length, _).

decode_string_align(CharList, ByteList, Length) :-
	decode_string(CharList, BL, Length),
	align_byte_list(BL, ByteList, Length).

align_byte_list(ByteList, NewList, Length) :-
	%PaddingLen is (4 - Length mod 4) mod 4,
	Length = [_,_,_,_, _,_,A,B],
	add_list([_,C,D], [0,A,B], [1,0,0], 0),
	PaddingLen = [0,0,0,0, 0,0,C,D],
	align_byte_list_append(ByteList, NewList, PaddingLen).

align_byte_list_append(BL, BL, [0,0,0,0, 0,0,0,0]) :- !.
align_byte_list_append(BL, NL, PL) :-
	append(BL, [0,0,0,0, 0,0,0,0], Tmp),
	%NewPadding is PL - 1,
	add_list(NewPadding, [0,0,0,0, 0,0,0,1], PL, 0),
	align_byte_list_append(Tmp, NL, NewPadding).

md5_init([ S0, S1, S2, S3 ]) :-
	conv_hex_to_dword( '67452301' , S0 ),
	conv_hex_to_dword( 'efcdab89' , S1 ),
	conv_hex_to_dword( '98badcfe' , S2 ),
	conv_hex_to_dword( '10325476' , S3 ),
	!.

decode_list([], []) :- !.
decode_list(List, [ Dword | ListDwords ]) :-
	divide_list(32, List, Dword, NewList),
	decode_list(NewList, ListDwords).

test_decode_list :-
	conv_hex_to_dword('01010101', A),
	conv_hex_to_dword('20202020', B),
	append([A,B],Test),
	decode_list(Test, [A,B]).

char_to_dword(String, Dword) :-
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
% md5_transform(type, x, y, z, result).

md5_transform(f, X, Y, Z, Res) :- sat(Res =:= (X*Y)+((~X)*Z)).
md5_transform(g, X, Y, Z, Res) :- sat(Res =:= (X*Z)+(Y*(~Z))).
md5_transform(h, X, Y, Z, Res) :- sat(Res =:= X#Y#Z).
md5_transform(i, X, Y, Z, Res) :- sat(Res =:= Y#(X+(~Z))).

% pojedyncza transformacja - makra F, G, H, I
md5_transform_list(Trans, [ X ], [ Y ], [ Z ], [ R ]) :- md5_transform(Trans, X, Y, Z, R).
md5_transform_list(Trans, [ X | XT ], [ Y | YT ], [ Z | ZT ], [ R | Result]) :-
	md5_transform(Trans, X, Y, Z, R),
	md5_transform_list(Trans, XT, YT, ZT, Result).

% zlozona transformacja - makra FF, GG, HH, II
% zaimplementowane dzialanie odwrotne, ale niesprawdzone
% bo strasznie czasochlonna petla
md5_transform_list(Trans, A, B, C, D, X, S, AC, Result) :-
	(   var(Result) ->
	md5_transform_list(Trans, B, C, D, F),
	add_list(X, AC, XaddAC, _),
	add_list(F, XaddAC, FaddXaddAC, _),
	add_list(A, FaddXaddAC, Sum, _),
	dword_rotate_left(S, Sum, Rotated),
	add_list(Rotated, B, Result, _)
	;   add_list(Rotated, B, Result, _),
	    dword_rotate_left(S, Sum, Rotated),
	    add_list(X, AC, XaddAC, _),
	    add_list(F, XaddAC, FaddXaddAC, _),
	    add_list(A, FaddXaddAC, Sum, _),
	    md5_transform_list(Trans, B, C, D, F)
	).

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

%
% divide_list/4
% divide_list(C, List, Left, Right).
%
% Relacja prawdziwa gdy List sklada sie z dwoch list: Left o dlugosci C
% oraz Right.
%
% Dzielenie listy o dlugosci N na dwie listy: liste Left o dlugosci C,
% oraz Right o dlugosci N-C. Relacja nie wymaga znajomosci dlugosci N,
% ale wymaga znajomosci dlugosci C, aby odliczyc miejsce rozerwania
% list. Wartosc C moze przyjac wartosci nieujemne - w tym zero.
%
% W przypadku relacyjnej wersji predykatu nalezy uwazac na mozliwosc
% wejscia w petle nieskonczona przy jednoczesnej niewiadomej zarowno
% dlugosci glownej listy jak i pola Right.
%
% Example: divide_list(2, [1,2,3,4], [1,2], [3,4]).

divide_list(C, List, Left, Right) :-
	append([Left, Right], List),
	length(Left, C).

% true relation
rol_list([H|T], R) :- append(T, [H], R).

% dword_rotate_left/4
% dword_rotate_left(C, InputList, OutputList)
% falszywa relacja, ale zachowujaca ograniczona i wystarczajaca
% odwracalnosc - OutputList to InputList o C obroconych elementow
% BitCounter zawsze musi byc podany Zawsze musi byc podana
% przynajmnie jedna z list: wejsciowa lub wynikowa

dword_rotate_left(0, Input, Input).
dword_rotate_left(C, Input, Output) :-
	C > 0,
	C < 32,
	(   nonvar(Input)
	->
	divide_list(C, Input, Left, Right),
	append(Right, Left, Output)
	;
	D is 32 - C,
	divide_list(D, Output, Left, Right),
	append(Right, Left, Input)).


md5_transform_states(States, BlockStr, NewStates) :-
        decode_list(BlockStr, X),
	md5_reverse_dwordlist(X, Y),
	md5_transform_states_decoded(States, Y, NewStates).

md5_reverse_dwordlist([], []).
md5_reverse_dwordlist([ Dword | DwordList ], [ R | Reversed ]) :-
	md5_reverse_dword(Dword, R),
	md5_reverse_dwordlist( DwordList, Reversed ).

md5_reverse_dword(Dword, Reversed) :-
	divide_list(8, Dword, B0, Tmp),
	divide_list(8, Tmp, B1, Tmp2),
	divide_list(8, Tmp2, B2, B3),
	append([B3, B2, B1, B0], Reversed).

% States to lista czterech list po 4 bajty kazda
md5_transform_states_decoded(States, Dwords, NewStates) :-
	md5_transform_states(1, States, Dwords, Result),
	md5_add_states(States, Result, NewStates).

md5_add_states([S1,S2,S3,S4], [T1,T2,T3,T4], [O1,O2,O3,O4]) :-
	add_list(S1, T1, O1, _),
	add_list(S2, T2, O2, _),
	add_list(S3, T3, O3, _),
	add_list(S4, T4, O4, _).

md5_transform_states(65, States, _, States).
md5_transform_states(Round, [ A, B, C, D ], X, NewStates) :-
	md5_round_constant(Round, Trans, Rotation, AC, Index),
	md5_rotate_constant(Rotation, RotValue),
	length(X, 16),
	nth0(Index, X, XValue),
	conv_hex_to_dword(AC, DwordAC),
	md5_transform_list(Trans, A, B, C, D, XValue, RotValue, DwordAC, Result),
	NewRound is Round + 1,
	md5_transform_states(NewRound, [ D, Result, B, C ], X, NewStates).

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

% jeszcze nie dziala, zawiesza sie
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
	DwordList = [Test,One,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Two,Zs].

% md5_update(
%    States, NewStates,
%    Buffer, NewBuffer,
%    Input, InputLen,
%    InCount0, OutCount0,
%    InCount1, OutCount1).
md5_update(States, States, Buffer, NewBuffer, Input, InputLen0, BitCount, NewBitCount) :-
	byte(InputLen0),
	word(BitCount),
	word(NewBitCount),
	length(Index, 6),
	length(PartLen, 7),

	byte_dec2bin(InputLen, InputLen0),

	%Index is (BitCount // 8) mod 64,
	append([[_,_,_,_,_,_,_], Index, [_, _, _]], BitCount),

	%PartLen is 64 - Index,
	IndexTmp = [0 | Index],
	add_list(PartLen, IndexTmp, [1,0,0,0,0,0,0], 0),

	%InputLen < PartLen,
	PartLenTmp = [0 | PartLen],
	lt_list(InputLen0, PartLenTmp),

	%NewBitCount is BitCount + InputLen * 8,
	append([[0,0,0,0, 0], InputLen0, [0,0,0]], InputLenBits),
	add_list(BitCount, InputLenBits, NewBitCount, _),

	byte(IndexDec),
	append([[0,0], Index], IndexDec),
	byte_dec2bin(Index0, IndexDec),

	byte_copy(Buffer, Index0, Input, InputLen, NewBuffer),
	!.

md5_update(States, NewStates, Buffer, NewBuffer, Input, InputLen0, BitCount, NewBitCount) :-
	byte(InputLen0),
	word(BitCount),
	word(NewBitCount),
	byte(Index),
	length(Index, 6),
	length(PartLen, 7),

	byte_dec2bin(InputLen, InputLen0),

	%Index is (BitCount // 8) mod 64,
	append([[_,_,_,_,_,_,_], Index, [_, _, _]], BitCount),

	%PartLen is 64 - Index,
	IndexTmp = [0 | Index],
	add_list(PartLen, IndexTmp, [1,0,0,0,0,0,0], 0),

	%InputLen < PartLen,
	PartLenTmp = [0 | PartLen],
	\+(lt_list(InputLen0, PartLenTmp)),

	%NewBitCount is BitCount + InputLen * 8,
	append([[0,0,0,0, 0], InputLen0, [0,0,0]], InputLenBits),
	add_list(BitCount, InputLenBits, NewBitCount, _),

	byte(IndexDec),
	append([[0,0], Index], IndexDec),
	byte_dec2bin(Index0, IndexDec),

	byte(PartLenDec),
	append([[0], PartLen], PartLenDec),
	byte_dec2bin(PartLen0, PartLenDec),

	byte_copy(Buffer, Index0, Input, PartLen0, TmpBuffer),
	md5_transform_states(States, TmpBuffer, NewStates),
	NewPartLen is PartLen0 * 8,
	divide_list(NewPartLen, Input, _, NewInput),
	Len is InputLen - PartLen0,
	byte_copy(TmpBuffer, 0, NewInput, Len, NewBuffer),
	!.

test_md5_update :-
	test_md5_update_1,
	test_md5_update_1_reverse,
	test_md5_update_2.

test_md5_update_1 :-
	char_to_dword('TEST', Test),
	conv_hex_to_dword('00000000', Zs),  % padding
	length(Buffer, 512),
	maplist(=(0), Buffer),
	append([Test,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], NewBuffer),
	md5_update(_, _, Buffer, NewBuffer, Test,
		   [0,0,0,0, 0,1,0,0],
		   [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
		   [0,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,0,0]).

% niedoskonaly test
% sprawdza tylko dzialanie odwrotne dla braku bufora
% a nie sprawdza dzialania przy braku licznikow
test_md5_update_1_reverse :-
	char_to_dword('TEST', Test),
	conv_hex_to_dword('00000000', Zs),  % padding
	append([Test,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs], NewBuffer),
	% 4, 0, 32
	md5_update(_, _, Buffer, NewBuffer, Test,
		   [0,0,0,0, 0,1,0,0],
		   [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
		   [0,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,0,0]),
	length(Buffer, 512),
	maplist(=(0), Buffer).

test_md5_update_2 :-
	md5_init(States),
	char_to_dword('TSET', Test),
	conv_hex_to_dword('00000000', Zs),
	conv_hex_to_dword('80000000', Temp),
	conv_hex_to_dword('20000000', Bits),
	append([Bits, Zs], AddBits),
	append([Test,Temp,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,  Zs,Zs], Buffer),
	append([Test,Temp,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Zs,Bits,Zs], NewBuffer),
	md5_update(States, [S0, S1, S2, S3], Buffer, NewBuffer, AddBits,
		   [0,0,0,0, 1,0,0,0],
		   [0,0,0,0, 0,0,0,1, 1,1,0,0, 0,0,0,0],
		   [0,0,0,0, 0,0,1,0, 0,0,0,0, 0,0,0,0]),
	conv_hex_to_dword('4bd93b03', S0),
	conv_hex_to_dword('e4d76811', S1),
	conv_hex_to_dword('c344d6f0', S2),
	conv_hex_to_dword('bf355ec9', S3).

% w miare odwracalna relacja, aczkolwiek wady takie jak przy
% buffer/5
% znana wada: przy odwracaniu potafi znalezc rozwiazanie np.
% ByteIndex = 1, Data = [], ByteLen = 0
% chociaz nie powinno to powodowac zadnych dalszych problemow
byte_copy(BitList, ByteIndex, Data, ByteLen, NewBuffer) :-
	(   nonvar(BitList) ->
	Index is ByteIndex * 8,
	Len is ByteLen * 8,
	    buffer(BitList, Index, Data, Len, NewBuffer)
	;   buffer(BitList, Index, Data, Len, NewBuffer),
	    ByteIndex is Index / 8,
	    ByteLen is Len / 8,
	    integer(ByteIndex),
	    integer(ByteLen)
	).


test_byte_copy :-
	byte_copy([1,1,1,1,0,0,0,0, 1,1,1,1,0,0,0,0], 1, [1,0,1,0, 1,0,1,0], 1, X),
	X = [1,1,1,1,0,0,0,0, 1,0,1,0,1,0,1,0].

% wydaje sie byc w miare poprawna relacja
% tzn. wymagana jest obecnosc co najmniej jednego: Buffer lub NewBuffer
% aczkolwiek z liczeniu "od przodu" wymagana jest dlugosc danych
% a w liczeniu "od tylu" zaklada sie, ze dlugosc Data wynosi dokladnie
% DataLen, co nie zawsze jest prawda (np. gdy wycinamy fragment z
% Paddingu)

buffer(Buffer, Index, Data, DataLen, NewBuffer) :-
	(   nonvar(Buffer) ->
	divide_list(Index, Buffer, Left, CenterAndRight),
	divide_list(DataLen, CenterAndRight, _, Right),
	divide_list(DataLen, Data, ProperData, _),
	append( [Left, ProperData, Right], NewBuffer)
	;
	divide_list(Index, NewBuffer, Left, CenterAndRight),
	divide_list(DataLen, CenterAndRight, Data, Right),
	length(OrigData, DataLen),
	append( [Left, OrigData, Right], Buffer )
	).

test_buffer :-
	buffer([1,2,3,4,5], 2, [6,7], 2, X),
	X = [1,2,6,7,5].


addx :-
	A = [0,1,0,1,1,0,0,1,1,0,1],
	print('A  = '), print(A), nl,
	B = [0,0,0,1,1,0,1,0,1,0,1],
	print('B  = '), print(B), nl,
	add_list(A, B, S, _),
	print('S  = '), print(S), nl, nl,
	and_list(A, B, Top),
	print('a  = '), print(Top), nl,
	or_list(A, B, Bottom),
	print('b  = '), print(Bottom), nl, nl,
        rol_list(Top, RTop),
	xor_list(Top, RTop, Tmp1),
	and_list(Tmp1, Top, C0),
	% tam, gdzie w C0 jest 1, tam w Sumie na pewno bedzie 0
	print('C0 = '), print(C0), nl,
	and_list(Top, RTop, C1),
	% tam, gdzie w C1 jest 1, tam w Sumie na penwo bedzie 1
	print('C1 = '), print(C1), nl,
	not_list(A, NotA),
	not_list(B, NotB),
	and_list(NotA, NotB, PossibleC),
	print('CP = '), print(PossibleC), nl.













