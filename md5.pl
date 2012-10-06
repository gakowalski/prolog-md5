% and (A, B, Result) realizuje Result = A and B
and(0, 0, 0).
and(0, 1, 0).
and(1, 0, 0).
and(1, 1, 1).
% or (A, B, Result) realizuje Result = A or B
or(0, 0, 0).
or(0, 1, 1).
or(1, 0, 1).
or(1, 1, 1).
% xor (A, B, Result) realizuje Result = A xor B
xor(0, 0, 0).
xor(0, 1, 1).
xor(1, 0, 1).
xor(1, 1, 0).
% nor (A, Result) realizuje Result = not A
not(0, 1).
not(1, 0).
% add (A, B, Sum, NextCarry) realizuje sume dwoch bitow
% add(A, B, S, C) :- xor(A, B, S), and(A, B, C).
add(0, 0, 0, 0).
add(0, 1, 1, 0).
add(1, 0, 1, 0).
add(1, 1, 0, 1).
% add (A, B, PrevCarry, Sum, NextCarry) realizuje sume dwoch bitow
% add(A, B, T, S, C) :- add(A, B, S1, C1), add(S1, T, S, C2), or(C1, C2, C).
% czy ostatni OR mo�e by� zamieniony na XOR?
add(0, 0, 0, 0, 0).
add(0, 1, 0, 1, 0).
add(1, 0, 0, 1, 0).
add(1, 1, 0, 0, 1).
add(0, 0, 1, 1, 0).
add(0, 1, 1, 0, 1).
add(1, 0, 1, 0, 1).
add(1, 1, 1, 1, 1).

add_list( [ A ], [ B ], [ S ], C ) :- add(A, B, S, C).
add_list( [ A | AT ], [ B | BT ], [ S | ST ], C ) :-
%	same_length(AT, BT),
%	same_length(AT, ST),
	add(A, B, Prev, S, C),
	add_list(AT, BT, ST, Prev).

and_list([ X ], [ Y ], [ Z ]) :- and(X, Y, Z).
and_list([ X | XT ], [ Y | YT ], [ Z | ZT ]) :- and(X, Y, Z), and_list(XT, YT, ZT).

or_list([ X ], [ Y ], [ Z ]) :- or(X, Y, Z).
or_list([ X | XT ], [ Y | YT ], [ Z | ZT ]) :- or(X, Y, Z), or_list(XT, YT, ZT).

xor_list([ X ], [ Y ], [ Z ]) :- xor(X, Y, Z).
xor_list([ X | XT ], [ Y | YT ], [ Z | ZT ]) :- xor(X, Y, Z), xor_list(XT, YT, ZT).

not_list([ X ], [ Y ]) :- not(X, Y).
not_list([ X | XT ], [ Y | YT ]) :- not(X, Y), not_list(XT, YT).

trim_list( [ 0 | List ], TrimmedList ) :- trim_list( List, TrimmedList).
trim_list( [ 1 | List ], [ 1 | List ]).
trim_list( [ 0 ], [ 0 ]).

% hex_digit(digit, bit_list)
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

code_to_list(Code, Value) :-
	High is Code // 16,
	Low is Code mod 16,
	hex_digit(High, _, D1),
	hex_digit(Low, _, D2),
	append(D1, D2, Value).

test_code_to_list :- code_to_list(54, [ 0, 0, 1, 1, 0, 1, 1, 0 ]).

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
	run_test(test_code_to_list),
	run_test(test_conv_hex_to_dword),
	run_test(test_conv_hex_to_dword_reverse),
	run_test(test_md5_transform_list_f),
	run_test(test_md5_transform_list_g),
	run_test(test_md5_transform_list_h),
	run_test(test_md5_transform_list_i),
	!.

run_test(Test) :- print(Test), nl, call(Test), !.

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

% glowna funkcja md5
md5(MsgStr, Digest) :-
	string_length(MsgStr, MsgStrLen),
	md5_init(Context),
	md5_update(Context, MsgStr, MsgStrLen),
	md5_final(Context, Digest),
	!.

md5_init(
    [ state0(S0),
      state1(S1),
      state2(S2),
      state3(S3)
    ]) :-
	conv_hex_to_dword( '67452301' , S0 ),
	conv_hex_to_dword( 'efcdab89' , S1 ),
	conv_hex_to_dword( '98badcfe' , S2 ),
	conv_hex_to_dword( '10325476' , S3 ),
	!.




decode(MsgStr, ListDwords) :-
	string_length(MsgStr, Length),
	0 is Length mod 4,
	decode(MsgStr, 0, Length, ListDwords).
decode(_, Length, Length, []) :- !.
decode(MsgStr, Start, Length, [ Substring | ListDwords ]) :-
	Start < Length,
	sub_string(MsgStr, Start, 4, _, Substring),
	NewStart is Start + 4,
	decode(MsgStr, NewStart, Length, ListDwords).

char_to_dword(String, Dword) :-
	string_to_list(String, [ C1, C2, C3, C4 ]),
	code_to_list(C1, B1),
	code_to_list(C2, B2),
	code_to_list(C3, B3),
	code_to_list(C4, B4),
	append([B4, B3, B2, B1], Dword).

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

md5_transform(f, 0, 0, 0, 0).
md5_transform(f, 0, 0, 1, 1).
md5_transform(f, 0, 1, 0, 0).
md5_transform(f, 0, 1, 1, 1).
md5_transform(f, 1, 0, 0, 0).
md5_transform(f, 1, 0, 1, 0).
md5_transform(f, 1, 1, 0, 1).
md5_transform(f, 1, 1, 1, 1).
md5_transform(g, 0, 0, 0, 0).
md5_transform(g, 0, 0, 1, 0).
md5_transform(g, 0, 1, 0, 1).
md5_transform(g, 0, 1, 1, 0).
md5_transform(g, 1, 0, 0, 0).
md5_transform(g, 1, 0, 1, 1).
md5_transform(g, 1, 1, 0, 1).
md5_transform(g, 1, 1, 1, 1).
md5_transform(h, 0, 0, 0, 0).
md5_transform(h, 0, 0, 1, 1).
md5_transform(h, 0, 1, 0, 1).
md5_transform(h, 0, 1, 1, 0).
md5_transform(h, 1, 0, 0, 1).
md5_transform(h, 1, 0, 1, 0).
md5_transform(h, 1, 1, 0, 0).
md5_transform(h, 1, 1, 1, 1).
md5_transform(i, 0, 0, 0, 1).
md5_transform(i, 0, 0, 1, 0).
md5_transform(i, 0, 1, 0, 0).
md5_transform(i, 0, 1, 1, 1).
md5_transform(i, 1, 0, 0, 1).
md5_transform(i, 1, 0, 1, 1).
md5_transform(i, 1, 1, 0, 0).
md5_transform(i, 1, 1, 1, 0).

% pojedyncza transformacja - makra F, G, H, I
md5_transform_list(Trans, [ X ], [ Y ], [ Z ], [ R ]) :- md5_transform(Trans, X, Y, Z, R).
md5_transform_list(Trans, [ X | XT ], [ Y | YT ], [ Z | ZT ], [ R | Result]) :-
	md5_transform(Trans, X, Y, Z, R),
	md5_transform_list(Trans, XT, YT, ZT, Result).

% zlozona transformacja - makra FF, GG, HH, II
md5_transform_list(Trans, A, B, C, D, X, S, AC, Result) :-
	md5_transform_list(Trans, B, C, D, F),
	% operacje mozliwe do dalszego scalenia!
	add_list(X, AC, XaddAC, _),
	add_list(F, XaddAC, FaddXaddAC, _),
	add_list(A, FaddXaddAC, Sum, _),
	rol_list(S, Sum, Rotated),
	add_list(Rotated, B, Result, _).

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

divide_list(0, List, [], List).
divide_list(C, [ E | List ], [ E | Left ], Right) :-
	D is C - 1,
	divide_list(D, List, Left, Right),
	!.

rol_list(0, Input, Input).
rol_list(C, Input, Output) :-
	divide_list(C, Input, Left, Right),
	append(Right, Left, Output),
	!.













