:- use_module(library(clpb)).

bytelist_to_bitlist([], []).
bytelist_to_bitlist([Byte | Byte_List], [ A,B,C,D,E,F,G,H | Bit_List]) :-
	byte_to_bits(Byte, [A,B,C,D,E,F,G,H]),
	bytelist_to_bitlist(Byte_List, Bit_List).
bytelist_to_bitlist([], Bit_List) :-
	length(Bit_List, Length),
	between(1, 7, Length),
	print("This bitlist has number of bits not divisible by 8"), nl,
	print("Remainder: "), print(Bit_List), nl.

% md5/2
md5(Message, _Digest) :-
	length(Message, Message_Byte_Length),

	% some artificial limitation to simplify things
	Message_Byte_Length < 256,

	print("Looking for solution for message of length "),
	print(Message_Byte_Length), print(" chars."),  nl,

	bytelist_to_bitlist(Message, Message_Bits),
	byte_to_bits(Message_Byte_Length, Length_Encoded),

	length(Message_Block, 512),
	length(Message_Length, 64),
	append([Padding_1, Length_Encoded], Message_Length),
	maplist(=(0), Padding_1),
	append([Message_Bits, [1], Padding_2, Message_Length], Message_Block),
	maplist(=(0), Padding_2),

	md5_init_states(States).

% md5_init_states/1
% md5_init_states([A, B, C, D]).
md5_init_states([0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476]).

% md5_rotate_constant/2
% md5_rotate_constant(ID, Value)
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

% md5_round_constant/5
% md5_round_constant(round_no, func_id, rotate_const_id, magic_const,
% block_no)
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

% byte_to_bits/2
% byte_to_bits(Byte_Value, Bit_List).
byte_to_bits(0,[0,0,0,0,0,0,0,0]).
byte_to_bits(1,[0,0,0,0,0,0,0,1]).
byte_to_bits(2,[0,0,0,0,0,0,1,0]).
byte_to_bits(3,[0,0,0,0,0,0,1,1]).
byte_to_bits(4,[0,0,0,0,0,1,0,0]).
byte_to_bits(5,[0,0,0,0,0,1,0,1]).
byte_to_bits(6,[0,0,0,0,0,1,1,0]).
byte_to_bits(7,[0,0,0,0,0,1,1,1]).
byte_to_bits(8,[0,0,0,0,1,0,0,0]).
byte_to_bits(9,[0,0,0,0,1,0,0,1]).
byte_to_bits(10,[0,0,0,0,1,0,1,0]).
byte_to_bits(11,[0,0,0,0,1,0,1,1]).
byte_to_bits(12,[0,0,0,0,1,1,0,0]).
byte_to_bits(13,[0,0,0,0,1,1,0,1]).
byte_to_bits(14,[0,0,0,0,1,1,1,0]).
byte_to_bits(15,[0,0,0,0,1,1,1,1]).
byte_to_bits(16,[0,0,0,1,0,0,0,0]).
byte_to_bits(17,[0,0,0,1,0,0,0,1]).
byte_to_bits(18,[0,0,0,1,0,0,1,0]).
byte_to_bits(19,[0,0,0,1,0,0,1,1]).
byte_to_bits(20,[0,0,0,1,0,1,0,0]).
byte_to_bits(21,[0,0,0,1,0,1,0,1]).
byte_to_bits(22,[0,0,0,1,0,1,1,0]).
byte_to_bits(23,[0,0,0,1,0,1,1,1]).
byte_to_bits(24,[0,0,0,1,1,0,0,0]).
byte_to_bits(25,[0,0,0,1,1,0,0,1]).
byte_to_bits(26,[0,0,0,1,1,0,1,0]).
byte_to_bits(27,[0,0,0,1,1,0,1,1]).
byte_to_bits(28,[0,0,0,1,1,1,0,0]).
byte_to_bits(29,[0,0,0,1,1,1,0,1]).
byte_to_bits(30,[0,0,0,1,1,1,1,0]).
byte_to_bits(31,[0,0,0,1,1,1,1,1]).
byte_to_bits(32,[0,0,1,0,0,0,0,0]).
byte_to_bits(33,[0,0,1,0,0,0,0,1]).
byte_to_bits(34,[0,0,1,0,0,0,1,0]).
byte_to_bits(35,[0,0,1,0,0,0,1,1]).
byte_to_bits(36,[0,0,1,0,0,1,0,0]).
byte_to_bits(37,[0,0,1,0,0,1,0,1]).
byte_to_bits(38,[0,0,1,0,0,1,1,0]).
byte_to_bits(39,[0,0,1,0,0,1,1,1]).
byte_to_bits(40,[0,0,1,0,1,0,0,0]).
byte_to_bits(41,[0,0,1,0,1,0,0,1]).
byte_to_bits(42,[0,0,1,0,1,0,1,0]).
byte_to_bits(43,[0,0,1,0,1,0,1,1]).
byte_to_bits(44,[0,0,1,0,1,1,0,0]).
byte_to_bits(45,[0,0,1,0,1,1,0,1]).
byte_to_bits(46,[0,0,1,0,1,1,1,0]).
byte_to_bits(47,[0,0,1,0,1,1,1,1]).
byte_to_bits(48,[0,0,1,1,0,0,0,0]).
byte_to_bits(49,[0,0,1,1,0,0,0,1]).
byte_to_bits(50,[0,0,1,1,0,0,1,0]).
byte_to_bits(51,[0,0,1,1,0,0,1,1]).
byte_to_bits(52,[0,0,1,1,0,1,0,0]).
byte_to_bits(53,[0,0,1,1,0,1,0,1]).
byte_to_bits(54,[0,0,1,1,0,1,1,0]).
byte_to_bits(55,[0,0,1,1,0,1,1,1]).
byte_to_bits(56,[0,0,1,1,1,0,0,0]).
byte_to_bits(57,[0,0,1,1,1,0,0,1]).
byte_to_bits(58,[0,0,1,1,1,0,1,0]).
byte_to_bits(59,[0,0,1,1,1,0,1,1]).
byte_to_bits(60,[0,0,1,1,1,1,0,0]).
byte_to_bits(61,[0,0,1,1,1,1,0,1]).
byte_to_bits(62,[0,0,1,1,1,1,1,0]).
byte_to_bits(63,[0,0,1,1,1,1,1,1]).
byte_to_bits(64,[0,1,0,0,0,0,0,0]).
byte_to_bits(65,[0,1,0,0,0,0,0,1]).
byte_to_bits(66,[0,1,0,0,0,0,1,0]).
byte_to_bits(67,[0,1,0,0,0,0,1,1]).
byte_to_bits(68,[0,1,0,0,0,1,0,0]).
byte_to_bits(69,[0,1,0,0,0,1,0,1]).
byte_to_bits(70,[0,1,0,0,0,1,1,0]).
byte_to_bits(71,[0,1,0,0,0,1,1,1]).
byte_to_bits(72,[0,1,0,0,1,0,0,0]).
byte_to_bits(73,[0,1,0,0,1,0,0,1]).
byte_to_bits(74,[0,1,0,0,1,0,1,0]).
byte_to_bits(75,[0,1,0,0,1,0,1,1]).
byte_to_bits(76,[0,1,0,0,1,1,0,0]).
byte_to_bits(77,[0,1,0,0,1,1,0,1]).
byte_to_bits(78,[0,1,0,0,1,1,1,0]).
byte_to_bits(79,[0,1,0,0,1,1,1,1]).
byte_to_bits(80,[0,1,0,1,0,0,0,0]).
byte_to_bits(81,[0,1,0,1,0,0,0,1]).
byte_to_bits(82,[0,1,0,1,0,0,1,0]).
byte_to_bits(83,[0,1,0,1,0,0,1,1]).
byte_to_bits(84,[0,1,0,1,0,1,0,0]).
byte_to_bits(85,[0,1,0,1,0,1,0,1]).
byte_to_bits(86,[0,1,0,1,0,1,1,0]).
byte_to_bits(87,[0,1,0,1,0,1,1,1]).
byte_to_bits(88,[0,1,0,1,1,0,0,0]).
byte_to_bits(89,[0,1,0,1,1,0,0,1]).
byte_to_bits(90,[0,1,0,1,1,0,1,0]).
byte_to_bits(91,[0,1,0,1,1,0,1,1]).
byte_to_bits(92,[0,1,0,1,1,1,0,0]).
byte_to_bits(93,[0,1,0,1,1,1,0,1]).
byte_to_bits(94,[0,1,0,1,1,1,1,0]).
byte_to_bits(95,[0,1,0,1,1,1,1,1]).
byte_to_bits(96,[0,1,1,0,0,0,0,0]).
byte_to_bits(97,[0,1,1,0,0,0,0,1]).
byte_to_bits(98,[0,1,1,0,0,0,1,0]).
byte_to_bits(99,[0,1,1,0,0,0,1,1]).
byte_to_bits(100,[0,1,1,0,0,1,0,0]).
byte_to_bits(101,[0,1,1,0,0,1,0,1]).
byte_to_bits(102,[0,1,1,0,0,1,1,0]).
byte_to_bits(103,[0,1,1,0,0,1,1,1]).
byte_to_bits(104,[0,1,1,0,1,0,0,0]).
byte_to_bits(105,[0,1,1,0,1,0,0,1]).
byte_to_bits(106,[0,1,1,0,1,0,1,0]).
byte_to_bits(107,[0,1,1,0,1,0,1,1]).
byte_to_bits(108,[0,1,1,0,1,1,0,0]).
byte_to_bits(109,[0,1,1,0,1,1,0,1]).
byte_to_bits(110,[0,1,1,0,1,1,1,0]).
byte_to_bits(111,[0,1,1,0,1,1,1,1]).
byte_to_bits(112,[0,1,1,1,0,0,0,0]).
byte_to_bits(113,[0,1,1,1,0,0,0,1]).
byte_to_bits(114,[0,1,1,1,0,0,1,0]).
byte_to_bits(115,[0,1,1,1,0,0,1,1]).
byte_to_bits(116,[0,1,1,1,0,1,0,0]).
byte_to_bits(117,[0,1,1,1,0,1,0,1]).
byte_to_bits(118,[0,1,1,1,0,1,1,0]).
byte_to_bits(119,[0,1,1,1,0,1,1,1]).
byte_to_bits(120,[0,1,1,1,1,0,0,0]).
byte_to_bits(121,[0,1,1,1,1,0,0,1]).
byte_to_bits(122,[0,1,1,1,1,0,1,0]).
byte_to_bits(123,[0,1,1,1,1,0,1,1]).
byte_to_bits(124,[0,1,1,1,1,1,0,0]).
byte_to_bits(125,[0,1,1,1,1,1,0,1]).
byte_to_bits(126,[0,1,1,1,1,1,1,0]).
byte_to_bits(127,[0,1,1,1,1,1,1,1]).
byte_to_bits(128,[1,0,0,0,0,0,0,0]).
byte_to_bits(129,[1,0,0,0,0,0,0,1]).
byte_to_bits(130,[1,0,0,0,0,0,1,0]).
byte_to_bits(131,[1,0,0,0,0,0,1,1]).
byte_to_bits(132,[1,0,0,0,0,1,0,0]).
byte_to_bits(133,[1,0,0,0,0,1,0,1]).
byte_to_bits(134,[1,0,0,0,0,1,1,0]).
byte_to_bits(135,[1,0,0,0,0,1,1,1]).
byte_to_bits(136,[1,0,0,0,1,0,0,0]).
byte_to_bits(137,[1,0,0,0,1,0,0,1]).
byte_to_bits(138,[1,0,0,0,1,0,1,0]).
byte_to_bits(139,[1,0,0,0,1,0,1,1]).
byte_to_bits(140,[1,0,0,0,1,1,0,0]).
byte_to_bits(141,[1,0,0,0,1,1,0,1]).
byte_to_bits(142,[1,0,0,0,1,1,1,0]).
byte_to_bits(143,[1,0,0,0,1,1,1,1]).
byte_to_bits(144,[1,0,0,1,0,0,0,0]).
byte_to_bits(145,[1,0,0,1,0,0,0,1]).
byte_to_bits(146,[1,0,0,1,0,0,1,0]).
byte_to_bits(147,[1,0,0,1,0,0,1,1]).
byte_to_bits(148,[1,0,0,1,0,1,0,0]).
byte_to_bits(149,[1,0,0,1,0,1,0,1]).
byte_to_bits(150,[1,0,0,1,0,1,1,0]).
byte_to_bits(151,[1,0,0,1,0,1,1,1]).
byte_to_bits(152,[1,0,0,1,1,0,0,0]).
byte_to_bits(153,[1,0,0,1,1,0,0,1]).
byte_to_bits(154,[1,0,0,1,1,0,1,0]).
byte_to_bits(155,[1,0,0,1,1,0,1,1]).
byte_to_bits(156,[1,0,0,1,1,1,0,0]).
byte_to_bits(157,[1,0,0,1,1,1,0,1]).
byte_to_bits(158,[1,0,0,1,1,1,1,0]).
byte_to_bits(159,[1,0,0,1,1,1,1,1]).
byte_to_bits(160,[1,0,1,0,0,0,0,0]).
byte_to_bits(161,[1,0,1,0,0,0,0,1]).
byte_to_bits(162,[1,0,1,0,0,0,1,0]).
byte_to_bits(163,[1,0,1,0,0,0,1,1]).
byte_to_bits(164,[1,0,1,0,0,1,0,0]).
byte_to_bits(165,[1,0,1,0,0,1,0,1]).
byte_to_bits(166,[1,0,1,0,0,1,1,0]).
byte_to_bits(167,[1,0,1,0,0,1,1,1]).
byte_to_bits(168,[1,0,1,0,1,0,0,0]).
byte_to_bits(169,[1,0,1,0,1,0,0,1]).
byte_to_bits(170,[1,0,1,0,1,0,1,0]).
byte_to_bits(171,[1,0,1,0,1,0,1,1]).
byte_to_bits(172,[1,0,1,0,1,1,0,0]).
byte_to_bits(173,[1,0,1,0,1,1,0,1]).
byte_to_bits(174,[1,0,1,0,1,1,1,0]).
byte_to_bits(175,[1,0,1,0,1,1,1,1]).
byte_to_bits(176,[1,0,1,1,0,0,0,0]).
byte_to_bits(177,[1,0,1,1,0,0,0,1]).
byte_to_bits(178,[1,0,1,1,0,0,1,0]).
byte_to_bits(179,[1,0,1,1,0,0,1,1]).
byte_to_bits(180,[1,0,1,1,0,1,0,0]).
byte_to_bits(181,[1,0,1,1,0,1,0,1]).
byte_to_bits(182,[1,0,1,1,0,1,1,0]).
byte_to_bits(183,[1,0,1,1,0,1,1,1]).
byte_to_bits(184,[1,0,1,1,1,0,0,0]).
byte_to_bits(185,[1,0,1,1,1,0,0,1]).
byte_to_bits(186,[1,0,1,1,1,0,1,0]).
byte_to_bits(187,[1,0,1,1,1,0,1,1]).
byte_to_bits(188,[1,0,1,1,1,1,0,0]).
byte_to_bits(189,[1,0,1,1,1,1,0,1]).
byte_to_bits(190,[1,0,1,1,1,1,1,0]).
byte_to_bits(191,[1,0,1,1,1,1,1,1]).
byte_to_bits(192,[1,1,0,0,0,0,0,0]).
byte_to_bits(193,[1,1,0,0,0,0,0,1]).
byte_to_bits(194,[1,1,0,0,0,0,1,0]).
byte_to_bits(195,[1,1,0,0,0,0,1,1]).
byte_to_bits(196,[1,1,0,0,0,1,0,0]).
byte_to_bits(197,[1,1,0,0,0,1,0,1]).
byte_to_bits(198,[1,1,0,0,0,1,1,0]).
byte_to_bits(199,[1,1,0,0,0,1,1,1]).
byte_to_bits(200,[1,1,0,0,1,0,0,0]).
byte_to_bits(201,[1,1,0,0,1,0,0,1]).
byte_to_bits(202,[1,1,0,0,1,0,1,0]).
byte_to_bits(203,[1,1,0,0,1,0,1,1]).
byte_to_bits(204,[1,1,0,0,1,1,0,0]).
byte_to_bits(205,[1,1,0,0,1,1,0,1]).
byte_to_bits(206,[1,1,0,0,1,1,1,0]).
byte_to_bits(207,[1,1,0,0,1,1,1,1]).
byte_to_bits(208,[1,1,0,1,0,0,0,0]).
byte_to_bits(209,[1,1,0,1,0,0,0,1]).
byte_to_bits(210,[1,1,0,1,0,0,1,0]).
byte_to_bits(211,[1,1,0,1,0,0,1,1]).
byte_to_bits(212,[1,1,0,1,0,1,0,0]).
byte_to_bits(213,[1,1,0,1,0,1,0,1]).
byte_to_bits(214,[1,1,0,1,0,1,1,0]).
byte_to_bits(215,[1,1,0,1,0,1,1,1]).
byte_to_bits(216,[1,1,0,1,1,0,0,0]).
byte_to_bits(217,[1,1,0,1,1,0,0,1]).
byte_to_bits(218,[1,1,0,1,1,0,1,0]).
byte_to_bits(219,[1,1,0,1,1,0,1,1]).
byte_to_bits(220,[1,1,0,1,1,1,0,0]).
byte_to_bits(221,[1,1,0,1,1,1,0,1]).
byte_to_bits(222,[1,1,0,1,1,1,1,0]).
byte_to_bits(223,[1,1,0,1,1,1,1,1]).
byte_to_bits(224,[1,1,1,0,0,0,0,0]).
byte_to_bits(225,[1,1,1,0,0,0,0,1]).
byte_to_bits(226,[1,1,1,0,0,0,1,0]).
byte_to_bits(227,[1,1,1,0,0,0,1,1]).
byte_to_bits(228,[1,1,1,0,0,1,0,0]).
byte_to_bits(229,[1,1,1,0,0,1,0,1]).
byte_to_bits(230,[1,1,1,0,0,1,1,0]).
byte_to_bits(231,[1,1,1,0,0,1,1,1]).
byte_to_bits(232,[1,1,1,0,1,0,0,0]).
byte_to_bits(233,[1,1,1,0,1,0,0,1]).
byte_to_bits(234,[1,1,1,0,1,0,1,0]).
byte_to_bits(235,[1,1,1,0,1,0,1,1]).
byte_to_bits(236,[1,1,1,0,1,1,0,0]).
byte_to_bits(237,[1,1,1,0,1,1,0,1]).
byte_to_bits(238,[1,1,1,0,1,1,1,0]).
byte_to_bits(239,[1,1,1,0,1,1,1,1]).
byte_to_bits(240,[1,1,1,1,0,0,0,0]).
byte_to_bits(241,[1,1,1,1,0,0,0,1]).
byte_to_bits(242,[1,1,1,1,0,0,1,0]).
byte_to_bits(243,[1,1,1,1,0,0,1,1]).
byte_to_bits(244,[1,1,1,1,0,1,0,0]).
byte_to_bits(245,[1,1,1,1,0,1,0,1]).
byte_to_bits(246,[1,1,1,1,0,1,1,0]).
byte_to_bits(247,[1,1,1,1,0,1,1,1]).
byte_to_bits(248,[1,1,1,1,1,0,0,0]).
byte_to_bits(249,[1,1,1,1,1,0,0,1]).
byte_to_bits(250,[1,1,1,1,1,0,1,0]).
byte_to_bits(251,[1,1,1,1,1,0,1,1]).
byte_to_bits(252,[1,1,1,1,1,1,0,0]).
byte_to_bits(253,[1,1,1,1,1,1,0,1]).
byte_to_bits(254,[1,1,1,1,1,1,1,0]).
byte_to_bits(255,[1,1,1,1,1,1,1,1]).

byte_to_printable_char(32,` `).
byte_to_printable_char(33,`!`).
byte_to_printable_char(34,`"`).
byte_to_printable_char(35,`#`).
byte_to_printable_char(36,`$`).
byte_to_printable_char(37,`%`).
byte_to_printable_char(38,`&`).
byte_to_printable_char(39,`'`).
byte_to_printable_char(40,`(`).
byte_to_printable_char(41,`)`).
byte_to_printable_char(42,`*`).
byte_to_printable_char(43,`+`).
byte_to_printable_char(44,`,`).
byte_to_printable_char(45,`-`).
byte_to_printable_char(46,`.`).
byte_to_printable_char(47,`/`).
byte_to_printable_char(48,`0`).
byte_to_printable_char(49,`1`).
byte_to_printable_char(50,`2`).
byte_to_printable_char(51,`3`).
byte_to_printable_char(52,`4`).
byte_to_printable_char(53,`5`).
byte_to_printable_char(54,`6`).
byte_to_printable_char(55,`7`).
byte_to_printable_char(56,`8`).
byte_to_printable_char(57,`9`).
byte_to_printable_char(58,`:`).
byte_to_printable_char(59,`;`).
byte_to_printable_char(60,`<`).
byte_to_printable_char(61,`=`).
byte_to_printable_char(62,`>`).
byte_to_printable_char(63,`?`).
byte_to_printable_char(64,`@`).
byte_to_printable_char(65,`A`).
byte_to_printable_char(66,`B`).
byte_to_printable_char(67,`C`).
byte_to_printable_char(68,`D`).
byte_to_printable_char(69,`E`).
byte_to_printable_char(70,`F`).
byte_to_printable_char(71,`G`).
byte_to_printable_char(72,`H`).
byte_to_printable_char(73,`I`).
byte_to_printable_char(74,`J`).
byte_to_printable_char(75,`K`).
byte_to_printable_char(76,`L`).
byte_to_printable_char(77,`M`).
byte_to_printable_char(78,`N`).
byte_to_printable_char(79,`O`).
byte_to_printable_char(80,`P`).
byte_to_printable_char(81,`Q`).
byte_to_printable_char(82,`R`).
byte_to_printable_char(83,`S`).
byte_to_printable_char(84,`T`).
byte_to_printable_char(85,`U`).
byte_to_printable_char(86,`V`).
byte_to_printable_char(87,`W`).
byte_to_printable_char(88,`X`).
byte_to_printable_char(89,`Y`).
byte_to_printable_char(90,`Z`).
byte_to_printable_char(91,`[`).
byte_to_printable_char(92,`\\`).
byte_to_printable_char(93,`]`).
byte_to_printable_char(94,`^`).
byte_to_printable_char(95,`_`).
byte_to_printable_char(96,`\``).
byte_to_printable_char(97,`a`).
byte_to_printable_char(98,`b`).
byte_to_printable_char(99,`c`).
byte_to_printable_char(100,`d`).
byte_to_printable_char(101,`e`).
byte_to_printable_char(102,`f`).
byte_to_printable_char(103,`g`).
byte_to_printable_char(104,`h`).
byte_to_printable_char(105,`i`).
byte_to_printable_char(106,`j`).
byte_to_printable_char(107,`k`).
byte_to_printable_char(108,`l`).
byte_to_printable_char(109,`m`).
byte_to_printable_char(110,`n`).
byte_to_printable_char(111,`o`).
byte_to_printable_char(112,`p`).
byte_to_printable_char(113,`q`).
byte_to_printable_char(114,`r`).
byte_to_printable_char(115,`s`).
byte_to_printable_char(116,`t`).
byte_to_printable_char(117,`u`).
byte_to_printable_char(118,`v`).
byte_to_printable_char(119,`w`).
byte_to_printable_char(120,`x`).
byte_to_printable_char(121,`y`).
byte_to_printable_char(122,`z`).
byte_to_printable_char(123,`{`).
byte_to_printable_char(124,`|`).
byte_to_printable_char(125,`}`).
byte_to_printable_char(126,`~`).









