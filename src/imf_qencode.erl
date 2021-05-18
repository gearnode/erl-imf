%% Copyright (c) 2021 Bryan Frimin <bryan@frimin.fr>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(imf_qencode).

-export([encode/1]).

-type encoding() :: ascii | latin1 | utf8.

-spec encode(binary()) -> binary().
encode(Bin) ->
  case heuristic_encoding_bin(Bin) of
    ascii ->
      Bin;
    utf8 ->
      iolist_to_binary(encode_utf8_words(Bin, []));
    latin1 ->
      iolist_to_binary(encode_latin1_words(Bin, []))
  end.

-spec encode_utf8_words(binary(), iodata()) -> iodata().
encode_utf8_words(<<>>, Acc) ->
  lists:reverse(lists:join($\s, Acc));
encode_utf8_words(Bin, Acc) ->
  {EncodingWord, Rest} = encode_utf8_word(Bin, ["?Q?", "UTF-8", "=?"]),
  encode_utf8_words(Rest, [[EncodingWord, "?="] | Acc]).

-spec encode_utf8_word(binary(), iodata()) -> {iodata(), binary()}.
encode_utf8_word(<<>>, Acc) ->
  {lists:reverse(Acc), <<>>};
encode_utf8_word(Bin = <<C/utf8, Rest/binary>>, Acc) ->
  EncodedCodePoint = encode_utf8_codepoint(C),
  case iolist_size(Acc) + iolist_size(EncodedCodePoint) of
    N when N =< 73 ->
      encode_utf8_word(Rest, [EncodedCodePoint | Acc]);
    _ ->
      {lists:reverse(Acc), Bin}
  end.

-spec encode_utf8_codepoint(byte()) -> iodata().
encode_utf8_codepoint($\s) ->
  [$_];
encode_utf8_codepoint(C) when
    C > $\s, C =< $~, C =/= $_, C =/= $=, C =/= $! ->
  [C];
encode_utf8_codepoint(C) ->
  case <<C/utf8>> of
    <<B1>> -> 
      io_lib:format("=~2.16.0B", [B1]);
    <<B1, B2>> ->
      io_lib:format("=~2.16.0B=~2.16.0B", [B1, B2]);
    <<B1, B2, B3>> ->
      io_lib:format("=~2.16.0B=~2.16.0B=~2.16.0B", [B1, B2, B3]);
    <<B1, B2, B3, B4>> ->
      io_lib:format("=~2.16.0B=~2.16.0B=~2.16.0B=~2.16.0B", [B1, B2, B3, B4])
  end.

-spec encode_latin1_words(binary(), iodata()) -> iodata().
encode_latin1_words(<<>>, Acc) ->
  lists:reverse(lists:join($\s, Acc));
encode_latin1_words(Bin, Acc) ->
  {EncodingWord, Rest} = encode_latin1_word(Bin, ["?Q?", "ISO-8859-1", "=?"]),
  encode_latin1_words(Rest, [[EncodingWord, "?="] | Acc]).

-spec encode_latin1_word(binary(), iodata()) -> {iodata(), binary()}.
encode_latin1_word(<<>>, Acc) ->
  {lists:reverse(Acc), <<>>};
encode_latin1_word(Bin = <<C, Rest/binary>>, Acc) ->
  EncodedByte = encode_latin1_byte(C),
  case iolist_size(Acc) + iolist_size(EncodedByte) of
    N when N =< 73 ->
      encode_latin1_word(Rest, [EncodedByte | Acc]);
    _ ->
      {lists:reverse(Acc), Bin}
  end.

-spec encode_latin1_byte(byte()) -> iodata().
encode_latin1_byte($\s) ->
  [$_];
encode_latin1_byte(C) when
    C > $\s, C =< $~, C =/= $_, C =/= $=, C =/= $! ->
  [C];
encode_latin1_byte(C) ->
  io_lib:format("=~2.16.0B", [C]).

-spec heuristic_encoding_bin(binary()) -> encoding().
heuristic_encoding_bin(Bin) ->
  case is_ascii(Bin) of
    true ->
      ascii;
    false ->
      case unicode:characters_to_binary(Bin, utf8, utf8) of
        Bin -> utf8;
        _ -> latin1
      end
  end.

-spec is_ascii(binary()) -> boolean().
is_ascii(<<>>) -> true;
is_ascii(<<C, Rest/binary>>) when C >= 0, C =< 127 ->
  is_ascii(Rest);
is_ascii(_) ->
  false.
