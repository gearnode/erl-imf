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

-module(imf_quoted_encode).

-export([encode/2]).

-spec encode(binary(), binary()) -> binary().
encode(Bin, <<"UTF-8">>) ->
  iolist_to_binary(encode_utf8_words(Bin, []));
encode(Bin, <<"ISO-8859-1">>) ->
  iolist_to_binary(encode_latin1_words(Bin, []));
encode(Bin, <<"US-ASCII">>) ->
  Bin.

-spec encode_utf8_words(binary(), iodata()) -> iodata().
encode_utf8_words(<<>>, Acc) ->
  lists:reverse(lists:join($\s, Acc));
encode_utf8_words(Bin, Acc) ->
  {EncodingWord, Rest} = encode_utf8_word(Bin, []),
  encode_utf8_words(Rest, [EncodingWord | Acc]).

-spec encode_utf8_word(binary(), iodata()) -> {iodata(), binary()}.
encode_utf8_word(<<>>, Acc) ->
  {lists:reverse(Acc), <<>>};
encode_utf8_word(Bin = <<C/utf8, Rest/binary>>, Acc) ->
  EncodedCodePoint = imf_encode:encode_utf8_codepoint(C),
  case iolist_size(Acc) + iolist_size(EncodedCodePoint) of
    N when N =< 73 ->
      encode_utf8_word(Rest, [EncodedCodePoint | Acc]);
    _ ->
      {lists:reverse(Acc), Bin}
  end.

-spec encode_latin1_words(binary(), iodata()) -> iodata().
encode_latin1_words(<<>>, Acc) ->
  lists:reverse(lists:join($\s, Acc));
encode_latin1_words(Bin, Acc) ->
  {EncodingWord, Rest} = encode_latin1_word(Bin, []),
  encode_latin1_words(Rest, [EncodingWord | Acc]).

-spec encode_latin1_word(binary(), iodata()) -> {iodata(), binary()}.
encode_latin1_word(<<>>, Acc) ->
  {lists:reverse(Acc), <<>>};
encode_latin1_word(Bin = <<C, Rest/binary>>, Acc) ->
  EncodedByte = imf_encode:encode_latin1_byte(C),
  case iolist_size(Acc) + iolist_size(EncodedByte) of
    N when N =< 73 ->
      encode_latin1_word(Rest, [EncodedByte | Acc]);
    _ ->
      {lists:reverse(Acc), Bin}
  end.
