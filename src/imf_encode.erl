%% Copyright (c) 2022 Bryan Frimin <bryan@frimin.fr>.
%% Copyright (c) 2021 Exograd SAS.
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

-module(imf_encode).

-export([heuristic_encoding_bin/1,
         encode_utf8_codepoint/1, encode_latin1_byte/1]).

-export_type([encoding/0]).

-type encoding() :: ascii | latin1 | utf8.

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

-spec encode_utf8_codepoint(byte()) -> iodata().
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

-spec encode_latin1_byte(byte()) -> iodata().
encode_latin1_byte(C) when
    C > $\s, C =< $~, C =/= $_, C =/= $=, C =/= $! ->
  [C];
encode_latin1_byte(C) ->
  io_lib:format("=~2.16.0B", [C]).
