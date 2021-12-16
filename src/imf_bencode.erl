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

-module(imf_bencode).

-export([encode/1]).

-spec encode(binary()) -> iodata().
encode(Bin) ->
  EncodedBin = b64:encode(Bin),
  fold(EncodedBin, []).

-spec fold(binary(), iodata()) -> iodata().
fold(<<>>, Acc) ->
  lists:join("\r\n", lists:reverse(Acc));
fold(<<Bin:76/binary, Rest/binary>>, Acc) ->
  fold(Rest, [Bin | Acc]);
fold(Bin, Acc) ->
  fold(<<>>, [Bin | Acc]).
