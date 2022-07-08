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

-module(imf_phrase_field).

-export([encode/1]).

-spec encode([imf:phrase()]) -> iodata().
encode(Phrases) ->
  Encoded = lists:reverse(lists:foldl(fun encode/2, [], Phrases)),
  [lists:join(",\r\n ", Encoded), "\r\n"].

-spec encode(imf:phrase(), iodata()) -> iodata().
encode(Bin, Acc) ->
  case imf_qencode:encode(Bin) of
    Bin ->
      [imf:quote(Bin, atom) | Acc];
    Encoded ->
      [Encoded | Acc]
  end.
