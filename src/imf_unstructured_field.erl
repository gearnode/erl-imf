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

-module(imf_unstructured_field).

-export([encode/2]).

-spec encode(imf:unstructured(), pos_integer()) -> iodata().
encode(Bin, Prepend) ->
  case imf_qencode:encode(Bin) of
    <<>> ->
      [" \r\n"];
    Bin ->
      wrap_lines(Bin, Prepend, []);
    Encoded ->
      wrap_lines(Encoded, Prepend, [])
  end.

-spec wrap_lines(binary(), non_neg_integer(), iodata()) -> iodata().
wrap_lines(<<>>, _, Acc) ->
  lists:reverse(Acc);
wrap_lines(Bin, Prepend, Acc) ->
  {Line, Rest} = fold(Bin, Prepend, []),
  wrap_lines(Rest, 0, [[Line, $\r, $\n] | Acc]).

-spec fold(binary(), non_neg_integer(), iodata()) -> {iodata(), binary()}.
fold(Bin, LineSize, Acc) ->
  case split(Bin) of
    {<<>>, <<>>} ->
      {lists:reverse(Acc), <<>>};
    {Word, Rest} ->
      LineSize2 = LineSize + byte_size(Word) + 1,

      if
        LineSize2 < 78 ->
          fold(Rest, LineSize2, [[$\s, Word] | Acc]);
        LineSize2 >= 78, Acc =:= [] ->
          fold(Rest, LineSize2, [[$\s, Word] | Acc]);
        true ->
          {lists:reverse(Acc), Bin}
      end
  end.

-spec split(binary()) -> {binary(), binary()}.
split(Bin) ->
  case binary:split(Bin, [<<$\s>>, <<$\t>>]) of
    [P1, P2] -> {P1, P2};
    [P1] -> {P1, <<>>}
  end.
