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

-module(imf_unstructured_field).

-export([encode/2]).

-spec encode(imf:unstructured(), pos_integer()) -> iodata().
encode(Value, Prepend) ->
  wrap_lines(imf_mime:qencode(Value), Prepend, []).

-spec wrap_lines(binary(), non_neg_integer(), iodata()) -> iodata().
wrap_lines(<<>>, _, Acc) ->
  lists:reverse(Acc);
wrap_lines(Bin, Prepend, Acc) ->
  {Line, Rest} = fold(Bin, Prepend, []),
  wrap_lines(Rest, 0, [[Line, $\r, $\n] | Acc]).

-spec fold(binary(), non_neg_integer(), iodata()) -> {iodata(), binary()}.
fold(Bin, LineSize, Acc) ->
  case binary:split(Bin, [<<$\s>>, <<$\t>>]) of
    [P1, P2] ->
      case LineSize + byte_size(P1) + 1 of
        LineSize2 when LineSize2 < 78 ->
          fold(P2, LineSize2, [[$\s, P1]| Acc]);
        _ when Acc =:= [] ->
          {lists:reverse([[$\s, P1] | Acc]), P2};
        _ ->
          {lists:reverse(Acc), Bin}
      end;
    [P1] ->
      {lists:reverse([[$\s, P1] | Acc]), <<>>}
  end.
