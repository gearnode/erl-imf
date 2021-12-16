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

-module(imf_address_field).

-export([encode/1]).

-spec encode([imf:address()]) -> iodata().
encode(Addresses) ->
  Encoded = lists:reverse(lists:foldl(fun encode/2, [], Addresses)),
  [lists:join(",\r\n ", Encoded), "\r\n"].

-spec encode(imf:address(), iodata()) -> iodata().
encode({mailbox, #{name := Name, address := Address}}, Acc) ->
  case imf_qencode:encode(Name) of
    Name ->
      [[imf:quote(Name, atom), $\s, $<, Address, $>] | Acc];
    Encoded ->
      [[Encoded, $\s, $<, Address, $>] | Acc]
  end;
encode({mailbox, #{address := Address}}, Acc) ->
  [Address | Acc];
encode({group, #{name := Name, addresses := Addresses}}, Acc) ->
  Data = lists:join(",\r\n ",
                    lists:reverse(
                      lists:foldl(fun encode/2, [], Addresses))),
  case imf_qencode:encode(Name) of
    Name ->
      [[imf:quote(Name, atom), ":", Data, ";"] | Acc];
    Encoded ->
      [[Encoded, ":", Data, ";"] | Acc]
  end;
encode({group, #{name := Name}}, Acc) ->
  case imf_qencode:encode(Name) of
    Name ->
      [[imf:quote(Name, atom), ":", ";"] | Acc];
    Encoded ->
      [[Encoded, ":", ";"] | Acc]
  end.
