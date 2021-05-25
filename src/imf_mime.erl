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

-module(imf_mime).

-export([encode_version/1, encode_part/1]).

-export_type([part/0, header/0, field/0, body/0]).

-type part() :: #{header := header(),
                  body := body()}.

-type header() :: [field()].

-type field() :: content_type()
               | content_transfer_encoding()
               | content_id()
               | content_description().

-type content_type() :: {content_type, media_type()}.

-type content_transfer_encoding() :: {content_transfer_encoding, mechanism()}.

-type content_id() :: {content_id, imf:msg_id()}.

-type content_description() :: {content_description, text()}.

-type mechanism() :: '7bit' | '8bit' | binary | quoted_printable | base64.

-type media_type() ::
        #{type := binary(), subtype := binary(),
          parameters => #{attribute() => value()}}.

-type attribute() :: binary().
-type value() :: binary().

-type text() :: binary().

-type body() :: {data, iodata()}
              | {part, part()}
              | {part, [part()]}.

-spec encode_version(imf:mime_version()) -> iodata().
encode_version({Major, Minor}) ->
  io_lib:format("~B.~B", [Major, Minor]).

-spec encode_part(part()) -> iodata().
encode_part(#{header := Fields}) ->
  Data = lists:reverse(lists:foldl(fun encode_field/2, [], Fields)),
  Data.

-spec encode_field(field(), iodata()) -> iodata().
encode_field({content_type, MediaType}, Acc) ->
  [["Content-Type: ", encode_media_type(MediaType), "\r\n"] | Acc];
encode_field({content_transfer_encoding, Mechanism}, Acc) ->
  [["Content-Transfer-Encoding: ", encode_mechanism(Mechanism), "\r\n"] | Acc];
encode_field({content_id, Id}, Acc) ->
  [["Content-ID: ", imf_message_id_field:encode([Id])] | Acc];
encode_field({content_description, Bin}, Acc) ->
  [["Content-Description:", imf_unstructured_field:encode(Bin, 20)] | Acc];
encode_field({Key, Value}, Acc) ->
  [[Key, ": ", Value, "\r\n"] | Acc].

-spec encode_mechanism(mechanism()) -> iodata().
encode_mechanism('7bit') ->
  "7bit";
encode_mechanism('8bit') ->
  "8bit";
encode_mechanism(binary) ->
  "binary";
encode_mechanism(quoted_printable) ->
  "quoted-printable";
encode_mechanism(base64) ->
  "base64".

-spec encode_media_type(media_type()) -> iodata().
encode_media_type(MediaType) ->
  Type = maps:get(type, MediaType),
  SubType = maps:get(subtype, MediaType),
  F = fun (K, V, A) -> <<A/binary, $;, K/binary, $=, $", V/binary, $">> end,
  Parameters = maps:fold(F, <<>>, maps:get(parameters, MediaType, #{})),
  case Parameters of
    <<>> ->
      <<Type/binary, $/, SubType/binary>>;
    _Else ->
      <<Type/binary, $/, SubType/binary, Parameters/binary>>
  end.
