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

-module(email).

-export([encode/1, encode/2]).

-export_type([raw_message/0, message/0, header_fields/0, header_field/0,
              header_field_name/0, header_field_value/0]).

-type raw_message() :: binary().

-type message() :: #{header => header_fields(),
                     body => binary()}.

-type header_fields() :: [header_field()].
-type header_field() :: {header_field_name(), header_field_value()}.

-type header_field_name() :: binary().
-type header_field_value() :: binary().

-spec encode(email:message()) ->
        {ok, email:raw_message()} | {error, email_encoder:error_reason()}.
encode(Message) ->
  email_encoder:encode(Message).

-spec encode(email:message(), email_encoder:options()) ->
        {ok, email:raw_message()} | {error, email_encoder:error_reason()}.
encode(Message, Options) ->
  email_encoder:encode(Message, Options).
