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

-module(imf).

-export([encode/1, foo/0]).

-export_type([message/0, header/0, body/0]).

-export_type([field/0, origination_date_field/0, originator_field/0,
              destination_address_field/0, identification_field/0,
              informational_field/0, resent_field/0]).

-export_type([unstructured/0, msg_id/0, address/0, mailbox/0, group/0,
              phrase/0, date/0]).

-type message() :: #{header := header(), body := body()}.

-type header() :: [field()].

-type field() ::
        origination_date_field()
      | originator_field()
      | destination_address_field()
      | identification_field()
      | informational_field()
      | resent_field()
      | trace_field()
      | optional_field().

-type origination_date_field() :: {date, date()}.

-type originator_field() ::
        {from, [mailbox()]}
      | {sender, mailbox()}
      | {reply_to, [address()]}.

-type destination_address_field() ::
        {to, [address()]}
      | {cc, [address()]}
      | {bcc, [address()]}.

-type identification_field() ::
        {message_id, msg_id()}
      | {in_reply_to, nonempty_list(msg_id())}
      | {references, nonempty_list(msg_id())}.

-type informational_field() ::
        {subject, unstructured()}
      | {comments, unstructured()}
      | {keywords, [phrase()]}.

-type resent_field() ::
        {resent_date, date()}
      | {resent_from, [mailbox()]}
      | {resent_sender, mailbox()}
      | {resent_to, [address()]}
      | {resent_cc, [address()]}
      | {resent_bcc, [address()]}
      | {resent_msg_id, msg_id()}.

-type trace_field() ::
        {return_path, binary()}
      | {received, binary()}.


-type optional_field() :: {binary(), unstructured()}.

-type unstructured() :: binary().

-type msg_id() :: {binary(), binary()}.

-type address() :: mailbox() | group().

-type mailbox() :: {mailbox, #{name => binary(), address := binary()}}.

-type group() :: {group, #{name := binary(), addresses => [mailbox()]}}.

-type phrase() :: binary().

-type date() :: {localtime, calendar:datetime()}.

-type body() :: iodata().

-spec encode(message()) -> iodata().
encode(#{header := Header, body := _}) ->
  encode_header(Header).

-spec encode_header(header()) -> iodata().
encode_header(Fields) ->
  lists:foldl(fun encode_field/2, [], Fields).

-spec encode_field(field(), iodata()) -> iodata().
encode_field({date, Value}, Acc) ->
  [["Date: ", imf_date_field:encode(Value)] | Acc];
encode_field({from, Value}, Acc) ->
  [["From: ", imf_address_field:encode(Value)] | Acc];
encode_field({sender, Value}, Acc) ->
  [["Sender: ", imf_address_field:encode([Value])] | Acc];
encode_field({reply_to, Value}, Acc) ->
  [["Reply-To: ", imf_address_field:encode(Value)] | Acc];
encode_field({to, Value}, Acc) ->
  [["To: ", imf_address_field:encode(Value)] | Acc];
encode_field({cc, Value}, Acc) ->
  [["Cc: ", imf_address_field:encode(Value)] | Acc];
encode_field({bcc, _}, Acc) ->
  Acc;
encode_field({message_id, Value}, Acc) ->
  [["Message-ID: ", imf_message_id_field:encode([Value])] | Acc];
encode_field({in_reply_to, Value}, Acc) ->
  [["In-Reply-To: ", imf_message_id_field:encode(Value)] | Acc];
encode_field({references, Value}, Acc) ->
  [["References: ", imf_message_id_field:encode(Value)] | Acc];
encode_field({subject, Value}, Acc) ->
  [["Subject:", imf_unstructured_field:encode(Value, 8)] | Acc];
encode_field({comments, Value}, Acc) ->
  [["Comments:", imf_unstructured_field:encode(Value, 9)] | Acc];
encode_field({keywords, Value}, Acc) ->
  [["Keywords: ", imf_phrase_field:encode(Value)] | Acc];
encode_field({resent_date, Value}, Acc) ->
  [["Resent-Date: ", imf_date_field:encode(Value)] | Acc];
encode_field({resent_from, Value}, Acc) ->
  [["Resent-From: ", imf_address_field:encode(Value)] | Acc];
encode_field({resent_sender, [Value]}, Acc) ->
  [["Resent-Sender: ", imf_address_field:encode(Value)] | Acc];
encode_field({resent_to, Value}, Acc) ->
  [["Resent-To: ", imf_address_field:encode(Value)] | Acc];
encode_field({resent_cc, Value}, Acc) ->
  [["Resent-Cc: ", imf_address_field:encode(Value)] | Acc];
encode_field({resent_bcc, _}, Acc) ->
  Acc;
encode_field({resent_message_id, Value}, Acc) ->
  [["Resent-Message-ID: ", imf_message_id_field:encode([Value])] | Acc];
encode_field({return_path, AngleAddr}, Acc) ->
  [["Return-Path: <", AngleAddr, ">", "\r\n"] | Acc];
encode_filed({received, Value}, Acc) ->
  [["Received: ", Value, "\r\n"] | Acc]
encode_field({Name, Value}, Acc) ->
  Prepend = byte_size(Name) + 1,
  [[Name, ":", imf_unstructured_field:encode(Value, Prepend)] | Acc].

foo() ->
  Mail = #{header =>
             [{from,
               [{mailbox, #{name => <<"Bryan Frimin">>, address => <<"bryan@frimin.fr">>}},
                {mailbox, #{address => <<"bryan@example.com">>}}]},
              {sender,
               {mailbox, #{name => <<"Bryan Frimin">>, address => <<"bryan@frimin.fr">>}}},
              {reply_to,
               [{mailbox, #{address => <<"foo@example.com">>}},
                {group, #{name => <<"Mon super group">>,
                          addresses =>
                            [{mailbox, #{address => <<"group1@example.com">>}},
                             {mailbox, #{address => <<"group2@example.com">>}}]}}]},
              {message_id, {<<"123">>, <<"workstation.frimin.fr">>}},
              {subject, <<"mon super subject">>},
              {comments, <<"my comment about this message">>},
              {keywords, [<<"a">>, <<"b">>, <<"c">>]},
              {return_path, <<"people@example.com">>},
              {date, {localtime, calendar:local_time()}}],
           body =>
             <<"hello world">>},
  %% io:format("~p~n", [iolist_to_binary(encode(Mail))]).
  encode(Mail).

%% [CFWS] "<" addr-spec ">" [CFWS]
%% angle-addr / ([CFWS] "<" [CFWS] ">" [CFWS])
