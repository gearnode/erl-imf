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

-export_type([message/0, header/0, body/0]).

-export_type([field/0, origination_date_field/0, originator_field/0,
              destination_address_field/0, identification_field/0,
              informational_field/0, resent_field/0]).

-export_type([unstructured/0, msg_id/0, mailbox/0, group/0, phrase/0, date/0]).

-type message() :: #{header := header(), body := body()}.

-type header() :: [field()].

-type field() ::
        origination_date_field()
      | originator_field()
      | destination_address_field()
      | identification_field()
      | informational_field()
      | resent_field().

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
      | {comment, unstructured()}
      | {keywords, [phrase()]}.

-type resent_field() ::
        {resent_date, date()}
      | {resent_from, [mailbox()]}
      | {resent_sender, mailbox()}
      | {resent_to, [address()]}
      | {resent_cc, [address()]}
      | {resent_bcc, [address()]}
      | {resent_msg_id, msg_id()}.

-type unstructured() :: binary().

-type msg_id() :: binary().

-type address() :: mailbox() | group().

-type mailbox() :: #{name => binary(), address := binary()}.

-type group() :: #{name := binary(), addresses => [mailbox()]}.

-type phrase() :: binary().

-type date() :: {localtime, calendar:datetime()}.

-type body() :: term().
