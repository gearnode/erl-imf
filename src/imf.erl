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

-include_lib("kernel/include/inet.hrl").

-export([quote/2,
         encode/1,
         generate_message_id/0, generate_message_id/1]).

-export([mailbox/1, mailbox/2, group/1, group/2]).

-export([recipient_addresses/1]).

-export_type([message/0, header/0]).

-export_type([field/0, origination_date_field/0, originator_field/0,
              destination_address_field/0, identification_field/0,
              informational_field/0, resent_field/0]).

-export_type([unstructured/0, msg_id/0, address/0, mailbox/0, group/0,
              phrase/0, date/0]).

-type message() :: #{header := header(), body := imf_mime:part()}.

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

-spec recipient_addresses(message()) -> [binary()].
recipient_addresses(#{header := Header}) ->
  Set = sets:new(),
  F = fun
        ({FieldName, Values}, Set2) when
            FieldName =:= to; FieldName =:= cc; FieldName =:= bcc ->
          lists:foldl(
            fun (Value, Set3) ->
                case Value of
                  {group, #{addresses := AddressesSpec}} ->
                    lists:foldl(
                      fun ({mailbox, #{address := AddrSpec}}, Set4) ->
                          sets:add_element(AddrSpec, Set4)
                      end, Set3, AddressesSpec);
                  {group, _} ->
                    Set3;
                  {mailbox, #{address := AddrSpec}} ->
                    sets:add_element(AddrSpec, Set3)
                end
            end, Set2, Values);
        (_, Acc) ->
          Acc
      end,
  sets:to_list(lists:foldl(F, Set, Header)).

-spec quote(binary(), atom | dotatom) -> binary().
quote(Bin, Type) ->
  case should_be_quote(Bin, Type) of
    true -> <<$", (escape(Bin))/binary, $">>;
    false -> escape(Bin)
  end.

-spec should_be_quote(binary(), atom | dotatom) -> boolean().
should_be_quote(<<>>, _) ->
  false;
should_be_quote(<<C, Rest/binary>>, atom)
  when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9;
       C =:= $!; C =:= $#; C =:= $$; C =:= $%; C =:= $&; C =:= $';
       C =:= $*; C =:= $+; C =:= $-; C =:= $/; C =:= $=; C =:= $?;
       C =:= $^; C =:= $_; C =:= $`; C =:= ${; C =:= $}; C =:= $|;
       C =:= $~ ->
  should_be_quote(Rest, atom);
should_be_quote(<<C, Rest/binary>>, dotatom)
  when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9;
       C =:= $!; C =:= $#; C =:= $$; C =:= $%; C =:= $&; C =:= $';
       C =:= $*; C =:= $+; C =:= $-; C =:= $/; C =:= $=; C =:= $?;
       C =:= $^; C =:= $_; C =:= $`; C =:= ${; C =:= $}; C =:= $|;
       C =:= $~; C =:= $. ->
  should_be_quote(Rest, dotatom);
should_be_quote(_, _) ->
  true.

-spec escape(binary()) -> binary().
escape(Bin) ->
  escape(Bin, <<>>).

-spec escape(binary(), binary()) -> binary().
escape(<<>>, Acc) ->
  Acc;
escape(<<$\0, Rest/binary>>, Acc) ->
  escape(Rest, <<Acc/binary, $\\, $0>>);
escape(<<$\b, Rest/binary>>, Acc) ->
  escape(Rest, <<Acc/binary, $\\, $b>>);
escape(<<$\t, Rest/binary>>, Acc) ->
  escape(Rest, <<Acc/binary, $\\, $t>>);
escape(<<$\n, Rest/binary>>, Acc) ->
  escape(Rest, <<Acc/binary, $\\, $n>>);
escape(<<$\v, Rest/binary>>, Acc) ->
  escape(Rest, <<Acc/binary, $\\, $v>>);
escape(<<$\f, Rest/binary>>, Acc) ->
  escape(Rest, <<Acc/binary, $\\, $f>>);
escape(<<$\r, Rest/binary>>, Acc) ->
  escape(Rest, <<Acc/binary, $\\, $r>>);
escape(<<C, Rest/binary>>, Acc) when C =:= $!; C =:= $\s;
                                     C >= $#, C =< $[;
                                     C >= $], C =< $~ ->
  escape(Rest, <<Acc/binary, C>>);
escape(<<C, Rest/binary>>, Acc) ->
  escape(Rest, <<Acc/binary, $\\, C>>).

-spec generate_message_id() -> msg_id().
generate_message_id() ->
  generate_message_id(fqdn()).

-spec generate_message_id(binary()) -> msg_id().
generate_message_id(FQDN) ->
  {ksuid:generate(), FQDN}.

-spec fqdn() -> binary().
fqdn() ->
  {ok, Hostname} = inet:gethostname(),
  case inet:gethostbyname(Hostname) of
    {ok, #hostent{h_name = FQDN}} when is_atom(FQDN) ->
      atom_to_binary(FQDN);
    {ok, #hostent{h_name = FQDN}} when is_list(FQDN) ->
      iolist_to_binary(FQDN);
    {error, _} ->
      <<"localhost">>
  end.

-spec mailbox(binary(), binary()) -> mailbox().
mailbox(Name, Address) ->
  {mailbox, #{name => Name, address => Address}}.

-spec mailbox(binary()) -> mailbox().
mailbox(Address) ->
  {mailbox, #{address => Address}}.

-spec group(binary(), [{binary(), binary()} | binary()]) -> group().
group(GroupName, Addresses) ->
  {group,
   #{name => GroupName,
     addresses => lists:map(
                    fun ({Name, Address}) -> mailbox(Name, Address);
                        (Address) -> mailbox(Address)
                    end, Addresses)}}.

-spec group(binary()) -> group().
group(Name) ->
  {group, #{name => Name}}.

-spec encode(message()) -> iodata().
encode(#{header := Header, body := Body}) ->
  [encode_header(Header),
   imf_mime:encode_part(Body)].

-spec encode_header(header()) -> iodata().
encode_header(Fields) ->
  lists:reverse(lists:foldl(fun encode_field/2, [], Fields)).

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
encode_field({resent_sender, Value}, Acc) ->
  [["Resent-Sender: ", imf_address_field:encode([Value])] | Acc];
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
encode_field({received, Value}, Acc) ->
  [["Received: ", Value, "\r\n"] | Acc];
encode_field({Name, Value}, Acc) ->
  Prepend = byte_size(Name) + 1,
  [[Name, ":", imf_unstructured_field:encode(Value, Prepend)] | Acc].
