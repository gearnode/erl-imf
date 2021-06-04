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

-module(imf_tests).

-include_lib("eunit/include/eunit.hrl").

quote_test_() ->
  [?_assertEqual(<<"hello">>,
                 imf:quote(<<"hello">>, atom)),
   ?_assertEqual(<<"\"hello\\\"world\"">>,
                 imf:quote(<<"hello\"world">>, atom)),
   ?_assertEqual(<<"\"hello world\"">>,
                 imf:quote(<<"hello world">>, atom)),
   ?_assertEqual(<<"\"hello\\nworld\"">>,
                 imf:quote(<<"hello\nworld">>, atom)),
   ?_assertEqual(<<"\"hello.world\"">>,
                 imf:quote(<<"hello.world">>, atom)),
   ?_assertEqual(<<"hello.world">>,
                 imf:quote(<<"hello.world">>, dotatom)),
   ?_assertEqual(<<"\"hello\\\\world\"">>,
                 imf:quote(<<"hello\\world">>, atom)),
   ?_assertEqual(<<"\"\\0\"">>,
                 imf:quote(<<$\0>>, atom)),
   ?_assertEqual(<<"\"\\b\"">>,
                 imf:quote(<<$\b>>, atom)),
   ?_assertEqual(<<"\"\\t\"">>,
                 imf:quote(<<$\t>>, atom)),
   ?_assertEqual(<<"\"\\v\"">>,
                 imf:quote(<<$\v>>, atom)),
   ?_assertEqual(<<"\"\\f\"">>,
                 imf:quote(<<$\f>>, atom)),
   ?_assertEqual(<<"\"\\r\"">>,
                 imf:quote(<<$\r>>, atom))].

encode(Mail) ->
  iolist_to_binary(imf:encode(Mail)).

encode_test_() ->
  EmptyBody = #{header => [], body => {data, <<>>}},
  [?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header => [], body => EmptyBody})),

   %% Date header field
   ?_assertEqual(
      <<"Date: Fri, 21 May 2021 14:47:17 -0400\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{date,
                   {localtime, {{2021,5,21},{14,47,17}}}}],
               body => EmptyBody})),

   %% From header field
   ?_assertEqual(
      <<"From: JohnDoe <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{from,
                   [{mailbox,
                     #{name => <<"JohnDoe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: \"John Doe\" <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{from,
                   [{mailbox,
                     #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: john.doe@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{from,
                   [{mailbox,
                     #{address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: =?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{mailbox,
                     #{name => <<"John Doé">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: =?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{mailbox,
                     #{name => <<"John Doé"/utf8>>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: Group1:;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group, #{name => <<"Group1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: \"Group 1\":;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group, #{name => <<"Group 1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group, #{name => <<"Group d'été">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group, #{name => <<"Group d'été"/utf8>>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: Group1:\"John Doe\" <john.doe@example.com>;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doe">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: Group1:=?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: Group1:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com,\r\n"
        " Person1 <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:john.doe@example.com,\r\n"
        " \"Person.1\" <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{from,
                   [{group,
                     #{name => <<"Group d'été"/utf8>>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person.1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: person1@example.com,\r\n"
        " person2@example.com,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{from,
                   [{mailbox, #{address => <<"person1@example.com">>}},
                    {mailbox, #{address => <<"person2@example.com">>}},
                    {mailbox, #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"From: \"Person 1\" <person1@example.com>,\r\n"
        " \"Group 1\":;,\r\n"
        " \"Person 2\" <person2@example.com>,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{from,
                   [{mailbox,
                     #{name => <<"Person 1">>,
                       address => <<"person1@example.com">>}},
                    {group,
                     #{name => <<"Group 1">>}},
                    {mailbox,
                     #{name => <<"Person 2">>,
                       address => <<"person2@example.com">>}},
                    {mailbox,
                     #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   %% Sender header field
   ?_assertEqual(
      <<"Sender: \"John Doe\" <john.doe.example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{sender,
                   {mailbox,
                    #{name => <<"John Doe">>,
                      address => <<"john.doe.example.com">>}}}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Sender: =?ISO-8859-1?Q?John=20Do=E9?= <john.doe.example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{sender,
                   {mailbox,
                    #{name => <<"John Doé">>,
                      address => <<"john.doe.example.com">>}}}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Sender: =?UTF-8?Q?John=20Do=C3=A9?= <john.doe.example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{sender,
                   {mailbox,
                    #{name => <<"John Doé"/utf8>>,
                      address => <<"john.doe.example.com">>}}}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Sender: john.doe.example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{sender,
                   {mailbox,
                    #{address => <<"john.doe.example.com">>}}}],
               body => EmptyBody})),

   %% Reply-To header field
   ?_assertEqual(
      <<"Reply-To: JohnDoe <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{mailbox,
                     #{name => <<"JohnDoe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: \"John Doe\" <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{mailbox,
                     #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: john.doe@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{mailbox,
                     #{address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: =?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{mailbox,
                     #{name => <<"John Doé">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: =?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{mailbox,
                     #{name => <<"John Doé"/utf8>>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: Group1:;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group, #{name => <<"Group1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: \"Group 1\":;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group, #{name => <<"Group 1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group, #{name => <<"Group d'été">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group, #{name => <<"Group d'été"/utf8>>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: Group1:\"John Doe\" <john.doe@example.com>;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doe">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: Group1:=?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: Group1:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com,\r\n"
        " Person1 <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:john.doe@example.com,\r\n"
        " \"Person.1\" <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{group,
                     #{name => <<"Group d'été"/utf8>>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person.1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: person1@example.com,\r\n"
        " person2@example.com,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{mailbox, #{address => <<"person1@example.com">>}},
                    {mailbox, #{address => <<"person2@example.com">>}},
                    {mailbox, #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Reply-To: \"Person 1\" <person1@example.com>,\r\n"
        " \"Group 1\":;,\r\n"
        " \"Person 2\" <person2@example.com>,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{reply_to,
                   [{mailbox,
                     #{name => <<"Person 1">>,
                       address => <<"person1@example.com">>}},
                    {group,
                     #{name => <<"Group 1">>}},
                    {mailbox,
                     #{name => <<"Person 2">>,
                       address => <<"person2@example.com">>}},
                    {mailbox,
                     #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   %% To header field
   ?_assertEqual(
      <<"To: JohnDoe <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{to,
                   [{mailbox,
                     #{name => <<"JohnDoe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: \"John Doe\" <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{to,
                   [{mailbox,
                     #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: john.doe@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{to,
                   [{mailbox,
                     #{address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: =?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{mailbox,
                     #{name => <<"John Doé">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: =?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{mailbox,
                     #{name => <<"John Doé"/utf8>>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: Group1:;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group, #{name => <<"Group1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: \"Group 1\":;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group, #{name => <<"Group 1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group, #{name => <<"Group d'été">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group, #{name => <<"Group d'été"/utf8>>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: Group1:\"John Doe\" <john.doe@example.com>;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doe">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: Group1:=?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: Group1:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com,\r\n"
        " Person1 <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:john.doe@example.com,\r\n"
        " \"Person.1\" <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{to,
                   [{group,
                     #{name => <<"Group d'été"/utf8>>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person.1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: person1@example.com,\r\n"
        " person2@example.com,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{to,
                   [{mailbox, #{address => <<"person1@example.com">>}},
                    {mailbox, #{address => <<"person2@example.com">>}},
                    {mailbox, #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"To: \"Person 1\" <person1@example.com>,\r\n"
        " \"Group 1\":;,\r\n"
        " \"Person 2\" <person2@example.com>,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{to,
                   [{mailbox,
                     #{name => <<"Person 1">>,
                       address => <<"person1@example.com">>}},
                    {group,
                     #{name => <<"Group 1">>}},
                    {mailbox,
                     #{name => <<"Person 2">>,
                       address => <<"person2@example.com">>}},
                    {mailbox,
                     #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   %% Cc header field
   ?_assertEqual(
      <<"Cc: JohnDoe <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{mailbox,
                     #{name => <<"JohnDoe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: \"John Doe\" <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{mailbox,
                     #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: john.doe@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{mailbox,
                     #{address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: =?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{mailbox,
                     #{name => <<"John Doé">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: =?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{mailbox,
                     #{name => <<"John Doé"/utf8>>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: Group1:;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group, #{name => <<"Group1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: \"Group 1\":;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group, #{name => <<"Group 1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group, #{name => <<"Group d'été">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group, #{name => <<"Group d'été"/utf8>>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: Group1:\"John Doe\" <john.doe@example.com>;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doe">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: Group1:=?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: Group1:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com,\r\n"
        " Person1 <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:john.doe@example.com,\r\n"
        " \"Person.1\" <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{group,
                     #{name => <<"Group d'été"/utf8>>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person.1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: person1@example.com,\r\n"
        " person2@example.com,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{mailbox, #{address => <<"person1@example.com">>}},
                    {mailbox, #{address => <<"person2@example.com">>}},
                    {mailbox, #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Cc: \"Person 1\" <person1@example.com>,\r\n"
        " \"Group 1\":;,\r\n"
        " \"Person 2\" <person2@example.com>,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{cc,
                   [{mailbox,
                     #{name => <<"Person 1">>,
                       address => <<"person1@example.com">>}},
                    {group,
                     #{name => <<"Group 1">>}},
                    {mailbox,
                     #{name => <<"Person 2">>,
                       address => <<"person2@example.com">>}},
                    {mailbox,
                     #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   %% Bcc header field
   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{mailbox,
                     #{name => <<"JohnDoe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{mailbox,
                     #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{mailbox,
                     #{address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{mailbox,
                     #{name => <<"John Doé">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{mailbox,
                     #{name => <<"John Doé"/utf8>>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group, #{name => <<"Group1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group, #{name => <<"Group 1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group, #{name => <<"Group d'été">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group, #{name => <<"Group d'été"/utf8>>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doe">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{group,
                     #{name => <<"Group d'été"/utf8>>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person.1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{mailbox, #{address => <<"person1@example.com">>}},
                    {mailbox, #{address => <<"person2@example.com">>}},
                    {mailbox, #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{bcc,
                   [{mailbox,
                     #{name => <<"Person 1">>,
                       address => <<"person1@example.com">>}},
                    {group,
                     #{name => <<"Group 1">>}},
                    {mailbox,
                     #{name => <<"Person 2">>,
                       address => <<"person2@example.com">>}},
                    {mailbox,
                     #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   %% Message-ID header field
   ?_assertEqual(
      <<"Message-ID: <123@imf.example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{message_id, {<<"123">>, <<"imf.example.com">>}}],
               body => EmptyBody})),

   %% In-Reply-To header field
   ?_assertEqual(
      <<"In-Reply-To: <123@example.com>\r\n <456@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{in_reply_to,
                   [{<<"123">>, <<"example.com">>},
                    {<<"456">>, <<"example.com">>}]}],
               body => EmptyBody})),

   %% References header field
   ?_assertEqual(
      <<"References: <123@example.com>\r\n <456@example.com>\r\n\r\n\r\n">>,
     encode(#{header =>
                [{references,
                  [{<<"123">>, <<"example.com">>},
                   {<<"456">>, <<"example.com">>}]}],
              body => EmptyBody})),

   %% Subject
   ?_assertEqual(
      <<"Subject: \r\n\r\n\r\n">>,
      encode(#{header =>
                 [{subject, <<>>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Subject: \" \"\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{subject, <<" ">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Subject: \"hello world\"\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{subject, <<"hello world">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Subject: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{subject, <<"Une journée d'été">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Subject: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{subject, <<"Une journée d'été"/utf8>>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Subject: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9=20c'est=20long,=20vraiment=20t?=\r\n"
        " =?ISO-8859-1?Q?r=E8s=20long...=20non=20mais=20genre=20vraiment=20tr=E8s?="
        " =?ISO-8859-1?Q?=20tr=E8s=20=20tr=E8s=20long=20=21?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{subject, <<"Une journée d'été c'est long, vraiment très long... "
                              "non mais genre vraiment très très  très long !">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Subject: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9=20c'est=20long,=20vraiment?=\r\n"
        " =?UTF-8?Q?=20tr=C3=A8s=20long...=20non=20mais=20genre=20vraiment=20tr?="
        " =?UTF-8?Q?=C3=A8s=20tr=C3=A8s=20=20tr=C3=A8s=20long=20=21?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{subject, <<"Une journée d'été c'est long, vraiment très long... "
                              "non mais genre vraiment très très  très long !"/utf8>>}],
               body => EmptyBody})),

   %% Comments header field
   ?_assertEqual(
      <<"Comments: whaooooooooouuuuuu\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{comments, <<"whaooooooooouuuuuu">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Comments: \"my comment\"\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{comments, <<"my comment">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Comments: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{comments, <<"Une journée d'été">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Comments: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{comments, <<"Une journée d'été"/utf8>>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Comments: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9=20c'est=20long,=20vraiment=20t?=\r\n"
        " =?ISO-8859-1?Q?r=E8s=20long...=20non=20mais=20genre=20vraiment=20tr=E8s?="
        " =?ISO-8859-1?Q?=20tr=E8s=20=20tr=E8s=20long=20=21?=\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{comments, <<"Une journée d'été c'est long, vraiment très long... "
                               "non mais genre vraiment très très  très long !">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Comments: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9=20c'est=20long,=20vraiment?=\r\n"
        " =?UTF-8?Q?=20tr=C3=A8s=20long...=20non=20mais=20genre=20vraiment=20tr?="
        " =?UTF-8?Q?=C3=A8s=20tr=C3=A8s=20=20tr=C3=A8s=20long=20=21?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{comments, <<"Une journée d'été c'est long, vraiment très long..."
                               " non mais genre vraiment très très  très long !"/utf8>>}],
               body => EmptyBody})),

   %% Keywords header field
   ?_assertEqual(
      <<"Keywords: \r\n\r\n\r\n">>,
      encode(#{header =>
                 [{keywords, []}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Keywords: tag1\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{keywords, [<<"tag1">>]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Keywords: tag1,\r\n"
        " tag2,\r\n"
        " tag3\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{keywords, [<<"tag1">>, <<"tag2">>, <<"tag3">>]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Keywords: \"tag 1\",\r\n"
        " \"tag\\\"2\",\r\n"
        " tag3\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{keywords, [<<"tag 1">>, <<"tag\"2">>, <<"tag3">>]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Keywords: \"tag 1\",\r\n"
        " =?ISO-8859-1?Q?=E9t=E9?=,\r\n"
        " =?UTF-8?Q?=C3=A9t=C3=A9?=\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{keywords, [<<"tag 1">>, <<"été">>, <<"été"/utf8>>]}],
               body => EmptyBody})),

   %% Resent-Date header field
   ?_assertEqual(
      <<"Resent-Date: Mon, 24 May 2021 13:20:35 -0400\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_date, {localtime, {{2021,5,24},{13,20,35}}}}],
               body => EmptyBody})),

   %% Resent-From header field
   ?_assertEqual(
      <<"Resent-From: JohnDoe <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{mailbox,
                     #{name => <<"JohnDoe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: \"John Doe\" <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{mailbox,
                     #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: john.doe@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{mailbox,
                     #{address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: =?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{mailbox,
                     #{name => <<"John Doé">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: =?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{mailbox,
                     #{name => <<"John Doé"/utf8>>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: Group1:;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group, #{name => <<"Group1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: \"Group 1\":;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group, #{name => <<"Group 1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group, #{name => <<"Group d'été">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group, #{name => <<"Group d'été"/utf8>>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: Group1:\"John Doe\" <john.doe@example.com>;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doe">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: Group1:=?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: Group1:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com,\r\n"
        " Person1 <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:john.doe@example.com,\r\n"
        " \"Person.1\" <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{group,
                     #{name => <<"Group d'été"/utf8>>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person.1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: person1@example.com,\r\n"
        " person2@example.com,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{mailbox, #{address => <<"person1@example.com">>}},
                    {mailbox, #{address => <<"person2@example.com">>}},
                    {mailbox, #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-From: \"Person 1\" <person1@example.com>,\r\n"
        " \"Group 1\":;,\r\n"
        " \"Person 2\" <person2@example.com>,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_from,
                   [{mailbox,
                     #{name => <<"Person 1">>,
                       address => <<"person1@example.com">>}},
                    {group,
                     #{name => <<"Group 1">>}},
                    {mailbox,
                     #{name => <<"Person 2">>,
                       address => <<"person2@example.com">>}},
                    {mailbox,
                     #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   %% Resent-Sender header field
   ?_assertEqual(
      <<"Resent-Sender: \"John Doe\" <john.doe.example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_sender,
                   {mailbox,
                    #{name => <<"John Doe">>,
                      address => <<"john.doe.example.com">>}}}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Sender: =?ISO-8859-1?Q?John=20Do=E9?= <john.doe.example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_sender,
                   {mailbox,
                    #{name => <<"John Doé">>,
                      address => <<"john.doe.example.com">>}}}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Sender: =?UTF-8?Q?John=20Do=C3=A9?= <john.doe.example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_sender,
                   {mailbox,
                    #{name => <<"John Doé"/utf8>>,
                      address => <<"john.doe.example.com">>}}}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Sender: john.doe.example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_sender,
                   {mailbox,
                    #{address => <<"john.doe.example.com">>}}}],
               body => EmptyBody})),

   %% Resent-To header field
   ?_assertEqual(
      <<"Resent-To: JohnDoe <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{mailbox,
                     #{name => <<"JohnDoe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: \"John Doe\" <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{mailbox,
                     #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: john.doe@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{mailbox,
                     #{address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: =?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{mailbox,
                     #{name => <<"John Doé">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: =?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{mailbox,
                     #{name => <<"John Doé"/utf8>>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: Group1:;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group, #{name => <<"Group1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: \"Group 1\":;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group, #{name => <<"Group 1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group, #{name => <<"Group d'été">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group, #{name => <<"Group d'été"/utf8>>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: Group1:\"John Doe\" <john.doe@example.com>;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doe">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: Group1:=?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: Group1:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com,\r\n"
        " Person1 <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:john.doe@example.com,\r\n"
        " \"Person.1\" <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{group,
                     #{name => <<"Group d'été"/utf8>>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person.1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: person1@example.com,\r\n"
        " person2@example.com,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{mailbox, #{address => <<"person1@example.com">>}},
                    {mailbox, #{address => <<"person2@example.com">>}},
                    {mailbox, #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-To: \"Person 1\" <person1@example.com>,\r\n"
        " \"Group 1\":;,\r\n"
        " \"Person 2\" <person2@example.com>,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_to,
                   [{mailbox,
                     #{name => <<"Person 1">>,
                       address => <<"person1@example.com">>}},
                    {group,
                     #{name => <<"Group 1">>}},
                    {mailbox,
                     #{name => <<"Person 2">>,
                       address => <<"person2@example.com">>}},
                    {mailbox,
                     #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   %% Resent-Cc header field
   ?_assertEqual(
      <<"Resent-Cc: JohnDoe <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{mailbox,
                     #{name => <<"JohnDoe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: \"John Doe\" <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{mailbox,
                     #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: john.doe@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{mailbox,
                     #{address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: =?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{mailbox,
                     #{name => <<"John Doé">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: =?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{mailbox,
                     #{name => <<"John Doé"/utf8>>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: Group1:;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group, #{name => <<"Group1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: \"Group 1\":;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group, #{name => <<"Group 1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group, #{name => <<"Group d'été">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group, #{name => <<"Group d'été"/utf8>>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: Group1:\"John Doe\" <john.doe@example.com>;\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doe">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: Group1:=?ISO-8859-1?Q?John=20Do=E9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: Group1:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:=?UTF-8?Q?John=20Do=C3=A9?= <john.doe@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: =?ISO-8859-1?Q?Group=20d'=E9t=E9?=:john.doe@example.com,\r\n"
        " Person1 <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: =?UTF-8?Q?Group=20d'=C3=A9t=C3=A9?=:john.doe@example.com,\r\n"
        " \"Person.1\" <person1@example.com>;\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{group,
                     #{name => <<"Group d'été"/utf8>>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person.1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: person1@example.com,\r\n"
        " person2@example.com,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{mailbox, #{address => <<"person1@example.com">>}},
                    {mailbox, #{address => <<"person2@example.com">>}},
                    {mailbox, #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"Resent-Cc: \"Person 1\" <person1@example.com>,\r\n"
        " \"Group 1\":;,\r\n"
        " \"Person 2\" <person2@example.com>,\r\n"
        " person3@example.com\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_cc,
                   [{mailbox,
                     #{name => <<"Person 1">>,
                       address => <<"person1@example.com">>}},
                    {group,
                     #{name => <<"Group 1">>}},
                    {mailbox,
                     #{name => <<"Person 2">>,
                       address => <<"person2@example.com">>}},
                    {mailbox,
                     #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   %% Resent-Bcc header field
   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{mailbox,
                     #{name => <<"JohnDoe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{mailbox,
                     #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{mailbox,
                     #{address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{mailbox,
                     #{name => <<"John Doé">>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{mailbox,
                     #{name => <<"John Doé"/utf8>>,
                       address => <<"john.doe@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group, #{name => <<"Group1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group, #{name => <<"Group 1">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group, #{name => <<"Group d'été">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group, #{name => <<"Group d'été"/utf8>>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doe">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé">>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group,
                     #{name => <<"Group1">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{name => <<"John Doé"/utf8>>,
                             address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group,
                     #{name => <<"Group d'été">>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{group,
                     #{name => <<"Group d'été"/utf8>>,
                       addresses =>
                         [{mailbox,
                           #{address => <<"john.doe@example.com">>}},
                          {mailbox,
                           #{name => <<"Person.1">>,
                             address => <<"person1@example.com">>}}]}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{mailbox, #{address => <<"person1@example.com">>}},
                    {mailbox, #{address => <<"person2@example.com">>}},
                    {mailbox, #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"\r\n\r\n">>,
      encode(#{header =>
                 [{resent_bcc,
                   [{mailbox,
                     #{name => <<"Person 1">>,
                       address => <<"person1@example.com">>}},
                    {group,
                     #{name => <<"Group 1">>}},
                    {mailbox,
                     #{name => <<"Person 2">>,
                       address => <<"person2@example.com">>}},
                    {mailbox,
                     #{address => <<"person3@example.com">>}}]}],
               body => EmptyBody})),

   %% Resent-Message-ID header field
   ?_assertEqual(
      <<"Resent-Message-ID: <123@imf.example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{resent_message_id, {<<"123">>, <<"imf.example.com">>}}],
               body => EmptyBody})),

   %% Return-Path header field
   ?_assertEqual(
      <<"Return-Path: <john.doe@example.com>\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{return_path, <<"john.doe@example.com">>}],
               body => EmptyBody})),

   %% Custom header field
   ?_assertEqual(
     <<"X-Internal-Field: hello\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>, <<"hello">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"X-Internal-Field: \"hello world\"\r\n\r\n\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>, <<"hello world">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"X-Internal-Field: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>, <<"Une journée d'été">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"X-Internal-Field: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>, <<"Une journée d'été"/utf8>>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"X-Internal-Field: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9=20c'est=20long,=20vraiment=20t?=\r\n"
        " =?ISO-8859-1?Q?r=E8s=20long...=20non=20mais=20genre=20vraiment=20tr=E8s?="
        " =?ISO-8859-1?Q?=20tr=E8s=20=20tr=E8s=20long=20=21?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>,
                   <<"Une journée d'été c'est long, vraiment très long..."
                     " non mais genre vraiment très très  très long !">>}],
               body => EmptyBody})),

   ?_assertEqual(
      <<"X-Internal-Field: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9=20c'est=20long,=20vraiment?=\r\n"
        " =?UTF-8?Q?=20tr=C3=A8s=20long...=20non=20mais=20genre=20vraiment=20tr?="
        " =?UTF-8?Q?=C3=A8s=20tr=C3=A8s=20=20tr=C3=A8s=20long=20=21?=\r\n"
        "\r\n"
        "\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>,
                   <<"Une journée d'été c'est long, vraiment très long..."
                     " non mais genre vraiment très très  très long !"/utf8>>}],
               body => EmptyBody}))].
