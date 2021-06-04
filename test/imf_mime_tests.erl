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

-module(imf_mime_tests).

-include_lib("eunit/include/eunit.hrl").

encode(Part) ->
  iolist_to_binary(imf_mime:encode_part(Part)).

encode_part_test_() ->
  [%% Mime-Version header field
   ?_assertEqual(
      <<"Mime-Version: 1.0\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{mime_version, {1,0}}],
               body => {data, <<"Hello world">>}})),

   %% ContentType header field
   ?_assertEqual(
      <<"Content-Type: text/plain\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_type,
                   #{type => <<"text">>, subtype => <<"plain">>}}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Type: text/plain;\r\n"
        " a=\"foo\";\r\n"
        " b=\"bar\"\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_type,
                   #{type => <<"text">>, subtype => <<"plain">>,
                     parameters =>
                       #{<<"a">> => <<"foo">>, <<"b">> => <<"bar">>}}}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Type: text/plain;\r\n"
        " boundary=\"42\"\r\n"
        "\r\n"
        "--42\r\n"
        "Hello=20world\r\n"
        "--42--\r\n">>,
      encode(#{header =>
                 [{content_type,
                   #{type => <<"text">>, subtype => <<"plain">>,
                     parameters => #{<<"boundary">> => <<"42">>}}}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Type: text/plain;\r\n"
        " foo=\"bar\"\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_type,
                   #{type => <<"text">>, subtype => <<"plain">>,
                     parameters => #{<<"foo">> => <<"bar">>}}}],
               body => {data, <<"Hello world">>}})),

   %% Content-Transfer-Encoding header field
   ?_assertEqual(
      <<"Content-Transfer-Encoding: 7bit\r\n"
        "\r\n"
        "Hello world\r\n">>,
      encode(#{header =>
                 [{content_transfer_encoding, '7bit'}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Transfer-Encoding: 8bit\r\n"
        "\r\n"
        "Hello world\r\n">>,
      encode(#{header =>
                 [{content_transfer_encoding, '8bit'}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Transfer-Encoding: binary\r\n"
        "\r\n"
        "Hello world\r\n">>,
      encode(#{header =>
                 [{content_transfer_encoding, binary}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Mime-Version: 1.0\r\n"
        "Content-Type: text/plain;\r\n"
        " charset=\"UTF-8\"\r\n"
        "Content-Transfer-Encoding: quoted-printable\r\n"
        "Content-Disposition: inline\r\n"
        "\r\n"
        "Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9=20c'est=20long,=20vraiment=20tr=C3=A8sl=\n"
        "ong...=20non=20mais=20genre=20vraiment=20tr=C3=A8s=20tr=C3=A8s=20=20tr=C3=A8=\n"
        "s=20long=20=21\r\n"
        "\r\n">>,
      encode(
        imf_mime:main_part(
          imf_mime:text_plain(<<"Une journée d'été c'est long, vraiment très"
                                "long... non mais genre vraiment très très "
                                " très long !"/utf8>>)))),

   ?_assertEqual(
      <<"Mime-Version: 1.0\r\n"
        "Content-Type: text/plain;\r\n"
        " charset=\"ISO-8859-1\"\r\n"
        "Content-Transfer-Encoding: quoted-printable\r\n"
        "Content-Disposition: inline\r\n"
        "\r\n"
        "Une=20journ=E9e=20d'=E9t=E9=20c'est=20long,=20vraiment=20tr=E8slong...=20non=\n"
        "=20mais=20genre=20vraiment=20tr=E8s=20tr=E8s=20=20tr=E8s=20long=20=21\r\n"
        "\r\n">>,
      encode(
        imf_mime:main_part(
          imf_mime:text_plain(<<"Une journée d'été c'est long, vraiment très"
                                "long... non mais genre vraiment très très "
                                " très long !">>)))),
   ?_assertEqual(
      <<"Mime-Version: 1.0\r\n"
        "Content-Type: text/plain;\r\n"
        " charset=\"US-ASCII\"\r\n"
        "Content-Transfer-Encoding: quoted-printable\r\n"
        "Content-Disposition: inline\r\n"
        "\r\n"
        "Hello=20world!!\r\n"
        "\r\n">>,
      encode(
        imf_mime:main_part(
          imf_mime:text_plain(<<"Hello world!!">>)))),

   ?_assertEqual(
      <<"Content-Transfer-Encoding: base64\r\n"
        "\r\n"
        "SGVsbG8gd29ybGQ=\r\n">>,
      encode(#{header =>
                 [{content_transfer_encoding, base64}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Transfer-Encoding: base64\r\n"
        "\r\n"
        "V2hvb29vbyB0aGlzIGxpbmUgaXMgdmVyeSBsb25nLCBJIG1lYW4gdmVyeSB2ZXJ5IGxvbmcuIFRo\r\n"
        "aXMgc2hvdWxkIGJlIGZvbGQgb24gbXVsdGlsaW5lIHRvIGJlIGNvbXBsaWFudCB3aXRoIE1JTUUh\r\n">>,
      encode(#{header =>
                 [{content_transfer_encoding, base64}],
               body => {data, <<"Whooooo this line is very long, I mean very"
                                " very long. This should be fold on multiline"
                                " to be compliant with MIME!">>}})),

   %% Content-ID header field
   ?_assertEqual(
      <<"Content-ID: <123@localhost>\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_id, {<<"123">>, <<"localhost">>}}],
               body => {data, <<"Hello world">>}})),

   %% Content-Description header field
   ?_assertEqual(
      <<"Content-Description: \r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_description, <<>>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Description: \" \"\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_description, <<" ">>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Description: \"hello world\"\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_description, <<"hello world">>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Description: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9?=\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_description, <<"Une journée d'été">>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Description: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9?=\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_description, <<"Une journée d'été"/utf8>>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Description: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9=20c'est=20long,=20vraiment=20t?=\r\n"
        " =?ISO-8859-1?Q?r=E8s=20long...=20non=20mais=20genre=20vraiment=20tr=E8s?="
        " =?ISO-8859-1?Q?=20tr=E8s=20=20tr=E8s=20long=20=21?=\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_description,
                   <<"Une journée d'été c'est long, vraiment très long... "
                     "non mais genre vraiment très très  très long !">>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Description: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9=20c'est=20long,=20vraiment?=\r\n"
        " =?UTF-8?Q?=20tr=C3=A8s=20long...=20non=20mais=20genre=20vraiment=20tr?="
        " =?UTF-8?Q?=C3=A8s=20tr=C3=A8s=20=20tr=C3=A8s=20long=20=21?=\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_description,
                   <<"Une journée d'été c'est long, vraiment très long... "
                     "non mais genre vraiment très très  très long !"/utf8>>}],
               body => {data, <<"Hello world">>}})),

   %% Content-Disposition header field
   ?_assertEqual(
      <<"Content-Disposition: inline\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_disposition,
                   #{type => inline}}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Disposition: attachment\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_disposition,
                   #{type => attachment}}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Disposition: attachment;\r\n"
        " filename=foo.pdf\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_disposition,
                   #{type => attachment,
                     parameters => #{filename => <<"foo.pdf">>}}}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Disposition: attachment;\r\n"
        " creation-date=\"Fri, 28 May 2021 15:18:20 -0400\"\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_disposition,
                   #{type => attachment,
                     parameters =>
                       #{creation_date => {{2021,5,28},{15,18,20}}}}}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Disposition: attachment;\r\n"
        " modification-date=\"Fri, 28 May 2021 15:18:20 -0400\"\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_disposition,
                   #{type => attachment,
                     parameters =>
                       #{modification_date => {{2021,5,28},{15,18,20}}}}}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Disposition: attachment;\r\n"
        " read-date=\"Fri, 28 May 2021 15:18:20 -0400\"\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_disposition,
                   #{type => attachment,
                     parameters =>
                       #{read_date => {{2021,5,28},{15,18,20}}}}}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"Content-Disposition: attachment;\r\n"
        " size=100000987\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{content_disposition,
                   #{type => attachment,
                     parameters =>
                       #{size => 100_000_987}}}],
               body => {data, <<"Hello world">>}})),

   %% Custom MIME header fields
   ?_assertEqual(
      <<"X-Internal-Field: hello\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>, <<"hello">>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"X-Internal-Field: \"hello world\"\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>, <<"hello world">>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"X-Internal-Field: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9?=\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>, <<"Une journée d'été">>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"X-Internal-Field: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9?=\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>, <<"Une journée d'été"/utf8>>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"X-Internal-Field: =?ISO-8859-1?Q?Une=20journ=E9e=20d'=E9t=E9=20c'est=20long,=20vraiment=20t?=\r\n"
        " =?ISO-8859-1?Q?r=E8s=20long...=20non=20mais=20genre=20vraiment=20tr=E8s?="
        " =?ISO-8859-1?Q?=20tr=E8s=20=20tr=E8s=20long=20=21?=\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>,
                   <<"Une journée d'été c'est long, vraiment très long..."
                     " non mais genre vraiment très très  très long !">>}],
               body => {data, <<"Hello world">>}})),

   ?_assertEqual(
      <<"X-Internal-Field: =?UTF-8?Q?Une=20journ=C3=A9e=20d'=C3=A9t=C3=A9=20c'est=20long,=20vraiment?=\r\n"
        " =?UTF-8?Q?=20tr=C3=A8s=20long...=20non=20mais=20genre=20vraiment=20tr?="
        " =?UTF-8?Q?=C3=A8s=20tr=C3=A8s=20=20tr=C3=A8s=20long=20=21?=\r\n"
        "\r\n"
        "Hello=20world\r\n">>,
      encode(#{header =>
                 [{<<"X-Internal-Field">>,
                   <<"Une journée d'été c'est long, vraiment très long..."
                     " non mais genre vraiment très très  très long !"/utf8>>}],
               body => {data, <<"Hello world">>}}))].
