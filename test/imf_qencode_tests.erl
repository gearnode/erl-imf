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

-module(imf_qencode_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
  [?_assertEqual(
      <<"abc">>,
      imf_qencode:encode(<<"abc">>)),
   ?_assertEqual(
      <<"=?ISO-8859-1?Q?=E9t=E9?=">>,
      imf_qencode:encode(<<"été">>)),
   ?_assertEqual(
      <<"=?ISO-8859-1?Q?=E9t=E9=20=E9t=E9=E9t=E9=E9t=E9=E9t=E9=E9t=E9=E9t=E9=E9t?= =?ISO-8859-1?Q?=E9=E9t=E9=E9t=E9=E9t=E9=E9t=E9=E9t=E9=E9t=E9=E9t=E9=E9t?= =?ISO-8859-1?Q?=E9=E9t=E9=E9t=E9=E9t=E9=E9t=E9=E9t=E9?=">>,
      imf_qencode:encode(<<"été étéétéétéétéétéétéétéétéétéétéétéétéétéétéétéétéétéétéétéété">>)),
   ?_assertEqual(
      <<"=?UTF-8?Q?=C3=A9t=C3=A9?=">>,
      imf_qencode:encode(<<"été"/utf8>>)),
   ?_assertEqual(
      <<"=?UTF-8?Q?a=20$=20=C2=A2=20=E0=A4=B9=20=E2=82=AC=20=ED=95=9C=20?= =?UTF-8?Q?=F0=90=8D=88?=">>,
      imf_qencode:encode(<<"a $ ¢ ह € 한 𐍈"/utf8>>)),
   ?_assertEqual(
      <<"=?UTF-8?Q?=E2=82=AC=E2=82=AC=20=E2=82=AC=E2=82=AC=20=E2=82=AC=E2=82=AC=20?= =?UTF-8?Q?=E2=82=AC=E2=82=AC=E2=82=AC=E2=82=AC=E2=82=AC=E2=82=AC=E2=82=AC?= =?UTF-8?Q?=E2=82=AC=E2=82=AC=E2=82=AC=E2=82=AC=E2=82=AC=E2=82=AC=E2=82=AC?=">>,
      imf_qencode:encode(<<"€€ €€ €€ €€€€€€€€€€€€€€"/utf8>>))].
