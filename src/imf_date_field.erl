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

-module(imf_date_field).

-export([encode/1, format/1]).

-spec encode(imf:date()) -> iodata().
encode(Value) ->
  [format(Value), "\r\n"].

-spec format(imf:date()) -> iodata().
format({Type, Datetime}) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = Datetime,
  DayOfTheWeek = calendar:day_of_the_week(Year, Month, Day),
  DayName = lists:nth(DayOfTheWeek, days()),
  MonthName = lists:nth(Month, months()),
  TZOffset = timezone_offset({Type, Datetime}),
  io_lib:format("~s, ~b ~s ~b ~2..0b:~2..0b:~2..0b ~s",
                [DayName, Day, MonthName, Year, Hour, Minute, Second,
                 TZOffset]).

-spec days() -> iodata().
days() ->
  ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"].

-spec months() -> iodata().
months() ->
 ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
  "Nov", "Dec"].

-spec timezone_offset(imf:date()) -> iodata().
timezone_offset({local, Datetime}) ->
  UniversalDatetime =
    case calendar:local_time_to_universal_time_dst(Datetime) of
      [] ->
        error({illegal_local_time, Datetime});
      [_, UTC] ->
        UTC;
      [UTC] ->
        UTC
    end,
  case calendar:datetime_to_gregorian_seconds(Datetime) -
    calendar:datetime_to_gregorian_seconds(UniversalDatetime) of
    DiffSec when DiffSec < 0 ->
      io_lib:format("-~4..0w", [trunc(abs((DiffSec / 3600) * 100))]);
    DiffSec ->
      io_lib:format("+~4..0w", [trunc(abs((DiffSec / 3600) * 100))])
  end;
timezone_offset({universal, _}) ->
  "+0000".
