# Introduction
This repository contains an Erlang library designed to handle internet
message format generation and parsing.

# Capabilities
-  Internet Message Format - [RFC
   5322](https://tools.ietf.org/html/rfc5322)
- Internationalized Email Headers - [RFC
  6532](https://tools.ietf.org/html/rfc6532)
- Multipurpose Internet Mail Extensions (MIME)
  - [RFC 2045](https://tools.ietf.org/html/rfc2045)
  - [RFC 2046](https://tools.ietf.org/html/rfc2046)
  - [RFC 2047](https://tools.ietf.org/html/rfc2047)
  - [RFC 2048](https://tools.ietf.org/html/rfc2048)
  - [RFC 2049](https://tools.ietf.org/html/rfc2049)
  - [RFC 2183](https://tools.ietf.org/html/rfc2183)

# Build
You can build the library with:

    make build

# Test
You can execute the test suite with:

    make dialyzer test

You can generate the test coverage with:

    make cover

# Documentation
A handbook is available [in the `doc` directory](doc/handbook.md).

# Contact
If you find a bug or have any question, feel free to open a Github
issue.

Please not that we do not currently review or accept any contribution.

# Licence
Released under the ISC license.

Copyright (c) 2022 Bryan Frimin <bryan@frimin.fr>.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
