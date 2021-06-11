# Introduction
This document contains development notes about the `imf` library.

# Versioning
The following `imf` versions are available:
- `0.y.z` unstable versions.
- `x.y.z` stable versions: `imf` will maintain reasonable backward
  compatibility, deprecating features before removing them.
- Experimental untagged versions.

Developers who use unstable or experimental versions are responsible for
updating their application when `imf` is modified. Note that
unstable versions can be modified without backward compatibility at any
time.

# Types
TODO describe type

# Message structure
The e-mail is represented as an Erlang map containing the following
entries:
- `header`: the set of header fields documented below.
- `body`: the representation of the message body documenetd below.

## Header fields
Header fields are represented as list of tuples; the first element of
each tuple is the name of the field and second element is the value of
the field.

- know header field atom
- unknow binary

The following header fields are currently supported:
- `date`: TODO
- `from`: TODO
- `sender`: TODO
- `reply_to`: TODO
- `to`: The recipient addresses. The value is a list containing a mix
  of [mailboxes](#types) and [groups](#types).
- `cc` The carbon copy recipient addresses. The value is a list
  containing a mix of [mailboxes](#types) and [groups](#types).
- `bcc` The blind carbon copy recipient addresses. The value is a list
  containing a mix of [mailboxes](#types) and [groups](#types).
- `message_id`: The globally unique message identifier. The value is a
  [message id](#types).
- `in_reply_to`: The unique identifier reference to the replied
  message. The value is a [message id](#types).
- `references`: Unique identifier references to related messages. The
  value is a list of [message id](#types).

## Message body


Example:
```erlang
imf:encode(
    #{header =>
        [{date, {localtime, calendar:local_time()}},
         {from,
          [{mailbox, #{name => <<"John Doe">>,
                       address => <<"john.doe@example.com">>}},
           {mailbox, #{address => <<"bryan@example.com">>}}]},
         {sender,
          {mailbox, #{name => <<"John Doe">>,
                      address => <<"john.doe@example.com">>}}},
         {to,
          [{mailbox, #{address => <<"person1@example.com">>}},
           {group, #{name => <<"Product Team">>}},
           {mailbox,
            #{name => <<"Person 2">>, address => <<"person2@example.com">>}},
           {group, #{name => <<"Engineering Team">>,
                     addresses =>
                       [{mailbox, #{address => <<"person3@example.com">>}},
                        {mailbox, #{address => <<"person4@example.com">>}}]}}]},
         {message_id, generate_message_id()},
         {references,
          [{<<"0ujsszwN8NRY24YaXiTIE2VWDTS">>, <<"workstation.example.com">>},
           {<<"0ujssxh0cECutqzMgbtXSGnjorm">>, <<"workstation.example.com">>},
           {<<"0ujsszgFvbiEr7CDgE3z8MAUPFt">>, <<"workstation.example.com">>}]},
         {subject, <<"Hello World!">>},
         {comments, <<"it's an important email">>},
         {keywords,
          [<<"engineering">>, <<"important">>]},
         {return_path, <<"john.doe@example.com">>}],
      body =>
        <<"hello world!">>})
```

# Decode
Decoding is not supported.
