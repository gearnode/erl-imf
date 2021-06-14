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

Supported header field name must be an atom. For non supported header
field the key must be a binary.

The following header fields are currently supported:

**The Origination Date Field:**
- `date`: The creation datetime of the message. The value is a
  [date](#types).

**Originator Fields:**
- `from`: The message authors of the message. The value is a list
  containing a mix of [mailboxes](#types) and [groups](#types).
- `sender`: The author responsible for the actual transmission of the
   message. The value is a [mailbox](#types).
- `reply_to`: The addresses to which the author of the message suggests
   that replies be sent. The value is a list of [addresses](#types).

**Destination Address Fields:**
- `to`: The recipient addresses. The value is a list containing a mix
  of [mailboxes](#types) and [groups](#types).
- `cc` The carbon copy recipient addresses. The value is a list
  containing a mix of [mailboxes](#types) and [groups](#types).
- `bcc` The blind carbon copy recipient addresses. The value is a list
  containing a mix of [mailboxes](#types) and [groups](#types).

**Identification Fields:**
- `message_id`: The globally unique message identifier. The value is a
  [message id](#types).
- `in_reply_to`: The unique identifier reference to the replied
  message. The value is a [message id](#types).
- `references`: Unique identifier references to related messages. The
  value is a list of [message id](#types).

**Informational Fields:**
- `subject`: The topic of the message. The value is an [unstructured
  field](#types).
- `comments`: Comments on the text of the body message. The value is an
  [unstructured field](#types)
- `keywords`: Important words and phrases that might be useful for the
   recipient. The value is a list of [phrase](#types).

**Resent Fields:**
- `resent_date`: Corresponds to the `date` field. The value is a
  [date](#types).
- `resent_from`: Corresponds to the `from` field. The value is list
  containing a mix of [mailboxes](#types) and [groups](#types).
- `resent_sender`: Corresponds to the `sender` field. The value is a
  [mailbox](#types).
- `resent_to`: Corresponds to the `to` field. The value is a list
  containing a mix of [mailboxes](#types) and [groups](#types).
- `resent_cc`: Corresponds to the `cc` field. The value is a list
  containing a mix of [mailboxes](#types) and [groups](#types).
- `resent_bcc`: Corresponds to the `bcc` field. The value is a list
  containing a mix of [mailboxes](#types) and [groups](#types).
- `resent_msg_id`: Corresponds to the `message_id` field. The value is a
  [message id](#types).

## Message body
Message body are represented as an Erlang map containing the following
entries:
- `header`: the set of header fields documented below.
- `body`: the representation of the message body documenetd below.

Header fields are represented as list of tuples; the first element of
each tuple is the name of the field and second element is the value of
the field.

Supported header field name must be an atom. For non supported header
field the key must be a binary.

The following header fields are currently supported:
- `mime_version`: The mime version used. The value must be `{1,0}`
  tuple.
- `content_type`: TODO
- `content_tranfer_encoding`: The content transfer encoding applied. The
  value can be one of the following atom:
  - `7bit`
  - `8bit`
  - `binary`
  - `quoted_printable`
  - `base64`
- `content_id`: The unique indenfier of the body part. The value is a
  [message id](#types).
- `content_description`: The description of message body part. The value
  is an [unstructured field](#types).
- `content_disposition`: TODO

# Decode
Decoding is not supported.
