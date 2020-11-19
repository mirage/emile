(** Emile module, parser of e-mail address. *)

(** An e-mail address can contain as a part of a {!phrase} (identifier) an
   encoded string. Standards describe 2 kinds of encoding:

   {ul
   {- Quoted Printable: used to insert hexadecimal value with the [=] operator.}
   {- Base 64: string encoded in MIME's Base64}}

   Parser already decodes encoded {!raw}, the client can use it as is. *)
type raw =
  | Quoted_printable of (string, [ `Msg of string ]) result
  | Base64 of (string, [ `Msg of string ]) result

type word = [ `Atom of string | `String of string ]
(** The local part of an e-mail address is composed by two kinds of {i word}s:

   {ul
   {- [`Atom] is string as is.}
   {- [`String] is a string surrounded by double-quote to allow white-space.}}

   The second kind is sanitized — we deleted double-quote which surround
   [string]. *)

type local = word list
(** Local part of e-mail address. *)

(** Subset of domain described by RFC5321 which contains 3 kinds of address:

   {ul
   {- [IPv4]: a valid IPv4 address}
   {- [IPv6]: a valid IPv6 address}
   {- [Ext (ldh, value)]: an extended kind of domain recognized by [ldh]
   identifier which valus is [value]}}

   Parser of [IPv4] and [IPv6] was done by [Ipaddr]. An extended kind [Ext]
   needs to be resolved by the client. *)
type addr =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Ext of (string * string)

type domain = [ `Domain of string list | `Addr of addr | `Literal of string ]
(** Domain part of e-mail address. A domain integrate kinds from RFC5321 (see
   {!addr}), a domain described by RFC5322 and a [`Literal] which is the last {i
   best-effort} value possible as a domain.

   [Emile] {b does not} resolve domain. *)

type phrase = [ `Dot | `Word of word | `Encoded of string * raw ] list
(** A phrase is a sentence to associate a name with an e-mail address or a group
   of e-mail addresses. [`Encoded] value {b is not} normalized on the {i
   charset} specified. The encoded's string is decoded as is only. For example,
   [`Encoded] can inform to use KOI-8 encoding (cyrillic charset). However,
   [Emile] does not check if value is a valid KOI-8 string, nor normalizes to
   unicode. [Emile] just decodes it as is. *)

type mailbox =
  { name: phrase option
  ; local: local
  ; domain: domain * domain list }
(** A mailbox is an e-mail address. It contains an optional name (see
   {!phrase}), a local-part (see {!local}) and one or more {!domain}(s). *)

type group =
  { group: phrase
  ; mailboxes: mailbox list }
(** A group is a named set of {!mailbox}. *)

type address = local * (domain * domain list)
(** A basic e-mail address. *)

type t = [ `Mailbox of mailbox | `Group of group ]
(** The {i Emile}'s t type which is a {i singleton} (only one {!mailbox}) or a
   {i list} of e-mail addresses (a {!group}). *)

(** {2 Pretty-printers.} *)

type 'a fmt = Format.formatter -> 'a -> unit

val pp_addr : addr fmt
val pp_domain : domain fmt
val pp_word : word fmt
val pp_local : local fmt
val pp_raw : raw fmt
val pp_phrase : phrase fmt
val pp_mailbox : mailbox fmt
val pp_group : group fmt
val pp_address : address fmt
val pp : t fmt

(** {2 Equal & Compare.} *)

type 'a equal = 'a -> 'a -> bool
type 'a compare = 'a -> 'a -> int

val case_sensitive : string -> string -> int
(** Alias of {!String.compare}. *)

val case_insensitive : string -> string -> int
(** [case_insensitive a b] maps values with {!lowercase_ascii} and compare them
   with {!String.compare}. We {b do not} map UTF8 value. *)

val equal_word : compare:string compare -> word equal
(** [equal ~compare a b] tests if {!word} [a] and {!word} [b] are semantically
   equal. [compare] specifies implementation to compare two [string] (i.e. to be
   case-sensitive or not). *)

val compare_word : ?case_sensitive:bool -> word compare
(** [compare_word ?case_sensitive a b] compares {!word} [a] and {!word} [b]
   semantically. From standards, {!word} SHOULD be case-sensitive, the client
   can notice this behaviour by [?case_sensitive] (default is [true]). *)

val equal_raw : compare:string compare -> raw equal
(** [equal_raw a b] tests if {!raw} [a] and {!raw} [b] are semantically equal.
   {i Semantically equal} means we compare raw's content, by this way, a
   [Base64] raw could be equal to a [Quoted_printable] raw if and only if
   [string] are equal. *)

val compare_raw : compare:string compare -> raw compare
(** [compare_raw a b] compares {!raw} [a] and {!raw} [b] semantically. *)

val equal_phrase : phrase equal
(** [equal_phrase a b] tests if {!phrase} [a] and {!phrase} [b] are semantically
   equal. In this case, the comparison is case-insensitive between elements in
   {!phrase}. The order of elements is important. *)

val compare_phrase : phrase compare
(** [compare_phrase a b] compares {!phrase} [a] and {!phrase} [b] semantically. *)

val equal_addr : addr equal
(** [equal_addr a b] tests if {!addr} [a] and {!addr} [b] are semantically
   equal. An [IPv4] should be equal with an [IPv6] address. Then, for extended
   kind, we strictly compare ({!Pervasives.compare}) kind and value. *)

val compare_addr : addr compare
(** [compare_addr a b] compares {!addr} [a] and {!addr} [b], we prioritize
   [IPv6], [IPv4] and finally [Ext]. *)

val equal_domain : domain equal
(** [equal_addr a b] tests if {!domain} [a] and {!domain} [b] are semantically
   equal. We {b do not} resolve domain, a [`Domain] could be semantically equal
   to another [`Domain] if they point to the same [IPv4]/[IPv6]. *)

val compare_domain : domain compare
(** [comapre_domain a b] compares {!domain} [a] and {!domain} [b], we prioritize
   [`Domain], [`Literal] and finally [`Addr]. The comparison between two
   [`Literal] and between part of [`Domain] are case-insensitive. *)

val equal_domains : (domain * domain list) equal
(** [equal_domains a b] apply {!equal_domain} to ordered domains (see
   {!compare_domain}) between [a] and [b]. *)

val compare_domains : (domain * domain list) compare
(** [compare_domains a b] compares ordered list of {!domain} [a] and ordered
   list of {!domain} [b]. *)

val equal_local : ?case_sensitive:bool -> local equal
(** [equal_local ?case_sensitive a b] tests if {!local} [a] and {!local} [b] are
   semantically equal. Standards notices local-part SHOULD be case-sensitive,
   the client can choose this behaviour with [case_sensitive]. *)

val compare_local : ?case_sensitive:bool -> local compare
(** [compare_local ?case_sensitive a b] compares {!local} [a] and {!local} [b]
   semantically. The user can decide if the comparison is case-sensitive or not
   (with [case_sensitive]). *)

val equal_mailbox : ?case_sensitive:bool -> mailbox equal
(** [equal_mailbox ?case_sensitive a b] tests if {!mailbox} [a] and {!mailbox}
   [b] are semantically equal. The user can define if the local-part need to be
   case-sensitive or not (by [case_sensitive]). If [a] xor [b] has a name, we
   consider [a = b] if we have the same local-part and same domain(s).
   Otherwise, we compare identifier/{!phrase} between them. *)

val compare_mailbox : ?case_sensitive:bool -> mailbox compare
(** [compare ?case_sensitive a b] compares {!mailbox} [a] and {!mailbxo} [b]
   semantically. We prioritize local-part, domain-part and finally optionnal
   name. *)

val compare_group : group compare
(** [comapre_group a b] compares {!group} [a] and {!group} [b]. We compare the
   group name first and compare ordered {!mailbox}es list then. *)

val equal_group : group equal
(** [equal_group a b] tests if {!group} [a] and {!group} [b] are semantically
   equal. We compare first group name and ordered {!mailbox}es list then. *)

val compare_address : address compare
(** [compare_address a b] compares semantically {!address} [a]* and {!address} [b]. *)

val equal_address : address equal
(** [equal_address a b] tests semantically {!address} [a] and {!address} [b]. *)

val equal_set : t equal
(** [equal a b] tests semantically {!set} [a] and {!set} [b]. *)

val compare_set : t compare
(** [compare a b] compares {!set} [a] and {!set} [b]. *)

(** {2 Parsers}

    If you don't want a headache, you should move on. *)

module Parser : sig
  (** This is an aggregation of rules used to parse an e-mail address. The goal
     of this documentation is to show relations between RFCs, updates, and final
     description of parts needed to parse an e-mail address.

      Obviously, this part is most a copy-paste from RFCs to explain what we
     implement. And for a client, it's a boring and indigestible (but needed)
     work. We provide implementations only for people know what they really need
     — and avoid duplicate code in some ways.

      But the biggest advise about this module is just to ignore it and move on
     — like what I really want when I wrote this documentation. *)

  val is_vchar : char -> bool
  (** From {{:https://tools.ietf.org/html/rfc5234#appendix-B.1}RFC5234} (used in
     {{:https://tools.ietf.org/html/rfc5322#section-3.1}RFC5322}).

      {[VCHAR = %x21-7E ; visible (printing) characters]} *)

  val is_obs_no_ws_ctl : char -> bool
  (** From {{:https://tools.ietf.org/html/rfc5322#section-4.1}RFC5322}.

      {[
obs-NO-WS-CTL = %d1-8 /   ; US-ASCII control
                %d11 /    ;  characters that do not
                %d12 /    ;  include the carriage
                %d14-31 / ;  return, line feed, and
                %d127     ;  white space characters
      ]} *)

  val is_ctext : char -> bool
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
ctext = <any CHAR excluding "(",          ; => may be folded
         ")", BACKSLASH & CR, & including
         linear-white-space>
      ]}

      From {{:https://tools.ietf.org/html/rfc1522}RFC1522} (occurrences).

      {[
5. Use of encoded-words in message headers

  (2) An encoded-word may appear within a comment delimited by "(" and
      ")", i.e., wherever a "ctext" is allowed.  More precisely, the
      RFC 822 ABNF definition for "comment" is amended as follows:

      comment = "(" *(ctext / quoted-pair / comment / encoded-word) ")"

      A "Q"-encoded encoded-word which appears in a comment MUST NOT
      contain the characters "(", ")" or DQUOTE encoded-word that
      appears in a "comment" MUST be separated from any adjacent
      encoded-word or "ctext" by linear-white-space.

7. Conformance

  A mail reading program claiming compliance with this specification
  must be able to distinguish encoded-words from "text", "ctext", or
  "word"s, according to the rules in section 6, anytime they appear in
  appropriate places in message headers.  It must support both the "B"
  and "Q" encodings for any character set which it supports.  The
  program must be able to display the unencoded text if the character
      ]}

      From {{:https://tools.ietf.org/html/rfc2047}RFC2047} § Appendix.

      {[
+ clarification: an 'encoded-word' may appear immediately following
  the initial "(" or immediately before the final ")" that delimits a
  comment, not just adjacent to "(" and ")" *within* *ctext.
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.1}RFC2822}.

      {[
ctext = NO-WS-CTL / ; Non white space controls
        %d33-39 /   ; The rest of the US-ASCII
        %d42-91 /   ;  characters not including "(",
        %d93-126    ;  ")", or BACKSLASH
      ]}

      From {{:https://tools.ietf.org/rfc5322#section-3.2.2}RFC5322}.

      {[
ctext     = %d33-39 /     ; Printable US-ASCII
            %d42-91 /     ;  characters not including
            %d93-126 /    ;  "(", ")", or BACKSLASH
            obs-ctext
obs-ctext = obs-NO-WS-CTL

Update from RFC 2822
+ Removed NO-WS-CTL from ctext
      ]}

      From {{:https://tools.ietf.org/html/rfc5335#section-4.3}RFC5335}.

      {[
ctext          =/ UTF8-xtra-char
UTF8-xtra-char =  UTF8-2 / UTF8-3 / UTF8-4
UTF8-2         =  %xC2-DF UTF8-tail
UTF8-3         =  %xE0 %xA0-BF UTF8-tail /
                  %xE1-EC 2(UTF8-tail) /
                  %xED %x80-9F UTF8-tail /
                  %xEE-EF 2(UTF8-tail)
UTF8-4         =  %xF0 %x90-BF 2( UTF8-tail ) /
                  %xF1-F3 3( UTF8-tail ) /
                  %xF4 %x80-8F 2( UTF8-tail )
UTF8-tail      =  %x80-BF
      ]}

      From {{:https://tools.ietf.org/html/rfc6532#section-3.2}RFC6532}.

      {[
ctext =/ UTF8-non-ascii
      ]}

      {b Note about UTF-8, the process is out of this scope where we check only one byte here.}
      {b Note about compliance with RFC1522, it's out of scope where we check only one byte here.}
  *)

  val is_qtext : char -> bool
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
qtext = <any CHAR excepting DQUOTE,    ; => may be folded
         BACKSLASH & CR, and including
         linear-white-space>
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.5}RFC2822}.

      {[
qtext = NO-WS-CTL / ; Non white space controls
        %d33 /      ; The rest of the US-ASCII
        %d35-91 /   ;  characters not including BACKSLASH
        %d93-126    ;  or the quote character
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.4}RFC5322}.

      {[
qtext     = %d33 /        ; Printable US-ASCII
            %d35-91 /     ;  characters not including
            %d93-126 /    ;  BACKSLASH or the quote character
            obs-qtext
obs-qtext = obs-NO-WS-CTL
      ]}

      From {{:https://tools.ietf.org/html/rfc5335#section-4.3}RFC5335} (see {!is_ctext} about [UTF-xtra-char]).

      {[
utf8-qtext = qtext / UTF8-xtra-char
      ]}

      From {{:https://tools.ietf.org/html/rfc6532#section-3.2}RFC6532}.

      {[
qtext =/ UTF8-non-ascii
      ]}

      {b Note about UTF-8, the process is out of this scope where we check only one byte here.}
  *)

  val is_atext : char -> bool
  (** The ABNF of [atext] is not explicit from RFC822 but the relic could be find {{:https://tools.ietf.org/html/rfc822#section-3.3}here}.

      {[
atom = 1*<any CHAR except specials, SPACE and CTLs>
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.4}RFC2822}.

      {[
atext = ALPHA / DIGIT / ; Any character except controls,
        "!" / "#" /     ;  SP, and specials.
        "$" / "%" /     ;  Used for atoms
        "&" / "'" /
        "*" / "+" /
        "-" / "/" /
        "=" / "?" /
        "^" / "_" /
        "`" / "{" /
        "|" / "}" /
        "~"
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.3}RFC5322}.

      {[
atext = ALPHA / DIGIT / ; Printable US-ASCII
        "!" / "#" /     ;  characters not including
        "$" / "%" /     ;  specials.  Used for atoms.
        "&" / "'" /
        "*" / "+" /
        "-" / "/" /
        "=" / "?" /
        "^" / "_" /
        "`" / "{" /
        "|" / "}" /
        "~"
      ]}

      From {{:https://tools.ietf.org/html/rfc5335#section-3.2.4}RFC535} (see {!is_ctext} about [UTF-xtra-char]).

      {[
utf8-atext = ALPHA / DIGIT /
             "!" / "#" /     ; Any character except
             "$" / "%" /     ; controls, SP, and specials.
             "&" / "'" /     ; Used for atoms.
             "*" / "+" /
             "-" / "/" /
             "=" / "?" /
             "^" / "_" /
             "`" / "{" /
             "|" / "}" /
             "~" /
             UTF8-xtra-char
      ]}

      From {{:https://tools.ietf.org/html/rfc6532#section-3.2}RFC6532}.

      {[
atext =/ UTF8-non-ascii
      ]}

      {b Note about, UTF-8, the process is out of this scope where we check only byte here.}
  *)

  val is_wsp : char -> bool
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
LWSP-char = SPACE / HTAB ; semantics = SPACE
      ]}

      From RFC2882 and RFC5322, we did not find any occurrence of LWSP-char. It replaced by WSP (available on {{:https://tools.ietf.org/html/rfc5234#appendix-B.1}RFC5234}).
  *)

  val is_quoted_pair : char -> bool
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
quoted-pair = BACKSLASH CHAR ; may quote any char
CHAR is case-sensitive
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.2}RFC2822}.

      {[
quoted-pair = (BACKSLASH text) / obs-qp
text        = %d1-9 /                     ; Characters excluding CR and LF
              %d11 /
              %d12 /
              %d14-127 /
              obs-text
obs-text    = *LF *CR *(obs-char *LF *CR)
obs-char    = %d0-9 / %d11 /              ; %d0-127 except CR and
              %d12 / %d14-127             ;  LF
obs-qp      = BACKSLASH (%d0-127)
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.1}RFC5322}.

      {[
quoted-pair = (BACKSLASH (VCHAR / WSP)) / obs-qp
obs-qp      = BACKSLASH (%d0 / obs-NO-WS-CTL / LF / CR)
      ]}

      From {{:https://tools.ietf.org/html/rfc5335#section-4.3}RFC5335} (see {!is_ctext} about [UTF-xtra-char]).

      {[
utf8-text        = %d1-9 /         ; all UTF-8 characters except
                   %d11-12 /       ; US-ASCII NUL, CR, and LF
                   %d14-127 /
                   UTF8-xtra-char
utf8-quoted-pair = (BACKSLASH utf8-text) / obs-qp
      ]}

      {b Note this function is [fun _chr -> true].}
      {b Note RFC5322 (last version of e-mail) does not mention an update from RFC2822. RFC6532 does not mention an update of [quoted-pair]. This implemention follow RFC5322 {b without} unicode support.}
  *)

  val is_dtext : char -> bool
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
dtext = <any CHAR excluding "[",          ; => may be folded
         "]", BACKSLASH & CR, & including
         linear-white-space>
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.4.1}RFC2822}.

      {[
dtext = NO-WS-CTL / ; Non white space controls

        %d33-90 /   ; The rest of the US-ASCII
        %d94-126    ;  characters not including "[",
                    ;  "]", or BACKSLASH
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.4.1}RFC5322}.

      {[
+ Removed NO-WS-CTL from dtext

dtext     = %d33-90 /                   ; Printable US-ASCII
            %d94-126 /                  ;  characters not including
            obs-dtext                   ;  "[", "]", or BACKSLASH
obs-dtext = obs-NO-WS-CTL / quoted-pair
      ]}

      {b Note [quoted-pair] can not be processed here where we handle only one byte.}
  *)

  val quoted_pair : char Angstrom.t
  (** See {!is_quoted_pair}. *)

  val fws : string Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.2.1}RFC822}.

      {[
Each header field can be viewed as a single, logical  line  of
ASCII  characters,  comprising  a field-name and a field-body.
For convenience, the field-body  portion  of  this  conceptual
entity  can be split into a multiple-line representation; this
is called "folding".  The general rule is that wherever  there
may  be  linear-white-space  (NOT  simply  LWSP-chars), a CRLF
immediately followed by AT LEAST one LWSP-char may instead  be
inserted.  Thus, the single line

    To:  "Joe & J. Harvey" <ddd @Org>, JJV @ BBN

can be represented as:

    To:  "Joe & J. Harvey" <ddd @ Org>,
            JJV@BBN

and

    To:  "Joe & J. Harvey"
                    <ddd@ Org>, JJV
     @BBN

and

    To:  "Joe &
     J. Harvey" <ddd @ Org>, JJV @ BBN

     The process of moving  from  this  folded   multiple-line
representation  of a header field to its single line represen-
tation is called "unfolding".  Unfolding  is  accomplished  by
regarding   CRLF   immediately  followed  by  a  LWSP-char  as
equivalent to the LWSP-char.

Note:  While the standard  permits  folding  wherever  linear-
       white-space is permitted, it is recommended that struc-
       tured fields, such as those containing addresses, limit
       folding  to higher-level syntactic breaks.  For address
       fields, it  is  recommended  that  such  folding  occur
       between addresses, after the separating comma.
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.3}RFC2822 § 3.2.3} & {{:https://tools.ietf.org/html/rfc2822#section-4.2}RFC2822 § 4.2}.

      {[
White space characters, including white space used in folding
(described in section 2.2.3), may appear between many elements in
header field bodies.  Also, strings of characters that are treated as
comments may be included in structured field bodies as characters
enclosed in parentheses.  The following defines the folding white
space (FWS) and comment constructs.

Strings of characters enclosed in parentheses are considered comments
so long as they do not appear within a "quoted-string", as defined in
section 3.2.5.  Comments may nest.

There are several places in this standard where comments and FWS may
be freely inserted.  To accommodate that syntax, an additional token
for "CFWS" is defined for places where comments and/or FWS can occur.
However, where CFWS occurs in this standard, it MUST NOT be inserted
in such a way that any line of a folded header field is made up
entirely of WSP characters and nothing else.

FWS = ([*WSP CRLF] 1*WSP) /   ; Folding white space
      obs-FWS

In the obsolete syntax, any amount of folding white space MAY be
inserted where the obs-FWS rule is allowed.  This creates the
possibility of having two consecutive "folds" in a line, and
therefore the possibility that a line which makes up a folded header
field could be composed entirely of white space.

obs-FWS = 1*WSP *(CRLF 1*WSP)
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.2}RFC5322 § 3.2.2} & {{:https://tools.ietf.org/rfc5322#section-4.2}RFC322 § 4.2}.

      {[
White space characters, including white space used in folding
(described in section 2.2.3), may appear between many elements in
header field bodies.  Also, strings of characters that are treated as
comments may be included in structured field bodies as characters
enclosed in parentheses.  The following defines the folding white
space (FWS) and comment constructs.

Strings of characters enclosed in parentheses are considered comments
so long as they do not appear within a "quoted-string", as defined in
section 3.2.4.  Comments may nest.

There are several places in this specification where comments and FWS
may be freely inserted.  To accommodate that syntax, an additional
token for "CFWS" is defined for places where comments and/or FWS can
occur.  However, where CFWS occurs in this specification, it MUST NOT
be inserted in such a way that any line of a folded header field is
made up entirely of WSP characters and nothing else.

FWS = ([*WSP CRLF] 1*WSP) / obs-FWS ; Folding white space

In the obsolete syntax, any amount of folding white space MAY be
inserted where the obs-FWS rule is allowed.  This creates the
possibility of having two consecutive "folds" in a line, and
therefore the possibility that a line which makes up a folded header
field could be composed entirely of white space.

obs-FWS = 1*WSP *(CRLF 1*WSP)
      ]}

      {b NOTE} [FWS] token is a part of an email content and it does not
      correspond to real/usual input like form input. Implementation discard,
      by implementation, it where something else must handle folding whitespace.

      [unstrctrd] is a library which handles [FWS] but [emile] just wants to
      parse correctly an email address (either if it comes from an email or not).
      At the end, [emile] {b is not} compliant to RFC822.
  *)

  val comment : unit Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
comment = "(" *(ctext / quoted-pair / comment) ")"
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.3}RFC2822}.

      {[
ccontent = ctext / quoted-pair / comment
comment  = "(" *([FWS] ccontent) [FWS] ")"
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.2}RFC5322}.

      {[
ccontent = ctext / quoted-pair / comment
comment  = "(" *([FWS] ccontent) [FWS] ")"
      ]}
  *)

  val cfws : unit Angstrom.t
  (** From RFC822, see {!fws} and {!obs_fws}.

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.3}RFC2822}.

      {[
CFWS = *([FWS] comment) (([FWS] comment) / FWS)
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.2}RFC5322}.

      {[
CFWS = (1*([FWS] comment) [FWS]) / FWS

Update from RFC 2822:
+ Simplified CFWS syntax.
      ]}
  *)

  val qcontent : string Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
quoted-string = DQUOTE *(qtext/quoted-pair) DQUOTE ; Regular qtext or
                                                   ;   quoted chars.

      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.5}RFC2822}.

      {[
qcontent = qtext / quoted-pair
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.4}RFC5322}.

      {[
qcontent = qtext / quoted-pair
      ]}

      From {{:https://tools.ietf.org/html/rfc5335#section-4.3}RFC5355}.

      {[
utf8-qcontent = utf8-qtext / utf8-quoted-pair
qcontent = utf8-qcontent
      ]}
  *)

  val quoted_string : string Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
quoted-string = DQUOTE *(qtext/quoted-pair) DQUOTE ; Regular qtext or
                                                   ;   quoted chars.
      ]}

      From {{:https://tools.ietf.org/html/rfc2047}RFC2047}.

      {[
+ An 'encoded-word' MUST NOT appear within a 'quoted-string'
      ]}

      From {{:https://tools.ietf.org/html/rfc2822}RFC2822}.

      {[
quoted-string = [CFWS]
                DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                [CFWS]

A quoted-string is treated as a unit.  That is, quoted-string is
identical to atom, semantically.  Since a quoted-string is allowed to
contain FWS, folding is permitted.  Also note that since quoted-pair
is allowed in a quoted-string, the quote and backslash characters may
appear in a quoted-string so long as they appear as a quoted-pair.

Semantically, neither the optional CFWS outside of the quote
characters nor the quote characters themselves are part of the
quoted-string; the quoted-string is what is contained between the two
quote characters.  As stated earlier, the BACKSLASH in any quoted-pair
and the CRLF in any FWS/CFWS that appears within the quoted-string are
semantically "invisible" and therefore not part of the quoted-string
either.
      ]}

      {b Note in other words, space(s) in [FWS] are "visible" between DQUOTE.}

      From {{:https://tools.ietf.org}RFC5322}.

      {[
quoted-string = [CFWS]
                DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                [CFWS]
      ]}

      {b Note currenlty, this implementation has a bug about multiple spaces in [quoted-string]. We need to update {!fws} to count how many space(s) we skip.}
  *)

  val atom : string Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
atom = 1*<any CHAR except specials, SPACE and CTLs>

Difference from RFC 733:
- Atoms may not contain SPACE.
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.4}RFC2822}.

      {[
atom = [CFWS] 1*atext [CFWS]
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.3}RFC5322}.

      {[
atom = [CFWS] 1*atext [CFWS]
      ]}

      From {{:https://tools.ietf.org/html/rfc5335#section-4.3}RFC5335}.

      {[
utf8-atom = [CFWS] 1*utf8-atext [CFWS]
      ]}
  *)

  val word : word Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
word = atom / quoted-string
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.6}RFC2822}.

      {[
word = atom / quoted-string
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.5}RFC5322}.

      {[
word = atom / quoted-string
      ]}
  *)

  val dot_atom_text : string list Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc2822#section-3.2.4}RFC2822}.

      {[
dot-atom-text = 1*atext *("." 1*atext)
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.3}RFC5322}.

      {[
dot-atom-text = 1*atext *("." 1*atext)
      ]}
  *)

  val dot_atom : string list Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc2822#section-3.2.4}RFC2822}.

      {[
dot-atom = [CFWS] dot-atom-text [CFWS]
      ]}

      From {{:https://tools.ietf.org/rfc5322#section-3.2.3}RFC5322}.

      {[
dot-atom = [CFWS] dot-atom-text [CFWS]
      ]}
  *)

  val local_part : local Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-}RFC822}.

      {[
local-part = word *("." word) ; uninterpreted
                              ; case-preserved

The local-part of an  addr-spec  in  a  mailbox  specification
(i.e.,  the  host's  name for the mailbox) is understood to be
whatever the receiving mail protocol server allows.  For exam-
ple,  some systems do not understand mailbox references of the
form "P. D. Q. Bach", but others do.

This specification treats periods (".") as lexical separators.
Hence,  their  presence  in  local-parts which are not quoted-
strings, is detected.   However,  such  occurrences  carry  NO
semantics.  That is, if a local-part has periods within it, an
address parser will divide the local-part into several tokens,
but  the  sequence  of  tokens will be treated as one uninter-
preted unit.  The sequence  will  be  re-assembled,  when  the
address is passed outside of the system such as to a mail pro-
tocol service.

For example, the address:

                   First.Last@Registry.Org

is legal and does not require the local-part to be  surrounded
with  quotation-marks.   (However,  "First  Last" DOES require
quoting.)  The local-part of the address, when passed  outside
of  the  mail  system,  within  the  Registry.Org  domain,  is
"First.Last", again without quotation marks.
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.4.1}RFC2822 § 3.4.1} & {{:https://tools.ietf.org/html/rfc2822#section-4.4}RFC2822 § 4.4}.

      {[
local-part = dot-atom / quoted-string / obs-local-part
obs-local-part = word *("." word)

The local-part portion is a domain dependent string.  In addresses,
it is simply interpreted on the particular host as a name of a
particular mailbox.

Update:
+ CFWS within local-parts and domains not allowed.*
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.4.1}RFC5322 § 3.4.1} & {{:https://tools.ietf.org/html/rfc5322#section-4.4}RFC5322 § 4.4}.

      {[
local-part = dot-atom / quoted-string / obs-local-part
obs-local-part = word *("." word)
      ]}
  *)

  val obs_local_part : local Angstrom.t
  (** See {!local_part}. *)

  val domain_literal : string Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
domain-literal = "[" *(dtext / quoted-pair) "]"

o   Square brackets ("[" and "]") are used to indicate the
    presence  of  a  domain-literal, which the appropriate
    name-domain  is  to  use  directly,  bypassing  normal
    name-resolution mechanisms.

Domain-literals which refer to domains within the ARPA  Inter-
net  specify  32-bit  Internet addresses, in four 8-bit fields
noted in decimal, as described in Request for  Comments  #820,
"Assigned Numbers."  For example:

                         [10.0.3.19]

Note:  THE USE OF DOMAIN-LITERALS IS STRONGLY DISCOURAGED.  It
       is  permitted  only  as  a means of bypassing temporary
       system limitations, such as name tables which  are  not
       complete.
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.4.1}RFC2822}.

      {[
domain-literal = [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.4.1}RFC5322}.

      {[
domain-literal = [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
      ]}
  *)

  val obs_domain : string list Angstrom.t

  val domain : domain Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-6.1}RFC822 § 6.1},
      {{:https://tools.ietf.org/html/rfc822#section-6.2.1}RFC822 § 6.2.1},
      {{:https://tools.ietf.org/html/rfc822#section-6.2.2}RFC822 § 6.2.2}
      & {{:https://tools.ietf.org/html/rfc822#section-6.2.3}RFC822 § 6.2.3}.

      {[
domain = sub-domain *("." sub-domain)
sub-domain = domain-ref / domain-literal
domain-ref = atom                        ; symbolic reference

6.2.1.  DOMAINS

A name-domain is a set of registered (mail)  names.   A  name-
domain  specification  resolves  to  a subordinate name-domain
specification  or  to  a  terminal  domain-dependent   string.
Hence,  domain  specification  is  extensible,  permitting any
number of registration levels.

Name-domains model a global, logical, hierarchical  addressing
scheme.   The  model is logical, in that an address specifica-
tion is related to name registration and  is  not  necessarily
tied  to  transmission  path.   The  model's  hierarchy  is  a
directed graph, called an in-tree, such that there is a single
path  from  the root of the tree to any node in the hierarchy.
If more than one path actually exists, they are considered  to
be different addresses.

The root node is common to all addresses; consequently, it  is
not  referenced.   Its  children  constitute "top-level" name-
domains.  Usually, a service has access to its own full domain
specification and to the names of all top-level name-domains.

The "top" of the domain addressing hierarchy -- a child of the
root  --  is  indicated  by  the right-most field, in a domain
specification.  Its child is specified to the left, its  child
to the left, and so on.

Some groups provide formal registration services;  these  con-
stitute   name-domains   that  are  independent  logically  of
specific machines.  In addition, networks and machines  impli-
citly  compose name-domains, since their membership usually is
registered in name tables.

In the case of formal registration, an organization implements
a  (distributed)  data base which provides an address-to-route
mapping service for addresses of the form:

                 person@registry.organization

Note that "organization" is a logical  entity,  separate  from
any particular communication network.

A mechanism for accessing "organization" is universally avail-
able.   That mechanism, in turn, seeks an instantiation of the
registry; its location is not indicated in the address specif-
ication.   It  is assumed that the system which operates under
the name "organization" knows how to find a subordinate regis-
try.  The registry will then use the "person" string to deter-
mine where to send the mail specification.

The latter,  network-oriented  case  permits  simple,  direct,
attachment-related address specification, such as:

                      user@host.network

Once the network is accessed, it is expected  that  a  message
will  go  directly  to the host and that the host will resolve
the user name, placing the message in the user's mailbox.

6.2.2.  ABBREVIATED DOMAIN SPECIFICATION

Since any number of  levels  is  possible  within  the  domain
hierarchy,  specification  of  a  fully  qualified address can
become inconvenient.  This standard permits abbreviated domain
specification, in a special case:

    For the address of  the  sender,  call  the  left-most
    sub-domain  Level  N.   In a header address, if all of
    the sub-domains above (i.e., to the right of) Level  N
    are  the same as those of the sender, then they do not
    have to appear in the specification.   Otherwise,  the
    address must be fully qualified.

    This feature is subject  to  approval  by  local  sub-
    domains.   Individual  sub-domains  may  require their
    member systems, which originate mail, to provide  full
    domain  specification only.  When permitted, abbrevia-
    tions may be present  only  while  the  message  stays
    within the sub-domain of the sender.

    Use of this mechanism requires the sender's sub-domain
    to reserve the names of all top-level domains, so that
    full specifications can be distinguished from abbrevi-
    ated specifications.

For example, if a sender's address is:

         sender@registry-A.registry-1.organization-X

and one recipient's address is:

        recipient@registry-B.registry-1.organization-X

and another's is:

        recipient@registry-C.registry-2.organization-X

then ".registry-1.organization-X" need not be specified in the
the  message,  but  "registry-C.registry-2"  DOES  have  to be
specified.  That is, the first two addresses may  be  abbrevi-
ated, but the third address must be fully specified.

When a message crosses a domain boundary, all  addresses  must
be  specified  in  the  full format, ending with the top-level
name-domain in the right-most field.  It is the responsibility
of  mail  forwarding services to ensure that addresses conform
with this requirement.  In the case of abbreviated  addresses,
the  relaying  service must make the necessary expansions.  It
should be noted that it often is difficult for such a  service
to locate all occurrences of address abbreviations.  For exam-
ple, it will not be possible to find such abbreviations within
the  body  of  the  message.   The "Return-Path" field can aid
recipients in recovering from these errors.

Note:  When passing any portion of an addr-spec onto a process
       which  does  not interpret data according to this stan-
       dard (e.g., mail protocol servers).  There must  be  NO
       LWSP-chars  preceding  or  following the at-sign or any
       delimiting period ("."), such as  shown  in  the  above
       examples,   and   only  ONE  SPACE  between  contiguous
       <word>s.

6.2.3.  DOMAIN TERMS

A domain-ref must be THE official name of a registry, network,
or  host.   It  is  a  symbolic  reference, within a name sub-
domain.  At times, it is necessary to bypass standard  mechan-
isms  for  resolving  such  references,  using  more primitive
information, such as a network host address  rather  than  its
associated host name.

To permit such references, this standard provides the  domain-
literal  construct.   Its contents must conform with the needs
of the sub-domain in which it is interpreted.

Domain-literals which refer to domains within the ARPA  Inter-
net  specify  32-bit  Internet addresses, in four 8-bit fields
noted in decimal, as described in Request for  Comments  #820,
"Assigned Numbers."  For example:

                         [10.0.3.19]

Note:  THE USE OF DOMAIN-LITERALS IS STRONGLY DISCOURAGED.  It
       is  permitted  only  as  a means of bypassing temporary
       system limitations, such as name tables which  are  not
       complete.

The names of "top-level" domains, and  the  names  of  domains
under  in  the  ARPA Internet, are registered with the Network
Information Center, SRI International, Menlo Park, California.
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.4.1}RFC2822 § 3.4.1} & {{:https://tools.ietf.org/html/rfc2822#section-4.4}RFC2822 § 4.4}.

      {[
domain = dot-atom / domain-literal / obs-domain
obs-domain = atom *("." atom)
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.4.1}RFC5322 § 3.4.1} & {{:https://tools.ietf.org/html/rfc5322#section-4.4}RFC5322 § 4.4}.

      {[
domain = dot-atom / domain-literal / obs-domain
obs-domain = atom *("." atom)
      ]}

      {b Note from RFC5322, we should accept any domain as [`Literal] and let the}
     user to resolve it. Currently, we fail when we catch a [`Literal] and do
     the best effort where we follow RFC5321. But may be it's inconvenient (or
     not?) to fail.
  *)

  val id_left : local Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc2822#section-3.6.4}RFC2822 § 3.6.4} & {{:https://tools.ietf.org/html/rfc2822#section-4.5.4}RFC2822 § 4.5.4}.

      {[
obs-id-left = local-part
no-fold-quote = DQUOTE *(qtext / quoted-pair) DQUOTE
id-left = dot-atom-text / no-fold-quote / obs-id-left
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.6.4}RFC5322 § 3.6.4} & {{:https://tools.ietf.org/html/rfc5322#section-4.5.4}RFC5322 § 4.5.4}.

      {[
id-left = dot-atom-text / obs-id-left
obs-id-left = local-part
      ]}
  *)

  val no_fold_literal : string Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc2822#section-3.6.4}RFC2822}.

      {[
no-fold-literal = "[" *(dtext / quoted-pair) "]"
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.6.4}RFC5322}.

      {[
no-fold-literal = "[" *dtext "]"
      ]}
  *)

  val id_right : domain Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc2822#section-3.6.4}RFC2822 § 3.6.4} & {{:https://tools.ietf.org/html/rfc2822#section-4.5.4}RFC2822 § 4.5.4}.

      {[
id-right = dot-atom-text / no-fold-literal / obs-id-right
obs-id-right = domain
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.6.4}RFC5322 § 3.6.4} & {{:https://tools.ietf.org/html/rfc5322#section-4.5.4}RFC5322 § 4.5.4}.

      {[
id-right = dot-atom-text / no-fold-literal / obs-id-right
obs-id-right = domain
      ]}
  *)

  val msg_id : (local * domain) Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-4.1}RFC822 § 4.1} & {{:https://tools.ietf.org/html/rfc822#section-6.1}RFC822 § 6.1}.

      {[
addr-spec = local-part "@" domain ; global address
msg-id = "<" addr-spec ">" ; Unique message id
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.6.4}RFC2822}.

      {[
msg-id = [CFWS] "<" id-left "@" id-right ">" [CFWS]

Update:
+ CFWS within msg-id not allowed.*

The message identifier (msg-id) is similar in syntax to an angle-addr
construct without the internal CFWS.
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.6.4}RFC5322}.

      {[
msg-id = [CFWS] "<" id-left "@" id-right ">" [CFWS]

Update:
+ Removed no-fold-quote from msg-id.  Clarified syntax

The message identifier (msg-id) itself MUST be a globally unique
identifier for a message.  The generator of the message identifier
MUST guarantee that the msg-id is unique.  There are several
algorithms that can be used to accomplish this.  Since the msg-id has
a similar syntax to addr-spec (identical except that quoted strings,
comments, and folding white space are not allowed), a good method is
to put the domain name (or a domain literal IP address) of the host
on which the message identifier was created on the right-hand side of
the "@" (since domain names and IP addresses are normally unique),
and put a combination of the current absolute date and time along
with some other currently unique (perhaps sequential) identifier
available on the system (for example, a process id number) on the
left-hand side.  Though other algorithms will work, it is RECOMMENDED
that the right-hand side contain some domain identifier (either of
the host itself or otherwise) such that the generator of the message
identifier can guarantee the uniqueness of the left-hand side within
the scope of that domain.

Semantically, the angle bracket characters are not part of the
msg-id; the msg-id is what is contained between the two angle bracket
characters.
      ]}
  *)

  val addr_spec : mailbox Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-6.1}RFC822}.

      {[
addr-spec = local-part "@" domain ; global address
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.4.1}RFC2822}.

      {[
An addr-spec is a specific Internet identifier that contains a
locally interpreted string followed by the at-sign character ("@",
ASCII value 64) followed by an Internet domain.  The locally
interpreted string is either a quoted-string or a dot-atom.  If the
string can be represented as a dot-atom (that is, it contains no
characters other than atext characters or "." surrounded by atext
characters), then the dot-atom form SHOULD be used and the
quoted-string form SHOULD NOT be used. Comments and folding white
space SHOULD NOT be used around the "@" in the addr-spec.

addr-spec = local-part "@" domain
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.4.1}RFC5322}.

      {[
Note: A liberal syntax for the domain portion of addr-spec is
given here.  However, the domain portion contains addressing
information specified by and used in other protocols (e.g.,
[RFC1034], [RFC1035], [RFC1123], [RFC5321]).  It is therefore
incumbent upon implementations to conform to the syntax of
addresses for the context in which they are used.

addr-spec = local-part "@" domain
      ]}
  *)

  val angle_addr : mailbox Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-6.1}RFC822}.

      The ABNF of [angle-addr] is not explicit from RFC 822 but the relic could be find here,
      as a part of mailbox:

      {[
mailbox =  addr-spec         ; simple address
        /  phrase route-addr ; name & addr-spec
      ]}

      From {{:https://tools.ietf.org/html/rfc2882#section-3.4}RFC2822 § 3.4} & {{:https://tools.ietf.org/html/rfc2822#section-4.4}RFC2822 § 4.4}.

      {[
obs-domain-list = "@" domain *( *(CFWS / "," ) [CFWS] "@" domain)
obs-route = [CFWS] obs-domain-list ":" [CFWS]
obs-angle-addr = [CFWS] "<" [obs-route] addr-spec ">" [CFWS]
angle-addr = [CFWS] "<" addr-spec ">" [CFWS] / obs-angle-addr
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section#section-3.4}RFC5322 § 3.4} & {{:https://tools.ietf.org/html/rfc5322#section-4.4}RFC5322 § 4.4}.

      {[
obs-domain-list = *(CFWS / ",") "@" domain
                  *("," [CFWS] ["@" domain])
obs-route = obs-domain-list ":"
obs-angle-addr = [CFWS] "<" obs-route addr-spec ">" [CFWS]
angle-addr = [CFWS] "<" addr-spec ">" [CFWS] /
      ]}
  *)

  val obs_domain_list : domain list Angstrom.t
  (** See {!angle_addr}. *)

  val obs_route : domain list Angstrom.t
  (** See {!angle_addr}. *)

  val obs_angle_addr : mailbox Angstrom.t
  (** See {!angle_addr}. *)

  val phrase : phrase Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-3.3}RFC822}.

      {[
phrase = 1*word ; Sequence of words
      ]}

      From {{:https://tools.ietf.org/html/rfc2047#section-2}RFC2047 § 2} & {{:https://tools.ietf.org/html/rfc2047#section-5}RFC2047 § 5}.

      {[
(3) As a replacement for a 'word' entity within a 'phrase', for example,
    one that precedes an address in a From, To, or Cc header.  The ABNF
    definition for 'phrase' from RFC 822 thus becomes:

    phrase = 1*( encoded-word / word )

    In this case the set of characters that may be used in a "Q"-encoded
    'encoded-word' is restricted to: <upper and lower case ASCII
    letters, decimal digits, "!", "*", "+", "-", "/", "=", and "_"
    (underscore, ASCII 95.)>.  An 'encoded-word' that appears within a
    'phrase' MUST be separated from any adjacent 'word', 'text' or
    'special' by 'linear-white-space'.

encoded-word = "=?" charset "?" encoding "?" encoded-text "?="
charset = token    ; see section 3
encoding = token   ; see section 4
token = 1*<Any CHAR except SPACE, CTLs, and especials>
especials = "(" / ")" / "<" / ">" / "@" / "," / ";" / ":" / "
            <"> / "/" / "[" / "]" / "?" / "." / "="
encoded-text = 1*<Any printable ASCII character other than "?"
                  or SPACE>
               ; (but see "Use of encoded-words in message
               ; headers", section 5)
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.2.6}RFC2822 § 3.2.6} & {{:https://tools.ietf.org/html/rfc2822#section-4.1}RFC2822 § 4.1}.

      {[
obs-phrase = word *(word / "." / CFWS)
phrase = 1*word / obs-phrase

Update:
+ Period allowed in obsolete form of phrase.
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.2.5}RFC5322 § 3.2.5} & {{:https://tools.ietf.org/html/rfc5322#section-4.1}RFC5322 § 4.1}.

      {[
phrase = 1*word / obs-phrase

Note: The "period" (or "full stop") character (".") in obs-phrase
is not a form that was allowed in earlier versions of this or any
other specification.  Period (nor any other character from
specials) was not allowed in phrase because it introduced a
parsing difficulty distinguishing between phrases and portions of
an addr-spec (see section 4.4).  It appears here because the
period character is currently used in many messages in the
display-name portion of addresses, especially for initials in
names, and therefore must be interpreted properly.

obs-phrase = word *(word / "." / CFWS)
      ]}
  *)

  val obs_phrase : phrase Angstrom.t
  (** See {!phrase}. *)

  val display_name : phrase Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-6.1}RFC822}.

      {[
mailbox =  addr-spec         ; simple address
        /  phrase route-addr ; name & addr-spec
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.4}RFC2822}.

      {[
display-name = phrase
name-addr = [display-name] angle-addr

Note: Some legacy implementations used the simple form where the
addr-spec appears without the angle brackets, but included the name
of the recipient in parentheses as a comment following the addr-spec.
Since the meaning of the information in a comment is unspecified,
implementations SHOULD use the full name-addr form of the mailbox,
instead of the legacy form, to specify the display name associated
with a mailbox.  Also, because some legacy implementations interpret
the comment, comments generally SHOULD NOT be used in address fields
to avoid confusing such implementations.
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.4}RFC5322}.

      {[
name-addr = [display-name] angle-addr
display-name = phrase
      ]}
  *)

  val mailbox : mailbox Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc822#section-6.1}RFC822}.

      {[
mailbox =  addr-spec         ; simple address
        /  phrase route-addr ; name & addr-spec
      ]}

      From {{:https://tools.ietf.org/html/rfc2822#section-3.4}RFC2822}.

      {[
mailbox = name-addr / addr-spec
      ]}

      From {{:https://tools.ietf.org/html/rfc5322#section-3.4}RFC5322}.

      {[
mailbox = name-addr / addr-spec
      ]}
  *)

  val mailbox_list : mailbox list Angstrom.t
  (** From {{:https://tools.ietf.org/html/rfc5322#section-3.4}RFC5322}.
      
      {[
obs-mbox-list = *([CFWS] ",") mailbox *("," [mailbox / CFWS])
mailbox-list = (mailbox *("," mailbox)) / obs-mbox-list
      ]}
  *)

  (** / *)

  val group : group Angstrom.t
  val address : t Angstrom.t
  val address_list : t list Angstrom.t

  (** / *)
end

type error = [ `Invalid of string * string ]
(** The error type. *)

val pp_error : error fmt
(** [pp_error ppf err] is pretty-printer of {!error}. *)

module List : sig
  val of_string_with_crlf : string -> (t list, [> error ]) result
  (** [of_string_with_crlf s] parses [s] which can be a list of named {i group}
     or a single {!mailbox} separated by a comma. In the case of a group, [s]
     starts with a name and contains a list of email separated by a comma and
     terminates with a semicolon:

      {[Gallium: Gabriel <gabriel@gallium.fr>, Armael <armael@gallium.fr>;]}

      [s] must terminate with [CRLF] as the delimiter. If the parser fails, it return an error
     {!error}. *)

  val of_string : string -> (t list, [> error ]) result
  (** [of_string s] is {!of_string_with_crlf} but did not need [CRLF] at the
      end. It's possible that [of_string] did not consume all [s]. *)

  val of_string_raw : off:int -> len:int -> ?tmp:Bigstringaf.t -> string -> (int * t list, [> error ]) result
  (** [of_string_raw s off len] is {!of_string_with_crlf} but did not need
     [CRLF]. It parses only a sub-part of [s] starting at [off] and computes at
     most [len] bytes. It returns how many bytes it consumed.

      If the user has an already allocated {!Bigstringaf.t}, it can use it as an
     internal buffer to parse given input [s]. *)

  val to_string : t list -> string
  (** [to_string lst] returns a well-formed string which represents
     the given list of {!t}. *)
end

val address_of_string_with_crlf : string -> (address, [> error ]) result
(** [address_of_string_with_crlf s] parses [s] which have the form:
   [local@domain]. Named email or multiple-domain email are not handle by this
   parser. [s] must terminate with [CRLF] as the delimiter. If the parser fails, it return an
   error {!error}. *)

val address_of_string : string -> (address, [> error ]) result
(** [address_of_string s] parses [s] which have the form: [local@domain]. Named
   email or multiple-domain email are not handle by this function. If the parser
   fails, it return an error {!error}. It's possible that [address_of_string] did not consume all [s]. *)

val address_of_string_raw : off:int -> len:int -> ?tmp:Bigstringaf.t -> string -> (int * address, [> error ]) result
(** [address_of_string_raw s off len] parses a sub-part of [s] starting at [off]
   and it computes at most [len] bytes. It returns the email and how many bytes
   it consumes. Named email or multiple-domain are not handle by this parser. If
   the parser fails, it return an error {!error}.

    If the user has an already allocated {!Bigstringaf.t}, it can use it as an
   internal buffer to parse given input [s]. *)

val set_of_string_with_crlf : string -> (t, [> error ]) result
val set_of_string : string -> (t, [> error ]) result
val set_of_string_raw : off:int -> len:int -> ?tmp:Bigstringaf.t -> string -> (int * t, [> error ]) result

val of_string_with_crlf : string -> (mailbox, [> error ]) result
(** [of_string_with_crlf s] parses [s] which can have multiple form:

    {ul
    {- Named email [Bobby <bobby@mail.net>]}
    {- Multiple-domain email [<@laposte.net:bobby@mail.net]}
    {- Usual form [bobby@mail.net]}
    {- Surrounded form [<bobby@mail.net>]}}

    About named email, the parser handles {i encoded-word} (according RFC 2047)
   to be able to use a special {i charset} (like UTF-8) to show the name. Parser
   decodes {i encoded-word} as is and do not do any translation from {i charset}
   specified to any encoding (eg. translation from {i latin1} to {i UTF-8}).

    [s] must terminates with [CRLF] as the delimiter. If the parser fails, it return an error
   {!error}. *)

val of_string : string -> (mailbox, [> error ]) result
(** [of_string s] is {!of_string_with_crlf} but did not need
   [CRLF] at the end. It's possible that [of_string] did not consume all [s]. *)

val of_string_raw : off:int -> len:int -> ?tmp:Bigstringaf.t -> string -> (int * mailbox, [> error ]) result
(** [of_string_raw s off len] is {!of_string_with_crlf} but did not need [CRLF]
   at the end. It parses only a sub-part of [s] starting at [off] and computes
   at most [len] bytes. It returns how many bytes it consumed.

    If the user has an already allocated {!Bigstringaf.t}, it can use it as an
   internal buffer to parse given input [s]. *)

val to_string : mailbox -> string
(** [to_string mailbox] returns a well-formed string which represents
   the given {!mailbox} value. *)

val set_to_string : t -> string
(** [set_to_string t] returns a well-formed string which represents
   the given {!t} value. *)

val address_to_string : address -> string
(** [address_to_string address] returns a well-formed string which represents
   the given {!address} value. *)
