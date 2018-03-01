(** Emile module, parser of e-mail address. *)

(** An e-mail address can contain as a part of a {!phrase} (identifier) an
   encoded string. Standards describe 2 kinds of encoding:

   {ul

   {- Quoted Printable: used to insert hexadecimal value with the [=] operator.}

   {- Base 64: string encoded in MIME's Base64}}

   Parser already decodes encoded {!raw}, the client can use it as is. *)
type raw =
  | Quoted_printable of string
  | Base64 of [ `Dirty of string | `Clean of string | `Wrong_padding ]

(** The local part of an e-mail address is composed by two kinds of {i word}s:

   {ul

   {- [`Atom] is string as is.}

   {- [`String] is a string surrounded by double-quote to allow white-space.}}

   The second kind is sanitize - we deleted double-quote. *)
type word =
  [ `Atom of string
  | `String of string ]

type local = word list
(** Local part of e-mail address. *)

(** Subset of domain described by RFC5321 which contains 3 kinds of address:

   {ul

   {- [IPv4]: a valid IPv4 address}

   {- [IPv6]: a valid IPv6 address}

   {- [Ext (ldh, value)]: an extended kind of domain recognized by [ldh]
   identifier which valus is [value]}}

   Parser of [IPv4] and [IPv6] was done by [Ipaddr]. An extended kind [Ext] need
   to be resolved by the client. *)
type addr =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Ext of (string * string)

(** Domain part of e-mail address. A domain integrate kinds from RFC5321 (see
   {!addr}), a domain described by RFC5322 and a [`Literal] which is the last {i
   best-effort} value possible as a domain.

   [Emile] {b does not} resolve domain. *)
type domain =
  [ `Domain of string list
  | `Addr of addr
  | `Literal of string ]

(** A phrase is a sentence to associate a name with an e-mail address or a group
   of e-mail addresses. [`Encoded] value {b is not} normalized on the {i
   charset} specified. The encoded's string is decoded as is only. *)
type phrase =
  [ `Dot | `Word of word | `Encoded of (string * raw) ] list

(** A mailbox is an e-mail address. It contains an optional name (see
   {!phrase}), a local-part {see {!local}} and one or more {!domain}(s). *)
type mailbox =
  { name : phrase option
  ; local : local
  ; domain : domain * domain list }

(** A groyp is a named set of {!mailbox}. *)
type group =
  { group     : phrase
  ; mailboxes : mailbox list }

(** A basic e-mail address. *)
type address = local * (domain * domain list)

(** The {i Emile}'s set type which is a {i singleton} or a {i set} of e-mail
   addresses. *)
type set = [ `Mailbox of mailbox | `Group of group ]

(** {2 Pretty-printer} *)

val pp_addr: addr Fmt.t
val pp_domain: domain Fmt.t
val pp_word: word Fmt.t
val pp_local: local Fmt.t
val pp_raw: raw Fmt.t
val pp_phrase: phrase Fmt.t
val pp_mailbox: mailbox Fmt.t
val pp_group: group Fmt.t
val pp_address: address Fmt.t
val pp_set: set Fmt.t

(** {2 Equal & Compare} *)

type 'a equal = 'a -> 'a -> bool
type 'a compare = 'a -> 'a -> int

val case_sensitive: string -> string -> int
(** Alias of {!String.compare}. *)

val case_insensitive: string -> string -> int
(** [case_insensitive a b] maps values with {!lowercase_ascii} and compare them
   with {!String.compare}. We {b do not} map UTF8 value. *)

val equal_word: compare:(string -> string -> int) -> word equal
(** [equal ~compare a b] tests if {!word} [a] and {!word} [b] are semantically
   equal. [compare] specifies implementation to compare two [string] (i.e. to be
   case-sensitive or not). *)

val compare_word: ?case_sensitive:bool -> word compare
(** [compare_word ?case_sensitive a b] compares {!word} [a] and {!word} [b]
   semantically. From standards, {!word} SHOULD be case-sensitive, the client
   can notice this behaviour by [?case_sensitive] (default is [true]). *)

val equal_raw: compare:string compare -> raw equal
(** [equal_raw a b] tests if {!raw} [a] and {!raw} [b] are semantically equal.
   {i Semantically equal} means we compare raw's content, by this way, a
   [Base64] raw could be equal to a [Quoted_printable] raw if and only if
   [string] are equal. *)

val compare_raw: compare:string compare -> raw compare
(** [compare_raw a b] compares {!raw} [a] and {!raw} [b] semantically. *)

val equal_phrase: phrase equal
(** [equal_phrase a b] tests if {!phrase} [a] and {!phrase} [b] are semantically
   equal. In this case, the comparison is case-insensitive between elements in
   {!phrase}. The order of elements is important. *)

val compare_phrase: phrase compare
(** [compare_phrase a b] compares {!phrase} [a] and {!phrase} [b] semantically.
   *)

val equal_addr: addr equal
(** [equal_addr a b] tests if {!addr} [a] and {!addr} [b] are semantically
   equal. An [IPv4] should be equal with an [IPv6] address. Then, for extended
   kind, we strictly compare ({!Pervasives.compare}) kind and value. *)

val compare_addr: addr compare
(** [compare_addr a b] compares {!addr} [a] and {!addr} [b], we prioritize
   [IPv6], [IPv4] and finally [Ext]. *)

val equal_domain: domain equal
(** [equal_addr a b] tests if {!domain} [a] and {!domain} [b] are semantically
   equal. We {b do not} resolve domain, a [`Domain] could be semantically equal
   to another [`Domain] if they point to the same [IPv4]/[IPv6]. *)

val compare_domain: domain compare
(** [comapre_domain a b] compares {!domain} [a] and {!domain} [b], we prioritize
   [`Domain], [`Literal] and finally [`Addr]. The comparison between two
   [`Literal] and between part of [`Domain] are case-insensitive. *)

val equal_domains: (domain * domain list) equal
(** [equal_domains a b] apply {!equal_domain} to ordered domains (see
   {!compare_domain}) between [a] and [b]. *)

val compare_domains: (domain * domain list) compare
(** [compare_domains a b] compares ordered list of {!domain} [a] and ordered
   list of {!domain} [b]. *)

val equal_local: ?case_sensitive:bool -> local equal
(** [equal_local ?case_sensitive a b] tests if {!local} [a] and {!local} [b] are
   semantically equal. Standards notices local-part SHOULD be case-sensitive,
   the client can choose this behaviour with [case_sensitive]. *)

val compare_local: ?case_sensitive:bool -> local compare
(** [compare_local ?case_sensitive a b] compares {!local} [a] and {!local} [b]
   semantically. The user can decide if the comparison is case-sensitive or not
   (with [case_sensitive]). *)

val equal_mailbox: ?case_sensitive:bool -> mailbox equal
(** [equal_mailbox ?case_sensitive a b] tests if {!mailbox} [a] and {!mailbox}
   [b] are semantically equal. The user can define if the local-part need to be
   case-sensitive or not (by [case_sensitive]). If [a] xor [b] has a name, we
   consider [a = b] if we have the same local-part and same domain(s).
   Otherwise, we compare identifier/{!phrase} between them. *)

val compare_mailbox: ?case_sensitive:bool -> mailbox compare
(** [compare ?case_sensitive a b] compares {!mailbox} [a] and {!mailbxo} [b]
   semantically. We prioritize local-part, domain-part and finally optionnal
   name. *)

val compare_group: group compare
(** [comapre_group a b] compares {!group} [a] and {!group} [b]. We compare the
   group name first and compare ordered {!mailbox}es list then. *)

val equal_group: group equal
(** [equal_group a b] tests if {!group} [a] and {!group} [b] are semantically
   equal. We compare first group name and ordered {!mailbox}es list then. *)

val compare_address: address compare

val equal_address: address equal

val equal_set: set equal
(** [equal a b] tests semantically {!t} [a] and {!t} [b]. *)

val compare_set: set compare
(** [compare a b] compares {!t} [a] and {!t} [b]. *)

val strictly_equal_set: set equal
(** A structurally equal function on {!t}. *)

(** {2 Decoders} *)

type error =
  [ `Invalid of (string * string list)
  | `Incomplete ]

val pp_error: error Fmt.t
(** [pp_error ppf err] prints an {!error}. *)

module List:
sig
  val of_string_with_crlf: string -> (set list, error) result
  val of_string: string -> (set list, error) result
  val of_string_raw: string -> int -> int -> (set list * int, error) result
end

val address_of_string_with_crlf: string -> (address, error) result
val address_of_string: string -> (address, error) result
val address_of_string_raw: string -> int -> int -> (address * int, error) result

val set_of_string_with_crlf: string -> (set, error) result
val set_of_string: string -> (set, error) result
val set_of_string_raw: string -> int -> int -> (set * int, error) result

val of_string_with_crlf: string -> (mailbox, error) result
val of_string: string -> (mailbox, error) result
val of_string_raw: string -> int -> int -> (mailbox * int, error) result
