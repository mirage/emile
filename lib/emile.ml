type mailbox =
  { name : phrase option
  ; local : local
  ; domain : domain * domain list }

and domain =
  [ `Domain of string list
  | `Addr of addr
  | `Literal of string ]

and addr =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Ext of (string * string)

and phrase =
  [ `Dot
  | `Word of word
  | `Encoded of string * raw ] list

and raw =
  | Quoted_printable of (string, [`Msg of string]) result
  | Base64 of (string, [`Msg of string]) result

and word = [ `Atom of string | `String of string]
and local = word list
and group =
  { group : phrase
  ; mailboxes : mailbox list }
and address = local * (domain * domain list)

and t = [ `Mailbox of mailbox | `Group of group ]

(* Pretty-printers *)

module Fmt = struct
  let pf ppf fmt = Format.fprintf ppf fmt
  let string = Format.pp_print_string
  let char = Format.pp_print_char
  let const pp v ppf () = pp ppf v
  let always fmt ppf () = pf ppf fmt
  let quote pp_val ppf v = pf ppf "@[<1>@<1>\"%a@<1>\"@]" pp_val v
  let list ~sep:pp_sep pp_val ppf lst =
    let rec go = function
      | [] -> ()
      | [ x ] -> pp_val ppf x
      | x :: r -> pf ppf "%a%a" pp_val x pp_sep () ; go r in
    go lst
end

(* Encoder *)

let str_word ppf = function
  | `Atom v -> Fmt.string ppf v
  | `String v ->
    let escape = function
      | '\\' -> Fmt.string ppf "\\\\"
      | '"' -> Fmt.string ppf "\\\""
      | '\000' -> Fmt.string ppf "\\\000"
      | '\x07' -> Fmt.string ppf "\\a"
      | '\b' -> Fmt.string ppf "\\b"
      | '\t' -> Fmt.string ppf "\\t"
      | '\n' -> Fmt.string ppf "\\n"
      | '\x0b' -> Fmt.string ppf "\\v"
      | '\x0c' -> Fmt.string ppf "\\f"
      | '\r' -> Fmt.string ppf "\\r"
      | chr -> Fmt.char ppf chr in
    Fmt.string ppf "\"" ;
    String.iter escape v ;
    Fmt.string ppf "\""

let str_local ppf local =
  let rec go = function
    | [] -> ()
    | [ x ] -> str_word ppf x
    | x :: r -> Fmt.pf ppf "%a." str_word x ; go r in
  go local

let str_domain ppf = function
  | `Domain lst ->
    let rec go = function
      | [] -> () | [ x ] -> Fmt.string ppf x
      | x :: r -> Fmt.pf ppf "%s." x ; go r in
    go lst
  | `Addr (IPv4 v4) ->
    Fmt.pf ppf "[%s]" (Ipaddr.V4.to_string v4)
  | `Addr (IPv6 v6) ->
    Fmt.pf ppf "[IPv6:%s]" (Ipaddr.V6.to_string v6)
  | `Addr (Ext (key, value)) ->
    Fmt.pf ppf "[%s:%s]" key value
  | `Literal v ->
    Fmt.pf ppf "[%s]" v

[@@@warning "-8"]

let str_raw ~charset ppf = function
  | Quoted_printable (Ok v) ->
    let buf = Buffer.create 16 in
    let encoder = Pecu.Inline.encoder (`Buffer buf) in
    let rec go idx =
      (* XXX(dinosaure): safe due to [`Buffer]. *)
      let[@warning "-8"] `Ok : [ `Ok | `Partial ] =
        if idx = String.length v
        then Pecu.Inline.encode encoder `End
        else Pecu.Inline.encode encoder (`Char v.[idx]) in
      if idx < String.length v then go (succ idx) in
    go 0 ; Fmt.pf ppf "=?%s?Q?%s?="
      (String.lowercase_ascii charset)
      (Buffer.contents buf)
  | Base64 (Ok v) ->
    Fmt.pf ppf "=?%s?B?%s?="
      (String.lowercase_ascii charset)
      (Base64.encode_exn ~pad:true v)
  | _ -> assert false

[@@@warning "+8"]

let str_phrase ppf phrase =
  let str_elt ppf = function
    | `Dot -> Fmt.char ppf '.'
    | `Word w -> str_word ppf w
    | `Encoded (charset, v) -> str_raw ~charset ppf v in
  let rec go = function
    | [] -> ()
    | [ x ] -> str_elt ppf x
    | x :: r -> Fmt.pf ppf "%a " str_elt x ; go r in
  go phrase

let str_mailbox ppf = function
  | { name= None; local; domain= (domain, []) } ->
    Fmt.pf ppf "%a@%a" str_local local str_domain domain
  | { name= Some name; local; domain= (domain, []) } ->
    Fmt.pf ppf "%a <%a@%a>" str_phrase name str_local local str_domain domain
  | { name; local; domain= (x, r) } ->
    let () = match name with
      | Some name -> Fmt.pf ppf "%a <" str_phrase name
      | None -> Fmt.string ppf "<" in
    let rec go = function
      | [] -> ()
      | [ e ] -> Fmt.pf ppf "@%a" str_domain e
      | h :: t -> Fmt.pf ppf "@%a," str_domain h ; go t in
    go r ; Fmt.pf ppf ":%a@%a>" str_local local str_domain x

let str_address ppf (local, domain) =
  str_mailbox ppf { name= None; local; domain; }

let str_group ppf { group; mailboxes; } =
  Fmt.pf ppf "%a: " str_phrase group ;
  let rec go = function
    | [] -> ()
    | [ x ] -> str_mailbox ppf x
    | x :: r ->
      Fmt.pf ppf "%a, " str_mailbox x ; go r in
  go mailboxes ; Fmt.string ppf ";"

let str_addresses ppf lst =
  let rec go = function
    | [] -> ()
    | [ `Mailbox x ] -> str_mailbox ppf x
    | [ `Group x ] -> str_group ppf x
    | `Mailbox x :: r ->
      Fmt.pf ppf "%a, " str_mailbox x ; go r
    | `Group x :: r ->
      Fmt.pf ppf "%a, " str_group x ; go r in
  go lst

type 'a fmt = Format.formatter -> 'a -> unit

let pp_addr ppf = function
  | IPv4 ipv4 -> Fmt.pf ppf "[%s]" (Ipaddr.V4.to_string ipv4)
  | IPv6 ipv6 -> Fmt.pf ppf "[IPv6:%s]" (Ipaddr.V6.to_string ipv6)
  | Ext (key, value) -> Fmt.pf ppf "[%s:%s]" key value

let pp_domain ppf = function
  | `Domain lst -> Fmt.list ~sep:(Fmt.const Fmt.string ".") Fmt.string ppf lst
  | `Addr addr -> pp_addr ppf addr
  | `Literal lit -> Fmt.pf ppf "[%s]" lit

let pp_word ppf = function
  | `Atom atom -> Fmt.string ppf atom
  | `String str -> Fmt.quote Fmt.string ppf str

let pp_local ppf lst = Fmt.list ~sep:(Fmt.const Fmt.char '.') pp_word ppf lst

let pp_raw ppf = function
  | Quoted_printable (Ok s) -> Fmt.pf ppf "quoted-printable:%s" s
  | Base64 (Ok s) -> Fmt.pf ppf "base64:%s" s
  | Quoted_printable (Error (`Msg _)) | Base64 (Error (`Msg _)) ->
    Fmt.string ppf "#error"

let pp_phrase ppf phrase =
  let pp_elem ppf = function
    | `Dot -> Fmt.string ppf "."
    | `Word x -> Fmt.pf ppf "%a" pp_word x
    | `Encoded (_, raw) -> Fmt.pf ppf "<@[<hov>%a@]>" pp_raw raw in
  Fmt.list ~sep:(Fmt.always "@ ") pp_elem ppf phrase

let pp_mailbox ppf = function
  | { name= None; local; domain= domain, [] } ->
    Fmt.pf ppf "@[<0>%a@%a@]" pp_local local pp_domain domain
  | { name= None; local; domain= first, rest } ->
    let pp ppf domain = Fmt.pf ppf "@%a" pp_domain domain in
    Fmt.pf ppf "@[<1><%a:%a@%a>@]"
      (Fmt.list ~sep:(Fmt.const Fmt.char ',') pp)
      rest pp_local local pp_domain first
  | { name= Some name; local; domain } ->
    let pp_addr ppf (local, domains) =
      match domains with
      | domain, [] ->
        Fmt.pf ppf "@[<1><%a@%a>@]" pp_local local pp_domain domain
      | domain, rest ->
        let pp ppf domain = Fmt.pf ppf "@%a" pp_domain domain in
        Fmt.pf ppf "@[<1><%a:%a@%a>@]"
          (Fmt.list ~sep:(Fmt.const Fmt.string ",") pp)
          rest pp_local local pp_domain domain in
    Fmt.pf ppf "@[<hov>%a@]@ %a" pp_phrase
      name pp_addr (local, domain)

let pp_group ppf { group; mailboxes } =
  Fmt.pf ppf "@[<hov>%a@]:@ @[<hov>%a@]" pp_phrase group
    Fmt.(list ~sep:(always ",@ ") pp_mailbox) mailboxes

let pp_address ppf (local, domain) = pp_mailbox ppf { name= None; local; domain }

let pp ppf = function
  | `Mailbox mailbox -> pp_mailbox ppf mailbox
  | `Group group -> pp_group ppf group

(* Equal *)

(* XXX(dinosaure): CFWS (useless white space and comment) are already deleted by
   parser below.

   However, some parts are semantically equal, like:
   - raw with base64 encoding or quoted-printable encoding: if produced content
   is equal, these values are semantically equal
   - order of domains (RFC did not explain any specific process about order of
   domains)
   - RFC 1034 explains domains are case-insensitive
   - IPv4 could be equal to a subset of IPv6
   - [`Atom] and [`String] could be semantically equal (it's a /word/) (RFC 5321
   explains local-part - which contains /word/ - SHOULD be case-sensitive)

   So, for all of these, we implement two kinds of [equal]:
   - a strict implementation which strictly checks if two addresses are equal
   structurally
   - a semantic implementation which follows rules above *)

type 'a equal = 'a -> 'a -> bool
type 'a compare = 'a -> 'a -> int

let case_sensitive a b = String.compare a b
let case_insensitive a b = String.(compare (lowercase_ascii a) (lowercase_ascii b))

let equal_word ~compare a b =
  match a, b with
  | `Atom a, `Atom b
  | `String a, `Atom b
  | `Atom a, `String b
  | `String a, `String b -> compare a b = 0

let compare_word ~compare a b =
  match a, b with
  | `Atom a, `Atom b
  | `String a, `Atom b
  | `Atom a, `String b
  | `String a, `String b -> compare a b

let equal_raw ~compare a b =
  match a, b with
  | Quoted_printable (Ok a), Quoted_printable (Ok b)
  | Base64 (Ok a), Base64 (Ok b)
  | Base64 (Ok a), Quoted_printable (Ok b)
  | Quoted_printable (Ok a), Base64 (Ok b) -> compare a b = 0
  | _, _ -> false (* XXX(dinosaure): both error return [false]. *)

let inf = (-1) and sup = 1

let compare_raw ~compare a b =
  match a, b with
  | Quoted_printable (Ok a), Quoted_printable (Ok b)
  | Base64 (Ok a), Base64 (Ok b)
  | Base64 (Ok a), Quoted_printable (Ok b)
  | Quoted_printable (Ok a), Base64 (Ok b) -> compare a b
  | ( Quoted_printable (Error _) | Base64 (Error _) ),
    ( Quoted_printable (Ok _) | Base64 (Ok _) ) -> sup
  | ( Quoted_printable (Ok _) | Base64 (Ok _ ) ),
    ( Quoted_printable (Error _) | Base64 (Error _) ) -> inf
  | _, _ -> 0 (* XXX(dinosaure): both error are equal. *)

let compare_raw_with_string ~compare a b =
  match a with
  | Quoted_printable (Ok a) -> compare a b
  | Base64 (Ok a) -> compare a b
  | _ -> sup

let compare_string_with_raw ~compare a b =
  match b with
  | Quoted_printable (Ok b) -> compare a b
  | Base64 (Ok b) -> compare a b
  | _ -> inf

let equal_phrase a b =
  if List.length a <> List.length b then false
  else
    let compare a b = case_insensitive a b in
    List.for_all2
      (fun a b ->
        match a, b with
        | `Encoded (_, a), `Encoded (_, b) -> equal_raw ~compare a b
        | `Dot, `Dot -> true
        | `Word a, `Word b -> equal_word ~compare a b
        | `Encoded (_, a), `Word (`Atom b | `String b) ->
          compare_raw_with_string ~compare a b = 0
        | `Word (`Atom a | `String a), `Encoded (_, b) ->
          compare_string_with_raw ~compare a b = 0
        | _, _ -> false)
      a b

let compare_phrase a b =
  let compare = case_insensitive in
  let rec go a b =
    match a, b with
    | [], [] -> 0
    | _ :: _, [] -> sup
    | [], _ :: _ -> inf
    | a :: ar, b :: br ->
      match a, b with
      | `Word a, `Word b ->
        let res = compare_word ~compare a b in
        if res = 0 then go ar br else res
      | `Encoded (_, a), `Encoded (_, b) ->
        let res = compare_raw ~compare a b in
        if res = 0 then go ar br else res
      | `Dot, `Dot -> go ar br
      | `Encoded (_, a), `Word (`Atom b | `String b) ->
        let res = compare_raw_with_string ~compare a b in
        if res = 0 then go ar br else res
      | `Word (`Atom a | `String a), `Encoded (_, b) ->
        let res = compare_string_with_raw ~compare a b in
        if res = 0 then go ar br else res
      | `Dot, _ -> sup
      | `Word _, `Dot -> inf | `Word _, _ -> sup
      | `Encoded _, `Dot -> inf | `Encoded _, _ -> sup in
  go a b

let equal_addr a b =
  match a, b with
  | IPv4 ipv4, IPv6 ipv6 | IPv6 ipv6, IPv4 ipv4 ->
    Ipaddr.(compare (V4 ipv4) (V6 ipv6)) = 0
  | IPv6 a, IPv6 b ->
    Ipaddr.V6.compare a b = 0
  | IPv4 a, IPv4 b ->
    Ipaddr.V4.compare a b = 0
  | Ext (ldh_a, content_a), Ext (ldh_b, content_b) ->
    String.equal ldh_a ldh_b && String.equal content_a content_b
  (* XXX(dinosaure): RFC 5321 does not explain if Ldh token is case-insensitive. *)
  | _, _ -> false

let compare_addr a b =
  match a, b with
  | IPv4 ipv4, IPv6 ipv6 | IPv6 ipv6, IPv4 ipv4 ->
    Ipaddr.(compare (V4 ipv4) (V6 ipv6))
  | IPv6 a, IPv6 b ->
    Ipaddr.V6.compare a b
  | IPv4 a, IPv4 b ->
    Ipaddr.V4.compare a b
  | Ext (ldh_a, content_a), Ext (ldh_b, content_b) ->
    let ret = String.compare ldh_a ldh_b in
    if ret = 0 then String.compare content_a content_b else ret
  (* XXX(dinosaure): lexicographic compare. *)
  | IPv6 _, _ -> sup
  | IPv4 _, _ -> sup
  | Ext _, (IPv4 _ | IPv6 _) -> inf

let compare_domain a b =
  match a, b with
  | `Domain a, `Domain b ->
    let rec go a b =
      match a, b with
      | [], [] -> 0
      | a :: ar, b :: br ->
        let res = case_insensitive a b in
        if res = 0 then go ar br else res
      | [], _ :: _ -> inf | _ :: _, [] -> sup in
    (* compare [] [0] = -1 && compare [0] [] = 1 *)
    go a b
  | `Literal a, `Literal b -> case_insensitive a b
  | `Addr a, `Addr b -> compare_addr a b
  | `Domain _, _ -> sup
  | `Literal _, `Domain _ -> inf | `Literal _, _ -> sup
  | `Addr _, (`Domain _ | `Literal _) -> inf | `Addr _, _ -> sup

let compare_word ?case_sensitive:(c = false) a b =
  match a, b with
  | `Atom a, `Atom b
  | `String a, `String b
  | `Atom a, `String b
  | `String a, `Atom b ->
    if not c then case_insensitive a b else case_sensitive a b

let compare_local ?case_sensitive a b =
  let rec go a b =
    match a, b with
    | _ :: _, [] -> sup
    | [], _ :: _ -> inf
    | a :: ar, b :: br ->
      let res = compare_word ?case_sensitive a b in
      if res = 0 then go ar br else res
    | [], [] -> 0 in
  go a b

let equal_domain a b =
  match a, b with
  | `Domain a, `Domain b ->
    if List.length a <> List.length b then false
    else List.for_all2 (fun a b -> case_insensitive a b = 0) a b
  | `Literal a, `Literal b ->
    case_insensitive a b = 0
  | `Addr a, `Addr b ->
    equal_addr a b
  | _, _ -> false

(* XXX(dinosaure) we should resolve domain and compare with IP
     address if they are equal or not. *)

let equal_domains a b =
  if List.length a <> List.length b then false
  else
    let a = List.sort compare_domain a in
    let b = List.sort compare_domain b in
    List.for_all2 (fun a b -> equal_domain a b) a b

let equal_domains (a, ar) (b, br) = equal_domains (a :: ar) (b :: br)

let compare_domains a b =
  let rec go a b =
    match a, b with
    | _ :: _, [] -> sup
    | [], _ :: _ -> inf
    | a :: ar, b :: br ->
      let res = compare_domain a b in
      if res = 0 then go ar br else res
    | [], [] -> 0 in
  go (List.sort compare_domain a) (List.sort compare_domain b)

let compare_domains (a, ar) (b, br) = compare_domains (a :: ar) (b :: br)

let equal_local ?case_sensitive:(case = false) a b =
  let compare a b =
    if not case
    then case_insensitive a b
    else case_sensitive a b in
  if List.length a <> List.length b then false
  else List.for_all2 (fun a b -> equal_word ~compare a b) a b

let equal_mailbox ?case_sensitive a b =
  let equal_name a b =
    match a, b with
    | Some _, None | None, Some _ | None, None -> true
    | Some a, Some b -> equal_phrase a b in
  equal_local ?case_sensitive a.local b.local
  && equal_domains a.domain b.domain
  && equal_name a.name b.name

let compare_mailbox ?case_sensitive a b =
  let res = compare_domains a.domain b.domain in
  if res = 0 then
    let res = compare_local ?case_sensitive a.local b.local in
    if res = 0 then
      match a.name, b.name with
      | Some _, None -> sup
      | None, Some _ -> inf
      | Some a, Some b -> compare_phrase a b
      | None, None -> 0
    else res
  else res

let compare_group a b =
  let rec go a b =
    match a, b with
    | [], [] -> 0
    | _ :: _, [] -> sup
    | [], _ :: _ -> inf
    | a :: ar, b :: br ->
      let res = compare_mailbox a b in
      if res = 0 then go ar br else res in
  let res = compare_phrase a.group b.group in
  if res = 0 then
    go
      (List.sort compare_mailbox a.mailboxes)
      (List.sort compare_mailbox b.mailboxes)
  else res

let equal_group a b =
  let rec go a b =
    match a, b with
    | [], [] -> true
    | _ :: _, [] | [], _ :: _ -> false
    | a :: ar, b :: br ->
      let res = equal_mailbox a b in
      if res then go ar br else res in
  equal_phrase a.group b.group
  && go
    (List.sort compare_mailbox a.mailboxes)
    (List.sort compare_mailbox b.mailboxes)

let compare_address a b =
  compare_mailbox
    {name= None; local= fst a; domain= snd a}
    {name= None; local= fst b; domain= snd b}

let equal_address a b =
  equal_mailbox
    {name= None; local= fst a; domain= snd a}
    {name= None; local= fst b; domain= snd b}

let equal_set a b =
  match a, b with
  | `Group _, `Mailbox _ | `Mailbox _, `Group _ -> false
  | `Group a, `Group b -> equal_group a b
  | `Mailbox a, `Mailbox b -> equal_mailbox a b

let compare_set a b =
  match a, b with
  | `Group _, `Mailbox _ -> sup
  | `Mailbox _, `Group _ -> inf
  | `Group a, `Group b -> compare_group a b
  | `Mailbox a, `Mailbox b -> compare_mailbox a b

module Parser = struct
  [@@@warning "-32"]

  open Angstrom

  (* XXX(dinosaure): about each comment, because we don't have a software to
     prove these implementations, we check all /tokens/ by hands. All
     occurrences of token in RFCs appears: ABNF and comments. It's useful to not
     forget something when we implement them. It's little annoying to do this
     but some crazy people have the power to decide about how to write an
     e-mail - not my fault.

     Then, a dependence /by hands/ again show you why we need to implement
     token.

     Finally, a little comment by the implementor to explain what we have and
     what we _don't_ have. *)

  (* From RFC 5234 (used in RFC 5322)

     VCHAR          =  %x21-7E
                            ; visible (printing) characters

     Dependence

       VCHAR         <- quoted-pair
       quoted-pair   <- ccontent & qcontent
       ccontent      <- comment
       qcontent      <- quoted-string
       quoted-string <- word
       word          <- local-part
       local-part    <- addr-spec
       addr-spec     <- mailbox
  *)
  let is_vchar = function '\x21' .. '\x7e' -> true | _ -> false

  (* From RFC 5322

       obs-NO-WS-CTL   =   %d1-8 /            ; US-ASCII control
                           %d11 /             ;  characters that do not
                           %d12 /             ;  include the carriage
                           %d14-31 /          ;  return, line feed, and
                           %d127              ;  white space characters

     Dependence

       obs-NO-WS-CTL <- obs-ctext
       obs-ctext     <- ctext
       ctext         <- ccontent
       ccontent      <- comment
       comment       <- CFWS
  *)
  let is_obs_no_ws_ctl = function
    | '\001' .. '\008' | '\011' | '\012' | '\014' .. '\031' | '\127' ->
        true
    | _ ->
        false

  (* From RFC 822

       ctext       =  <any CHAR excluding "(",     ; => may be folded
                       ")", BACKSLASH & CR, & including
                       linear-white-space>

     From RFC 1522

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

     From RFC 2047

       Update from RFC 1522:
       + clarification: an 'encoded-word' may appear immediately following
         the initial "(" or immediately before the final ")" that delimits a
         comment, not just adjacent to "(" and ")" *within* *ctext.

     From RFC 2822

       ctext           =       NO-WS-CTL /     ; Non white space controls

                               %d33-39 /       ; The rest of the US-ASCII
                               %d42-91 /       ;  characters not including "(",
                               %d93-126        ;  ")", or BACKSLASH

     From RFC 5322

       ctext           =   %d33-39 /          ; Printable US-ASCII
                           %d42-91 /          ;  characters not including
                           %d93-126 /         ;  "(", ")", or BACKSLASH
                           obs-ctext
       obs-ctext       =   obs-NO-WS-CTL

       Update from RFC 2822
       + Removed NO-WS-CTL from ctext

     From RFC 5335

       ctext   =/  UTF8-xtra-char
       UTF8-xtra-char  =   UTF8-2 / UTF8-3 / UTF8-4
       UTF8-2          =   %xC2-DF UTF8-tail

       UTF8-3          =   %xE0 %xA0-BF UTF8-tail /
                           %xE1-EC 2(UTF8-tail) /
                           %xED %x80-9F UTF8-tail /
                           %xEE-EF 2(UTF8-tail)

       UTF8-4          =   %xF0 %x90-BF 2( UTF8-tail ) /
                           %xF1-F3 3( UTF8-tail ) /
                           %xF4 %x80-8F 2( UTF8-tail )

       UTF8-tail       =   %x80-BF

     From RFC 6532

       ctext   =/  UTF8-non-ascii

     Dependence

       ctext    <- ccontent
       ccontent <- comment
       comment  <- CFWS

     XXX(dinosaure):
     - about UTF-8, the process is out of this scope where we check only one byte here
     - about compliance with RFC 1522, it's out of scope where we check only one byte here

     This code is a translation of RFC 5322's ABNF.
  *)
  let is_ctext = function
    | '\033' .. '\039' | '\042' .. '\091' | '\093' .. '\126' ->
        true
    | c ->
        is_obs_no_ws_ctl c

  (* From RFC 822

       qtext       =  <any CHAR excepting DQUOTE,     ; => may be folded
                       BACKSLASH & CR, and including
                       linear-white-space>

     From RFC 2822

       qtext           =       NO-WS-CTL /     ; Non white space controls

                               %d33 /          ; The rest of the US-ASCII
                               %d35-91 /       ;  characters not including BACKSLASH
                               %d93-126        ;  or the quote character


     From RFC 5322

       qtext           =   %d33 /             ; Printable US-ASCII
                           %d35-91 /          ;  characters not including
                           %d93-126 /         ;  BACKSLASH or the quote character
                           obs-qtext
       obs-qtext       =   obs-NO-WS-CTL

     From RFC 5335

       See [is_ctext] for UTF8-xtra-char.

       utf8-qtext    =     qtext / UTF8-xtra-char

     From RFC 6532

       qtext   =/  UTF8-non-ascii

     Dependence

       qtext         <- qcontent
       qcontent      <- quoted-string
       quoted-string <- word
       word          <- local-part
       local-part    <- addr-spec
       addr-spec     <- mailbox

     XXX(dinosaure):
     - about UTF-8, the process is out of this scope where we check only one byte here

     This code is a translation of RFC 5322's ABNF.
  *)
  let is_qtext = function
    | '\033' | '\035' .. '\091' | '\093' .. '\126' ->
        true
    | c ->
        is_obs_no_ws_ctl c

  (* From RFC 822

       The ABNF of atext is not explicit from RFC 822 but the relic could be find here:

       atom        =  1*<any CHAR except specials, SPACE and CTLs>

     From RFC 2822

       atext           =       ALPHA / DIGIT / ; Any character except controls,
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

     From RFC 5322

       atext           =   ALPHA / DIGIT /    ; Printable US-ASCII
                           "!" / "#" /        ;  characters not including
                           "$" / "%" /        ;  specials.  Used for atoms.
                           "&" / "'" /
                           "*" / "+" /
                           "-" / "/" /
                           "=" / "?" /
                           "^" / "_" /
                           "`" / "{" /
                           "|" / "}" /
                           "~"

     From 5335

       utf8-atext   =  ALPHA / DIGIT /
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
       UTF8-xtra-char: see is_ctext

       This means that all the [RFC2822] constructs that build upon these
       will permit UTF-8 characters, including comments and quoted strings.
       We do not change the syntax of <atext> in order to allow UTF8
       characters in <addr-spec>.  This would also allow UTF-8 characters in
       <message-id>, which is not allowed due to the limitation described in
       Section 4.5.  Instead, <utf8-atext> is added to meet this
       requirement.

     From RFC 6532

       atext   =/  UTF8-non-ascii

     Dependence

       atext      <- atom
       atom       <- word
       word       <- local-part
       local-part <- addr-spec
       addr-spec  <- mailbox

     XXX(dinosaure):
     - about UTF-8, the process is out of this scope where we check only one byte here

     This code is a translation of RFC 5322's ABNF.
  *)
  let is_atext = function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '*'
    | '+'
    | '-'
    | '/'
    | '='
    | '?'
    | '^'
    | '_'
    | '`'
    | '{'
    | '}'
    | '|'
    | '~' ->
        true
    | _ ->
        false

  let is_cr = ( = ) '\r'
  let is_lf = ( = ) '\n'
  let is_d0 = ( = ) '\000'

  (* From RFC 822

       LWSP-char   =  SPACE / HTAB                 ; semantics = SPACE

     From RFC 2822 and RFC 5322, we did not find any occurrence of LWSP-char,
     it replaced by WSP. However, these RFCs does not provide an ABNF to describe
     WSP (described by RFC5234).
  *)
  let is_wsp = function '\x09' | '\x20' -> true | _ -> false

  (* From RFC 822

       quoted-pair =  BACKSLASH CHAR                     ; may quote any char

       CHAR is case-sensitive

     From RFC 2822

       quoted-pair     =       (BACKSLASH text) / obs-qp
       text            =       %d1-9 /         ; Characters excluding CR and LF
                               %d11 /
                               %d12 /
                               %d14-127 /
                               obs-text
       obs-text        =       *LF *CR *(obs-char *LF *CR)
       obs-char        =       %d0-9 / %d11 /          ; %d0-127 except CR and
                               %d12 / %d14-127         ;  LF
       obs-qp          =       BACKSLASH (%d0-127)

     From RFC 5322

       quoted-pair     =   (BACKSLASH (VCHAR / WSP)) / obs-qp
       obs-qp          =   BACKSLASH (%d0 / obs-NO-WS-CTL / LF / CR)

     From RFC 5335

       See [is_ctext] for UTF8-xtra-char.

       utf8-text   =  %d1-9 /         ; all UTF-8 characters except
                      %d11-12 /       ; US-ASCII NUL, CR, and LF
                      %d14-127 /
                      UTF8-xtra-char
       utf8-quoted-pair   = (BACKSLASH utf8-text) / obs-qp

     Dependence

       quoted-pair   <- ccontent & qcontent
       ccontent      <- comment
       qcontent      <- quoted-string
       quoted-string <- word
       word          <- local-part
       local-part    <- addr-spec
       addr-spec     <- mailbox

     XXX(dinosaure): we can factorize this code by [fun _ -> true]
  *)
  let is_quoted_pair chr =
    is_vchar chr
    || is_wsp chr
    || is_d0 chr
    || is_obs_no_ws_ctl chr
    || is_lf chr
    || is_cr chr

  (* From RFC 822

       dtext       =  <any CHAR excluding "[",     ; => may be folded
                  "]", BACKSLASH & CR, & including
                  linear-white-space>

     From RFC 2822

       dtext           =       NO-WS-CTL /     ; Non white space controls

                               %d33-90 /       ; The rest of the US-ASCII
                               %d94-126        ;  characters not including "[",
                                               ;  "]", or BACKSLASH

     From RFC 5322

       Update from RFC 2822:
       + Removed NO-WS-CTL from dtext

       dtext           =   %d33-90 /          ; Printable US-ASCII
                           %d94-126 /         ;  characters not including
                           obs-dtext          ;  "[", "]", or BACKSLASH
       obs-dtext       =   obs-NO-WS-CTL / quoted-pair

     Dependence

       dtext          <- domain-literal
       domain-literal <- domain
       domain         <- addr-spec
       addr-spec      <- mailbox

     XXX(dinosaure): [quoted-pair] can not be processed here where we handle only one byte.
  *)
  let is_dtext = function
    | '\033' .. '\090' | '\094' .. '\126' ->
        true
    | c ->
        is_obs_no_ws_ctl c

  let of_escaped_character = function
    | '\x61' ->
        '\x07' (* "\a" *)
    | '\x62' ->
        '\x08' (* "\b" *)
    | '\x74' ->
        '\x09' (* "\t" *)
    | '\x6E' ->
        '\x0A' (* "\n" *)
    | '\x76' ->
        '\x0B' (* "\v" *)
    | '\x66' ->
        '\x0C' (* "\f" *)
    | '\x72' ->
        '\x0D' (* "\r" *)
    | c ->
        c

  (* See [is_quoted_pair] *)
  let quoted_pair_ignore, quoted_pair =
    let quoted_char = char '\\' *> satisfy is_quoted_pair in
    quoted_char *> return (), quoted_char >>| of_escaped_character

  let wsp = satisfy is_wsp

  (* From RFC 822

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

     From RFC 2822

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

       FWS             =       ([*WSP CRLF] 1*WSP) /   ; Folding white space
                               obs-FWS

       In the obsolete syntax, any amount of folding white space MAY be
       inserted where the obs-FWS rule is allowed.  This creates the
       possibility of having two consecutive "folds" in a line, and
       therefore the possibility that a line which makes up a folded header
       field could be composed entirely of white space.

       obs-FWS         =       1*WSP *(CRLF 1*WSP)

     From RFC 5322

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

       FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
                                              ; Folding white space

       In the obsolete syntax, any amount of folding white space MAY be
       inserted where the obs-FWS rule is allowed.  This creates the
       possibility of having two consecutive "folds" in a line, and
       therefore the possibility that a line which makes up a folded header
       field could be composed entirely of white space.

       obs-FWS         =   1*WSP *(CRLF 1*WSP)

     Dependence

       FWS <- CFWS

     val fws: (bool, bool, bool) t

     - the first bool say if we have WSP BEFORE CRLF
     - the second bool say if we have CRLF
     - the third bool say if we have WSP AFTER CRLF

     Impossible case: (true, false, true), we set to [true] the third value
     only if we found a CRLF, so the third bool __could__ be [true] only
     if the second bool __is__ [true].

     XXX(dinosaure): [FWS] is a special token about mail (according RFC 822)
     and should never occur in real/usual inputs (like value of a form). [FWS] complexifies
     the way to parse an email address at the end. However, [emile] should be used in usual
     context where input does not have any [FWS] token.

     Be aware, if you want to extract an email address from an email, we should do a first
     pass with [unstrctrd] to remove [FWS] token. Then, output can be handle by [emile].
  *)
  let fws = take_while1 is_wsp

  (* From RFC 822

       comment     =  "(" *(ctext / quoted-pair / comment) ")"

     From RFC 2822

       ccontent        =       ctext / quoted-pair / comment
       comment         =       "(" *([FWS] ccontent) [FWS] ")"

     From RFC 5322

       ccontent        =   ctext / quoted-pair / comment
       comment         =   "(" *([FWS] ccontent) [FWS] ")"

     Dependence

       comment <- CFWS
  *)
  let comment =
    fix @@ fun comment ->
    let ccontent =
      peek_char_fail
      <?> "comment"
      >>= function
      | '(' ->
          comment
      | '\\' ->
          quoted_pair_ignore
      | c when is_ctext c ->
          skip_while is_ctext
          (* TODO: replace skip_while and handle unicode. *)
      | _ ->
          fail "comment"
    in
    char '('
    *> many (option "" fws *> ccontent)
    *> option "" fws
    *> char ')'
    *> return ()

  (* From RFC 822

       See [obs_fws] and [fws].

     From RFC 2822

       CFWS            =       *([FWS] comment) (([FWS] comment) / FWS)

     From RFC 5322

       CFWS            =   (1*([FWS] comment) [FWS]) / FWS

       Update from RFC 2822:
       + Simplified CFWS syntax.

     Dependence

       CFWS is needed for all
  *)
  let cfws =
    ( many1 (option "" fws *> comment)
      *> option "" fws
    <|> fws )
    *> return ()

  let cfws = cfws <?> "cfws"
  let is_ascii = function '\000' .. '\127' -> true | _ -> false
  let uchar_is_ascii x = (Uchar.to_int x) >= 0 && (Uchar.to_int x) <= 0x7f

  let with_uutf is =
    let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
    let buf = Buffer.create 0x100 in
    let rec go byte_count = match Uutf.decode decoder with
      | `Await -> `Continue
      | `Malformed _ -> `Error "Invalid UTF-8 character"
      | `Uchar uchar when uchar_is_ascii uchar ->
        if is (Uchar.to_char uchar)
        then ( Uutf.Buffer.add_utf_8 buf uchar ; go byte_count)
        else ( `End (Uutf.decoder_byte_count decoder - byte_count - 1) )
      | `Uchar uchar -> Uutf.Buffer.add_utf_8 buf uchar ; go byte_count
      | `End -> (`End (Uutf.decoder_byte_count decoder - byte_count)) in
    let scan buf ~off ~len =
      let src = Bigstringaf.substring buf ~off ~len in
      Uutf.Manual.src decoder (Bytes.unsafe_of_string src) 0 len ;
      go (Uutf.decoder_byte_count decoder) in
    fix @@ fun m ->
    available >>= fun len -> Unsafe.peek len scan >>= function
    | `Error err -> fail err
    | `Continue -> advance len >>= fun () -> m
    | `End len ->
      advance len >>= fun () ->
      return (Buffer.contents buf)

  let with_uutf1 is =
    available >>= fun n ->
    if n > 0
    then ( with_uutf is >>= fun s ->
           if String.length s > 0
           then return s
           else fail "with_uutf1" )
    else fail "with_uutf1"

  (* From RFC 822

       The ABNF of qcontent is not explicit from RFC 822 but the relic could be find here:

       quoted-string = <"> *(qtext/quoted-pair) <">; Regular qtext or

     From RFC 2822

       qcontent        =       qtext / quoted-pair

     From RFC 5322

       qcontent        =   qtext / quoted-pair

     From RFC 5335

       utf8-qcontent      = utf8-qtext / utf8-quoted-pair

     Dependence

       qcontent      <- quoted-string
       quoted-string <- word
       word          <- local-part
       local-part    <- addr-spec
       addr-spec     <- mailbox
  *)
  let qcontent =
    with_uutf1 is_qtext (* TODO: replace take_while and handle unicode. *)
    <|> (quoted_pair >>| String.make 1)

  let qcontent = qcontent <?> "qcontent"

  (* From RFC 822

       quoted-string = DQUOTE *(qtext/quoted-pair) DQUOTE; Regular qtext or
                                                         ;   quoted chars.

     From RFC 2047

       + An 'encoded-word' MUST NOT appear within a 'quoted-string'

     From RFC 2822

       quoted-string   =       [CFWS]
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

       XXX(dinosaure): in other words, space(s) in [FWS] are "visible" between DQUOTE.

     From RFC 5322

       quoted-string   =   [CFWS]
                           DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                           [CFWS]

       The explanation does not change from RFC 2822.

     Dependence

       quoted-string <- word
       word          <- local-part
       local-part    <- addr-spec
       addr-spec     <- mailbox

     XXX(dinosaure): currently, this implementation has a bug about multiple spaces in
     [quoted-string]. We need to update [fws] to count how many space(s) we skip.

     TODO: optimize and count space(s).
  *)
  let quoted_string =
    option () cfws
    *> char '"'
    *> ( many
           ( option "" fws >>= fun fws -> qcontent
             >>| fun s -> fws ^ s )
       >>= fun pre ->
       option "" fws
       >>| fun fws -> pre @ [ fws ] )
    <* char '"'
    >>| String.concat ""
    <* option () cfws

  (* From RFC 822

       atom        =  1*<any CHAR except specials, SPACE and CTLs>

       Difference from RFC 733:
       - Atoms may not contain SPACE.

     From RFC 2822

       atom            =       [CFWS] 1*atext [CFWS]

     From RFC 5322

       atom            =   [CFWS] 1*atext [CFWS]

     From RFC 5335

       utf8-atom     = [CFWS] 1*utf8-atext [CFWS]

     Dependence

       atom       <- word
       word       <- local-part
       local-part <- addr-spec
       addr-spec  <- mailbox
  *)
  let atom =
    option () cfws *> with_uutf1 is_atext <* option () cfws

  let atom = atom <?> "atom"

  (* From RFC 822

       word        =  atom / quoted-string

     From RFC 2822

       word            =       atom / quoted-string

     From RFC 5322

       word            =   atom / quoted-string

     Dependence

       word       <- atom
       atom       <- obs-local-part & dot-aton
       dot-atom   <- local-part
       local-part <- addr-spec
       addr-spec  <- mailbox
  *)
  let word = atom >>| (fun s -> `Atom s) <|> (quoted_string >>| fun s -> `String s)
  let word = word <?> "word"

  (* From RFC 2822

       dot-atom-text   =       1*atext *("." 1*atext)

     From RFC 5322

       dot-atom-text   =   1*atext *("." 1*atext)

     Dependence

       dot-atom-text <- local-part
       local-part    <- addr-spec
       addr-spec     <- mailbox
  *)
  let dot_atom_text = sep_by1 (char '.') (with_uutf1 is_atext)
  let dot_atom_text = dot_atom_text <?> "dot-atom-text"

  (* From RFC 2822

       dot-atom        =       [CFWS] dot-atom-text [CFWS]

     From RFC 5322

       dot-atom        =   [CFWS] dot-atom-text [CFWS]

     Dependence

       dot-atom   <- local-part
       local-part <- addr-spec
       addr-spec  <- mailbox
  *)
  let dot_atom = option () cfws *> dot_atom_text <* option () cfws
  let dot_atom = dot_atom <?> "dot-atom"

  (* From RFC 822

       local-part  =  word *("." word)             ; uninterpreted
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

     Fron RFC 2822

       local-part      =       dot-atom / quoted-string / obs-local-part
       obs-local-part  =       word *("." word)

       The local-part portion is a domain dependent string.  In addresses,
       it is simply interpreted on the particular host as a name of a
       particular mailbox.

       Update:
       + CFWS within local-parts and domains not allowed.*

    From RFC 5322

      local-part      =   dot-atom / quoted-string / obs-local-part
      obs-local-part  =   word *("." word)

    Dependence

      local-part <- addr-spec
      addr-spec  <- mailbox

     XXX(dinosaure): local-part MUST not be empty.
  *)
  let obs_local_part = sep_by1 (char '.') word
  let obs_local_part = obs_local_part <?> "obs-local-part"

  let local_part =
    let length = function
      | `Atom s ->
          String.length s
      | `String s ->
          String.length s
    in
    obs_local_part
    <|> (dot_atom >>| List.map (fun x -> `Atom x))
    <|> (quoted_string >>| fun s -> [`String s])
    >>= fun local ->
    if List.fold_left (fun a x -> a + length x) 0 local > 0 then return local
    else fail "local-part empty"

  let obs_domain = lift2 (fun x r -> x :: r) atom (many1 (char '.' *> atom))

  (* From RFC 822

       domain-literal =  "[" *(dtext / quoted-pair) "]"

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

     From RFC 2822

       domain-literal  =       [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]

     From RFC 5322

       domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]

     Dependence

       domain-literal <- domain
       domain         <- e-mail
  *)
  let domain_literal =
    option () cfws
    *> char '['
    *> ( many
           ( option "" fws
           *> (with_uutf1 is_dtext <|> (quoted_pair >>| String.make 1)) )
       >>| String.concat "" )
    <* option "" fws
    <* char ']'
    <* option () cfws

  (* From RFC 5321

       Let-dig        = ALPHA / DIGIT
       Ldh-str        = *( ALPHA / DIGIT / "-" ) Let-dig
       address-literal  = "[" ( IPv4-address-literal /
                        IPv6-address-literal /
                        General-address-literal ) "]"
                        ; See Section 4.1.3
       IPv4-address-literal  = Snum 3("."  Snum)
       IPv6-address-literal  = "IPv6:" IPv6-addr
       General-address-literal  = Standardized-tag ":" 1*dcontent
       Standardized-tag  = Ldh-str
                         ; Standardized-tag MUST be specified in a
                         ; Standards-Track RFC and registered with IANA
       dcontent       = %d33-90 / ; Printable US-ASCII
                      %d94-126 ; excl. "[", BACKSLASH, "]"
       Snum           = 1*3DIGIT
                      ; representing a decimal integer
                      ; value in the range 0 through 255
       IPv6-addr      = IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp
       IPv6-hex       = 1*4HEXDIG
       IPv6-full      = IPv6-hex 7(":" IPv6-hex)
       IPv6-comp      = [IPv6-hex *5(":" IPv6-hex)] "::"
                      [IPv6-hex *5(":" IPv6-hex)]
                      ; The "::" represents at least 2 16-bit groups of
                      ; zeros.  No more than 6 groups in addition to the
                      ; "::" may be present.
       IPv6v4-full    = IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
       IPv6v4-comp    = [IPv6-hex *3(":" IPv6-hex)] "::"
                      [IPv6-hex *3(":" IPv6-hex) ":"]
                      IPv4-address-literal
                      ; The "::" represents at least 2 16-bit groups of
                      ; zeros.  No more than 4 groups in addition to the
                      ; "::" and IPv4-address-literal may be present.

     XXX(dinosaure): about IPv4 and IPv6 parser, we use [Ipaddr].
  *)

  let is_dcontent = function
    | '\033' .. '\090' | '\094' .. '\126' ->
        true
    | _ ->
        false

  let ipv4_addr =
    let ipv4_address_literal s =
      let pos = ref 0 in
      try
        let ipv4 = Ipaddr.V4.of_string_raw s pos in
        if !pos = String.length s then return (IPv4 ipv4) else fail "IPv4"
      with Ipaddr.Parse_error _ -> fail "IPv4"
    in
    take_while1 is_dcontent >>= ipv4_address_literal

  let ipv6_addr =
    let ipv6_address_literal s =
      let pos = ref 0 in
      try
        let ipv6 = Ipaddr.V6.of_string_raw s pos in
        if !pos = String.length s then return (IPv6 ipv6) else fail "IPv6"
      with Ipaddr.Parse_error _ -> fail "IPv6"
    in
    string "IPv6:" *> take_while1 is_dcontent >>= ipv6_address_literal

  let let_dig =
    satisfy (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
          true
      | _ ->
          false)

  let ldh_str =
    take_while1 (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
      | _ -> false)
    >>= fun ldh ->
    if ldh.[String.length ldh - 1] = '-'
    then fail "invalid ldh-str"
    else return ldh

  let general_address_literal =
    ldh_str
    <* char ':'
    >>= fun ldh ->
    take_while1 is_dcontent >>| fun value -> Ext (ldh, value)

  (* From RFC 5321

     Let-dig        = ALPHA / DIGIT
     Ldh-str        = *( ALPHA / DIGIT / "-" ) Let-dig
     address-literal  = "[" ( IPv4-address-literal /
                      IPv6-address-literal /
                      General-address-literal ) "]"
                      ; See Section 4.1.3

     Sometimes a host is not known to the domain name system and
     communication (and, in particular, communication to report and repair
     the error) is blocked.  To bypass this barrier, a special literal
     form of the address is allowed as an alternative to a domain name.
     For IPv4 addresses, this form uses four small decimal integers
     separated by dots and enclosed by brackets such as [123.255.37.2],
     which indicates an (IPv4) Internet Address in sequence-of-octets
     form.  For IPv6 and other forms of addressing that might eventually
     be standardized, the form consists of a standardized "tag" that
     identifies the address syntax, a colon, and the address itself, in a
     format specified as part of the relevant standards (i.e., RFC 4291
     [8] for IPv6).

     Specifically:

     IPv4-address-literal  = Snum 3("."  Snum)

     IPv6-address-literal  = "IPv6:" IPv6-addr

     General-address-literal  = Standardized-tag ":" 1*dcontent

     Standardized-tag  = Ldh-str
                       ; Standardized-tag MUST be specified in a
                       ; Standards-Track RFC and registered with IANA
     dcontent       = %d33-90 / ; Printable US-ASCII
                    %d94-126 ; excl. "[", BACKSLASH, "]"

     Snum           = 1*3DIGIT
                    ; representing a decimal integer
                    ; value in the range 0 through 255

     IPv6-addr      = IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp

     IPv6-hex       = 1*4HEXDIG

     IPv6-full      = IPv6-hex 7(":" IPv6-hex)

     IPv6-comp      = [IPv6-hex *5(":" IPv6-hex)] "::"
                    [IPv6-hex *5(":" IPv6-hex)]
                    ; The "::" represents at least 2 16-bit groups of
                    ; zeros.  No more than 6 groups in addition to the
                    ; "::" may be present.

     IPv6v4-full    = IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal

     IPv6v4-comp    = [IPv6-hex *3(":" IPv6-hex)] "::"
                    [IPv6-hex *3(":" IPv6-hex) ":"]
                    IPv4-address-literal
                    ; The "::" represents at least 2 16-bit groups of
                    ; zeros.  No more than 4 groups in addition to the
                    ; "::" and IPv4-address-literal may be present.

     XXX(dinosaure): we use the [Ipaddr] parser about IPv4 and IPv6. Then, the input
     should be a [general_address_literal]. However we decided to accept any input which
     respect [dtext] as a `Literal (see [domain]).
  *)
  let address_literal = ipv4_addr <|> ipv6_addr <|> general_address_literal

  (* From RFC 822

       domain      =  sub-domain *("." sub-domain)
       sub-domain  =  domain-ref / domain-literal
       domain-ref  =  atom                         ; symbolic reference

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

     From RFC 2822

       domain          =       dot-atom / domain-literal / obs-domain
       obs-domain      =       atom *("." atom)

       Update:
       + CFWS within local-parts and domains not allowed.*

     From RFC 5322

       domain          =   dot-atom / domain-literal / obs-domain
       obs-domain      =   atom *("." atom)

     XXX(dinosaure): from the RFC 5322, we should accept any domain as
     [`Literal] and let the user to resolve it. Currently, we fail when
     we catch a [`Literal] and do the best effort where we follow
     RFC 5321. But may be it's inconvenient (or not?) to fail. TODO!
  *)
  let domain =
    let of_string ~error p s =
      match parse_string ~consume:All p s with
      | Ok v -> return v
      | Error _ -> fail error
    in
    let _literal s = return (`Literal s) in
    let addr s =
      of_string ~error:"address-literal" address_literal s
      >>| fun addr -> `Addr addr
    in
    obs_domain
    >>| (fun domain -> `Domain domain)
    <|> (domain_literal >>= fun s -> addr s)
    <|> (dot_atom >>| fun domain -> `Domain domain)

  (* From RFC 2822

       obs-id-left     =       local-part
       no-fold-quote   =       DQUOTE *(qtext / quoted-pair) DQUOTE
       id-left         =       dot-atom-text / no-fold-quote / obs-id-left

     From RFC 5322

       id-left         =   dot-atom-text / obs-id-left
       obs-id-left     =   local-part

     XXX(dinosaure): we took the RFC 5322's ABNF, the no-fold-quote token
     is available on the local-part as quoted-string.
  *)
  let id_left = local_part <|> (dot_atom_text >>| List.map (fun x -> `Atom x))

  (* From RFC 2822

       no-fold-literal =       "[" *(dtext / quoted-pair) "]"

     From RFC 5322

       no-fold-literal =   "[" *dtext "]"

     Dependence

       no-fold-literal <- id-right
       id-right        <- e-mail
  *)
  let no_fold_literal =
    char '[' *> with_uutf is_dtext
    (* TODO: replace take_while and handle unicode. *)
    <* char ']'

  (* From RFC 2822

       id-right        =       dot-atom-text / no-fold-literal / obs-id-right
       obs-id-right    =       domain

     From RFC 5322

       id-right        =   dot-atom-text / no-fold-literal / obs-id-right
       obs-id-right    =   domain
  *)
  let id_right =
    no_fold_literal
    >>| (fun literal -> `Literal literal)
    <|> domain
    <|> (dot_atom_text >>| fun domain -> `Domain domain)

  (* From RFC 822

       addr-spec   =  local-part "@" domain        ; global address
       msg-id      =  "<" addr-spec ">"            ; Unique message id

     From RFC 2822

       msg-id          =       [CFWS] "<" id-left "@" id-right ">" [CFWS]

       Update:
       + CFWS within msg-id not allowed.*

       The message identifier (msg-id) is similar in syntax to an angle-addr
       construct without the internal CFWS.

     From RFC 5322

       msg-id          =   [CFWS] "<" id-left "@" id-right ">" [CFWS]

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

     Dependence

       msg-id __is not__ used by mailbox
  *)
  let msg_id =
    option () cfws
    *> lift2
         (fun x y -> x, y)
         (char '<' *> id_left)
         (char '@' *> id_right <* char '>')
    <* option () cfws

  let filter_map predicate lst =
    List.fold_right
      (fun x a -> match predicate x with Some x -> x :: a | None -> a)
      lst []

  (* From RFC 822

       addr-spec   =  local-part "@" domain        ; global address

     From RFC 2822

       An addr-spec is a specific Internet identifier that contains a
       locally interpreted string followed by the at-sign character ("@",
       ASCII value 64) followed by an Internet domain.  The locally
       interpreted string is either a quoted-string or a dot-atom.  If the
       string can be represented as a dot-atom (that is, it contains no
       characters other than atext characters or "." surrounded by atext
       characters), then the dot-atom form SHOULD be used and the
       quoted-string form SHOULD NOT be used. Comments and folding white
       space SHOULD NOT be used around the "@" in the addr-spec.

       addr-spec       =       local-part "@" domain

     From RFC 5322

       Note: A liberal syntax for the domain portion of addr-spec is
       given here.  However, the domain portion contains addressing
       information specified by and used in other protocols (e.g.,
       [RFC1034], [RFC1035], [RFC1123], [RFC5321]).  It is therefore
       incumbent upon implementations to conform to the syntax of
       addresses for the context in which they are used.

       addr-spec       =   local-part "@" domain

     Dependence

       addr-spec <- mailbox

     XXX(dinosaure): about domain, we follow RFC 5321.
  *)
  let addr_spec =
    lift2
      (fun local d -> {name= None; local; domain= d, []})
      (local_part <?> "local-part")
      (char '@' >>= fun _ -> (domain <?> "domain"))

  let addr_spec = addr_spec <?> "addr-spec"

  let obs_domain_list =
    many (cfws <|> char ',' *> return ()) *> char '@' *> domain
    >>= fun first ->
    many
      ( char ','
      *> option () cfws
      *> option None (char '@' *> domain >>| fun x -> Some x) )
    >>| filter_map (fun x -> x)
    >>| fun rest -> first :: rest

  let obs_route = obs_domain_list <* char ':'

  let obs_angle_addr =
    option () cfws *> char '<' *> obs_route
    >>= fun domains ->
    addr_spec
    >>= (function
          | {domain= _, []; _} as addr ->
              return {addr with domain= fst addr.domain, domains}
          | _ ->
              fail "Invalid addr-spec")
    <* char '>'
    <* option () cfws

  (* From RFC 822

       The ABNF of angle-addr is not explicit from RFC 822 but the relic could be find here,
       as a part of mailbox:

       mailbox     =  addr-spec                    ; simple address
                   /  phrase route-addr            ; name & addr-spec

     From RFC 2822

       obs-domain-list =       "@" domain *( *(CFWS / "," ) [CFWS] "@" domain)
       obs-route       =       [CFWS] obs-domain-list ":" [CFWS]
       obs-angle-addr  =       [CFWS] "<" [obs-route] addr-spec ">" [CFWS]
       angle-addr      =       [CFWS] "<" addr-spec ">" [CFWS] / obs-angle-addr

     From RFC 5322

       obs-domain-list =   *(CFWS / ",") "@" domain
                           *("," [CFWS] ["@" domain])
       obs-route       =   obs-domain-list ":"
       obs-angle-addr  =   [CFWS] "<" obs-route addr-spec ">" [CFWS]
       angle-addr      =   [CFWS] "<" addr-spec ">" [CFWS] /

     Dependence

       angle-addr <- name-addr
       name-addr  <- mailbox
  *)
  let angle_addr =
    option () cfws *> char '<' *> addr_spec
    <* char '>'
    <* option () cfws
    <|> obs_angle_addr

  (* From RFC 822

       phrase      =  1*word                       ; Sequence of words

     From RFC 2047

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

     From RFC 2822

       obs-phrase      =       word *(word / "." / CFWS)
       phrase          =       1*word / obs-phrase

       Update:
       + Period allowed in obsolete form of phrase.

     From RFC 5322

       phrase          =   1*word / obs-phrase

       Note: The "period" (or "full stop") character (".") in obs-phrase
       is not a form that was allowed in earlier versions of this or any
       other specification.  Period (nor any other character from
       specials) was not allowed in phrase because it introduced a
       parsing difficulty distinguishing between phrases and portions of
       an addr-spec (see section 4.4).  It appears here because the
       period character is currently used in many messages in the
       display-name portion of addresses, especially for initials in
       names, and therefore must be interpreted properly.

       obs-phrase      =   word *(word / "." / CFWS)

     Dependence

       phrase       <- display-name
       display-name <- name-addr
       name-addr    <- mailbox
  *)
  let is_especials = function
    | '('
    | ')'
    | '<'
    | '>'
    | '@'
    | ','
    | ';'
    | ':'
    | '"'
    | '/'
    | '['
    | ']'
    | '?'
    | '.'
    | '=' ->
        true
    | _ ->
        false

  let is_ctl = function '\000' .. '\031' -> true | _ -> false
  let is_space = ( = ) ' '

  let token =
    take_while1 (fun chr ->
        not (is_especials chr || is_ctl chr || is_space chr))

  let is_b64 = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/' ->
        true
    | _ ->
        false

  let base64 = take_till (( = ) '?') >>| fun x -> Base64.decode x

  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ->
        true
    | _ ->
        false

  let hex a b =
    let aux code =
      match code with
      | '0' .. '9' ->
          Char.code code - Char.code '0' + 0
      | 'a' .. 'f' ->
          Char.code code - Char.code 'a' + 10
      | 'A' .. 'F' ->
          Char.code code - Char.code 'A' + 10
      | _ ->
          assert false
    in
    Char.chr ((aux a * 16) + aux b)

  let hex =
    char '=' *> satisfy is_hex
    >>= fun a -> satisfy is_hex >>= fun b -> return (hex a b)

  let quoted_printable =
    take_till (( = ) '?')
    >>| fun s ->
    let decoder = Pecu.Inline.decoder (`String s) in
    let result = Buffer.create 16 in
    let rec go () =
      match Pecu.Inline.decode decoder with
      | `Await ->
          assert false
          (* XXX(dinosaure): impossible case with [src = `String _] *)
      | `Char chr ->
          Buffer.add_char result chr ; go ()
      | `End ->
          Ok (Buffer.contents result)
      | `Malformed err ->
          Error (`Msg err)
    in
    go ()

  let encoded_word =
    string "=?" *> token
    >>= fun charset ->
    char '?' *> satisfy (function 'Q' | 'B' -> true | _ -> false)
    >>= (function 'Q' -> return `Q | 'B' -> return `B | _ -> assert false)
    >>= fun encoding ->
    char '?'
    (* XXX(dinosaure): in this part, we allocate in both cases a buffer, it
       could be interesting to find an other way to return decoded content
       (Base64 or Quoted-Printable). *)
    >>= fun _ ->
    ( match encoding with
    | `B ->
        base64 >>| fun v -> Base64 v
    | `Q ->
        quoted_printable >>| fun v -> Quoted_printable v )
    >>= fun decoded -> string "?=" *> return (charset, decoded)

  (* XXX(dinosaure): I did not find mention of CFWS token which surrounds
     encoded-word. However, this code come from Mr. MIME which passes all tests.
     So, I decide to let CFWS token but we need to understand why. TODO! *)
  let extended_word =
    option () cfws *> (encoded_word >>| fun x -> `Encoded x)
    <* option () cfws
    <|> (word >>| fun x -> `Word x)

  let obs_phrase =
    extended_word
    >>= fun first ->
    fix (fun m ->
        lift2
          (function
            | (`Dot | `Word _ | `Encoded _) as x ->
                fun r -> x :: r
            | `CFWS ->
                fun r -> r)
          ( extended_word
          <|> (char '.' >>| fun _ -> `Dot)
          <|> (cfws >>| fun () -> `CFWS) )
          m
        <|> return [])
    >>| fun rest -> first :: rest

  let phrase = obs_phrase <|> many1 extended_word

  (* From RFC 822

       The ABNF of name-addr is not explicit from RFC 822 but the relic could be find here:

       mailbox     =  addr-spec                    ; simple address
                   /  phrase route-addr            ; name & addr-spec
     From RFC 2822

       display-name    =       phrase
       name-addr       =       [display-name] angle-addr

       Note: Some legacy implementations used the simple form where the
       addr-spec appears without the angle brackets, but included the name
       of the recipient in parentheses as a comment following the addr-spec.
       Since the meaning of the information in a comment is unspecified,
       implementations SHOULD use the full name-addr form of the mailbox,
       instead of the legacy form, to specify the display name associated
       with a mailbox.  Also, because some legacy implementations interpret
       the comment, comments generally SHOULD NOT be used in address fields
       to avoid confusing such implementations.

     From RFC 5322

       name-addr       =   [display-name] angle-addr
       display-name    =   phrase

     Dependence

       name-addr <- mailbox
  *)
  let display_name = phrase

  let name_addr =
    option None (display_name >>| fun x -> Some x)
    >>= fun name -> angle_addr >>| fun addr -> {addr with name}

  let name_addr = name_addr <?> "name-addr"

  (* Last (but not least).

     Discard RFC 720.
     Discard RFC 724.
     Discard RFC 733.

     From RFC 822

       mailbox     =  addr-spec                    ; simple address
                   /  phrase route-addr            ; name & addr-spec

     From RFC 2822

       mailbox         =       name-addr / addr-spec

     From RFC 5322

       mailbox         =   name-addr / addr-spec
  *)
  let mailbox = name_addr <|> addr_spec <?> "mailbox"

  let obs_mbox_list =
    let rest =
      fix (fun m ->
          lift2
            (function `Mailbox x -> fun r -> x :: r | `Sep -> fun r -> r)
            ( char ','
            *> option `Sep
                 ( mailbox
                 >>| (fun m -> `Mailbox m)
                 <|> (cfws >>| fun () -> `Sep) ) )
            m
          <|> return [])
    in
    many (option () cfws *> char ',') *> mailbox
    >>= fun x -> rest >>| fun r -> x :: r

  let obs_group_list = many1 (option () cfws *> char ',') *> option () cfws

  let mailbox_list =
    obs_mbox_list
    <|> (mailbox >>= fun x -> many (char ',' *> mailbox) >>| fun r -> x :: r)

  let group_list =
    mailbox_list
    <|> (obs_group_list >>| fun () -> [])
    <|> (cfws >>| fun () -> [])

  let group =
    display_name
    >>= fun group ->
    char ':' *> (option [] group_list <?> "group-list")
    >>= fun mailboxes ->
    char ';' *> option () cfws >>| fun _ -> {group; mailboxes}

  let address =
    group >>| (fun g -> `Group g) <|> (mailbox >>| fun m -> `Mailbox m)

  let obs_addr_list =
    let rest =
      fix @@ fun m ->
      lift2
        (function `Addr x -> fun r -> x :: r | `Sep -> fun r -> r)
        (char ',' *> option `Sep (address >>| (fun a -> `Addr a) <|> (cfws >>| fun () -> `Sep))) m
      <|> return [] in
    many (option () cfws *> char ',') *> address
    >>= fun x -> rest >>| fun r -> x :: r

  let address_list =
    obs_addr_list
    <?> "obs-addr-list"
    <|> ( address
        >>= (fun x -> many (char ',' *> address) >>| fun r -> x :: r)
        <?> "regular-address-list" )
end

type error = [ `Invalid of string * string ]

let pp_error ppf = function
  | `Invalid (committed, rest) -> Fmt.pf ppf "Invalid email address: %s%s" committed rest

let of_string parser src tmp off max =
  let open Angstrom.Unbuffered in
  let rec k1 len = function
    | Done (committed, v) -> Ok (committed, v)
    | Partial { continue; committed; } ->
      k1 (len - committed) (continue tmp ~off:committed ~len:(len - committed) Complete)
    | Fail (committed, _, _) ->
      let committed, rest =
        String.sub src 0 committed,
        String.sub src committed (String.length src - committed) in
      Error (`Invalid (committed, rest))
  and k0 pos cur = function
    | Done (committed, v) -> Ok (committed, v)
    | Fail (committed, _, _) ->
      let committed, rest =
        String.sub src 0 committed,
        String.sub src committed (String.length src - committed) in
      Error (`Invalid (committed, rest))
    | Partial { continue; committed; } ->
      let len = min (Bigstringaf.length tmp - committed) (max - pos) in
      Bigstringaf.blit tmp ~src_off:committed tmp ~dst_off:0 ~len:cur ;
      Bigstringaf.blit_from_string src ~src_off:off tmp ~dst_off:cur ~len ;
      match max - (pos + len) with
      | 0 -> k1 (cur + len) (continue tmp ~off:0 ~len:(cur + len) Complete)
      | _ -> k0 (pos + len) (cur + len) (continue tmp ~off:0 ~len:(cur + len) Incomplete) in
  k0 0 0 (Angstrom.Unbuffered.parse parser)

let of_string_with_crlf parser src tmp off max =
  let parser =
    let open Angstrom in
    parser <* char '\r' <* char '\n' <* commit in
  of_string parser src tmp off max

let with_tmp k parser src off len =
  let tmp = Bigstringaf.create len in
  k parser src tmp off len

let with_off_and_len k parser src =
  let len = String.length src in
  with_tmp k parser src 0 len

let rr_map f = function Ok v -> Ok (f v) | Error _ as err -> err
let ( >|= ) a f = rr_map f a

module List = struct
  let of_string_raw ~off ~len ?(tmp = Bigstringaf.create len) src =
    of_string Parser.address_list src tmp off len
  let of_string_with_crlf src =
    with_off_and_len of_string_with_crlf Parser.address_list src >|= snd
  let of_string src =
    with_off_and_len of_string Parser.address_list src >|= snd
  let to_string lst =
    Format.asprintf "%a" str_addresses lst
end

let address_of_string_with_crlf src =
  with_off_and_len of_string_with_crlf Parser.addr_spec src
  >|= fun (_, { local; domain; _ }) -> local, domain

let address_of_string src =
  with_off_and_len of_string Parser.addr_spec src
  >|= fun (_, { local; domain; _ }) -> local, domain

let address_of_string_raw ~off ~len ?(tmp= Bigstringaf.create len) src =
  of_string Parser.addr_spec src tmp off len
  >|= fun (committed, { local; domain; _ }) -> (committed, (local, domain))

let set_of_string_raw ~off ~len ?(tmp = Bigstringaf.create len) src =
  of_string Parser.address src tmp off len
let set_of_string_with_crlf src =
  with_off_and_len of_string_with_crlf Parser.address src >|= snd
let set_of_string src =
  with_off_and_len of_string Parser.address src >|= snd

let of_string_raw ~off ~len ?(tmp = Bigstringaf.create len) src =
  of_string Parser.mailbox src tmp off len
let of_string_with_crlf src =
  with_off_and_len of_string_with_crlf Parser.mailbox src >|= snd
let of_string src =
  with_off_and_len of_string Parser.mailbox src >|= snd

let to_string mailbox =
  Format.asprintf "%a" str_mailbox mailbox

let set_to_string = function
  | `Mailbox m -> Format.asprintf "%a" str_mailbox m
  | `Group g -> Format.asprintf "%a" str_group g

let address_to_string address =
  Format.asprintf "%a" str_address address
