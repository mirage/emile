let some x = Some x

let to_mrmime mailbox =
  let local = mailbox.Emile.local in
  let domain, domains = mailbox.Emile.domain in
  let name = match mailbox.Emile.name with
    | None -> None
    | Some phrase ->
      let res =
        List.map
          (function
            | `Dot -> Ok `Dot |
              `Word _ as word -> Ok word
            | `Encoded (_, Emile.Base64 (Error _ as err))
            | `Encoded (_, Emile.Quoted_printable (Error _ as err)) -> err
            | `Encoded (c, Emile.Base64 (Ok raw)) ->
              let open Rresult.R in
              Mrmime.Encoded_word.(normalize_to_utf8 ~charset:(charset_of_string c) raw)
              >>= Mrmime.Encoded_word.(make ~encoding:q)
              >>| fun e -> `Encoded e
            | `Encoded (c, Emile.Quoted_printable (Ok raw)) ->
              let open Rresult.R in
              Mrmime.Encoded_word.(normalize_to_utf8 ~charset:(charset_of_string c) raw)
              >>= Mrmime.Encoded_word.(make ~encoding:q)
              >>| fun e -> `Encoded e)
          phrase in
      let open Rresult.R in
      some @@ (List.fold_left
                 (fun a x -> match a, x with
                    | (Error _ as err), _ -> err
                    | _, (Error _ as err) -> err
                    | Ok a, Ok x -> Ok (x :: a))
                 (Ok []) res >>| List.rev) in
  let cast_domain = function
    | `Domain _ | `Literal _ as v -> v
    | `Addr (Emile.Ext (k, v)) -> `Addr (Mrmime.Mailbox.Ext (k, v))
    | `Addr (Emile.IPv4 v) -> `Addr (Mrmime.Mailbox.IPv4 v)
    | `Addr (Emile.IPv6 v) -> `Addr (Mrmime.Mailbox.IPv6 v) in
  match name with
  | Some (Ok name) ->
    Ok (Mrmime.Mailbox.make ~name local ~domains:(List.map cast_domain domains) (cast_domain domain))
  | None ->
    Ok (Mrmime.Mailbox.make local ~domains:(List.map cast_domain domains) (cast_domain domain))
  | Some (Error _ as err) -> err

let of_mrmime mailbox =
  let local = mailbox.Mrmime.Mailbox.local in
  let domain, domains = mailbox.Mrmime.Mailbox.domain in
  let name = match mailbox.Mrmime.Mailbox.name with
    | None -> None
    | Some phrase ->
      let res =
        List.map
          (function
            | `Dot -> Ok `Dot
            | `Word _ as w -> Ok w
            | `Encoded { Mrmime.Encoded_word.data= Ok raw; encoding= Quoted_printable; _ } ->
              Ok (`Encoded ("utf8", Emile.Quoted_printable (Ok raw)))
            | `Encoded { Mrmime.Encoded_word.data= Ok raw; encoding= Base64; _ } ->
              Ok (`Encoded ("utf8", Emile.Base64 (Ok raw)))
            | `Encoded { Mrmime.Encoded_word.data= (Error _ as err); _ } -> err)
          phrase in
      let open Rresult.R in
      some @@ (List.fold_left
                 (fun a x -> match a, x with
                    | (Error _ as err), _ -> err
                    | _, (Error _ as err) -> err
                    | Ok a, Ok x -> Ok (x :: a))
                 (Ok []) res >>| List.rev) in
  let cast_domain = function
    | `Domain _ | `Literal _ as v -> v
    | `Addr (Mrmime.Mailbox.Ext (k, v)) -> `Addr (Emile.Ext (k, v))
    | `Addr (Mrmime.Mailbox.IPv4 v) -> `Addr (Emile.IPv4 v)
    | `Addr (Mrmime.Mailbox.IPv6 v) -> `Addr (Emile.IPv6 v) in
  match name with
  | Some (Ok name) ->
    Ok { Emile.name= Some name; local; domain= (cast_domain domain, List.map cast_domain domains) }
  | None ->
    Ok { Emile.name= None; local; domain= (cast_domain domain, List.map cast_domain domains) }
  | Some (Error _ as err) -> err
