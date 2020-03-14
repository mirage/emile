open Cmdliner

let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

let mailbox =
  let parser x = match Emile.of_string x with
    | Ok v -> Ok v
    | Error _ -> error_msgf "%S is an invalid mail address" x in
  let pp = Emile.pp_mailbox in
  Arg.conv ~docv:"<mailbox>" (parser, pp)
