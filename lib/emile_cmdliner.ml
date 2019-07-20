open Cmdliner

let mailbox =
  let parser x = match Emile.of_string x with
    | Ok v -> Ok v
    | Error _ -> Rresult.R.error_msgf "%S is an invalid mail address" x in
  let pp = Emile.pp_mailbox in
  Arg.conv ~docv:"<mailbox>" (parser, pp)
