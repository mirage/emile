open Cmdliner

let mailbox =
  let parser x = match Emile.of_string x with
    | Ok v -> Ok v
    | Error err -> Rresult.R.error_msgf "%a" Emile.pp_error err in
  let pp = Emile.pp_mailbox in
  Arg.conv ~docv:"<mailbox>" (parser, pp)
