let exit_success = 0

let exit_failure = 1

let () = Fmt.set_utf_8 Fmt.stdout true

let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty

let run filename =
  let ic = open_in filename in
  let rec go ic res =
    match input_line ic with
    | str -> (
        match Emile.of_string str with
        | Ok v ->
            Fmt.pr "[%a]: %a\n%!"
              Fmt.(styled `Green string)
              "OK" Emile.pp_mailbox v ;
            go ic res
        | Error _ ->
            Fmt.pr "[%a]: %S\n%!" Fmt.(styled `Red string) "ER" str ;
            go ic false)
    | exception End_of_file ->
        close_in ic ;
        res in
  if go ic true then exit exit_success else exit exit_failure

let () =
  match Sys.argv with
  | [| _; filename |] when Sys.file_exists filename -> run filename
  | _ -> Fmt.epr "%s <filename>\n%!" Sys.argv.(0)
