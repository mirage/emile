val to_mrmime : Emile.mailbox -> (Mrmime.Mailbox.t, [ `Msg of string ]) result
val of_mrmime : Mrmime.Mailbox.t -> (Emile.mailbox, [ `Msg of string ]) result
