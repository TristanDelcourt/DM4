type t =
  ([ `foo
   | `bar  (** 58 chars.................................................. *) ][@js.enum
                                                                       ])
let _ =
  aa
    (bbbbbbbbb cccccccccccc dddddddddddddddddddddddddddddddddddddddddddddddddddd)

let _ =
  aa
    (bbbbbbbbb cccccccccccc dddddddddddddddddddddddddddddddddddddd [@dddddddddd])

let _ =
  aa
    (bbbbbbbbb cccccccccccc ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
     [@dddddddddd])

let _ =
  aa
    (bbbbbbbbb cccccccccccc dddddddddddddddddddddddddddddddddddddd) [@dddddddddd]
