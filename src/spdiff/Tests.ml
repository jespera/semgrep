let spdiff_tag = Testo.Tag.declare "spdiff"

let tests: Testo.t list = [
  Testo.create  ~tags:[spdiff_tag]
  "Edit dist test"
  (fun () ->
    print_endline "Init";
    failwith "Not implemented"
  )
]
