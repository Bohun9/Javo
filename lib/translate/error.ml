let type_error (msg : string) : 'a =
  Printf.eprintf "Type error: %s\n" msg;
  exit 1
;;
