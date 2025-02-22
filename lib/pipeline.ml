let pipeline (source : string) : Lang.Bytecode.bytecode =
  let lexbuf = Lexing.from_string source in
  let ast =
    try Parser.program Lexer.token lexbuf with
    | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let column = pos.pos_cnum - pos.pos_bol in
      failwith (Printf.sprintf "Syntax error at (%d, %d)" line column)
  in
  let () = Translate.Semant.trans_program ast in
  let bytecode = Translate.Codegen.get_bytecode () in
  bytecode
;;
