let source_file = ref ""
let output_file = ref "a.jbc"
let dump_bytecode = ref false

let speclist =
  [ "--dump-bytecode", Arg.Set dump_bytecode, "Dump bytecode"
  ; "-o", Arg.Set_string output_file, "Specify output file (default: a.jbc)"
  ]
;;

let usage_msg = "Usage: javo <source_file> [-o <output_file>] [--dump-bytecode]"
let anon_fun filename = source_file := filename

let () =
  Arg.parse speclist anon_fun usage_msg;
  let source_program = In_channel.with_open_bin !source_file In_channel.input_all in
  let bytecode = Javo.Pipeline.pipeline source_program in
  if !dump_bytecode
  then Printf.printf "%s\n" (Lang.Bytecode.show_bytecode bytecode)
  else (
    let bytes = Lang.Bytecode.bytecode_to_bytes bytecode in
    Out_channel.with_open_bin !output_file (fun oc -> Out_channel.output_bytes oc bytes))
;;
