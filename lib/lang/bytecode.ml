type label = string [@@deriving show]

module LabelMap = Map.Make (struct
    type t = label

    let compare = compare
  end)

module IntMap = Map.Make (struct
    type t = int

    let compare = compare
  end)

type instr =
  | ILoadNull
  | ILoadInt of int
  | ILoadLocal of { stack_loc : int }
  | IAdd
  | ISub
  | IMul
  | IEq
  | INewObject of { class_id : int }
  | IMethodCall of
      { method_index : int
      ; num_args : int
      }
  | ILocalAssign of { stack_loc : int }
  | IFieldAssign of { field_index : int }
  | IFetch of { field_index : int }
  | IPop
  | IPopLet
  | ILabel of label
  | IBranch of label
  | IJump of label
  | IReturn
  | IExit
[@@deriving show]

type bytecode_fn = string * instr list [@@deriving show]
type dispatch_table = label list [@@deriving show]
type class_table = int * dispatch_table [@@deriving show]

type bytecode =
  | BC_Program of
      { functions : bytecode_fn list
      ; class_tables : class_table list
      }
[@@deriving show]

let opcode (i : instr) : char =
  let id =
    match i with
    | ILoadNull -> 0
    | ILoadInt _ -> 1
    | ILoadLocal _ -> 2
    | IAdd -> 3
    | INewObject _ -> 4
    | IMethodCall _ -> 5
    | ILocalAssign _ -> 6
    | IFieldAssign _ -> 7
    | IFetch _ -> 8
    | IPop -> 9
    | ILabel _ -> failwith "Label instruction has no opcode"
    | IBranch _ -> 10
    | IReturn -> 11
    | IExit -> 12
    | IPopLet -> 13
    | ISub -> 14
    | IMul -> 15
    | IEq -> 16
    | IJump _ -> 17
  in
  Char.chr id
;;

let bytecode_to_bytes (BC_Program { functions; class_tables } : bytecode) : bytes =
  let label_address = ref LabelMap.empty in
  let class_address = ref IntMap.empty in
  let current_byte = ref 0 in
  let process_instr instr =
    (match instr with
     | ILabel l -> label_address := LabelMap.add l !current_byte !label_address
     | _ -> incr current_byte);
    let payload_words =
      match instr with
      | ILoadInt _ -> 1
      | ILoadLocal _ -> 1
      | INewObject _ -> 1
      | IMethodCall _ -> 2
      | ILocalAssign _ -> 1
      | IFieldAssign _ -> 1
      | IFetch _ -> 1
      | IBranch _ -> 1
      | IJump _ -> 1
      | _ -> 0
    in
    current_byte := !current_byte + (8 * payload_words)
  in
  let process_class_table id class_table =
    class_address := IntMap.add id !current_byte !class_address;
    current_byte := !current_byte + (8 * (1 + List.length (snd class_table)))
  in
  let instrs = List.concat (List.map snd functions) in
  List.iter process_instr instrs;
  List.iteri process_class_table class_tables;
  let bytecode_size = !current_byte in
  let bytes = Bytes.create bytecode_size in
  current_byte := 0;
  let insert_byte (c : char) : unit =
    Bytes.set bytes !current_byte c;
    current_byte := !current_byte + 1
  in
  let insert_int (n : int) : unit =
    Bytes.set_int64_le bytes !current_byte (Int64.of_int n);
    current_byte := !current_byte + 8
  in
  let fill_instr instr =
    match instr with
    | ILabel _ -> ()
    | _ ->
      insert_byte (opcode instr);
      let payload =
        match instr with
        | ILoadInt n -> [ n ]
        | ILoadLocal { stack_loc } -> [ stack_loc ]
        | INewObject { class_id } -> [ IntMap.find class_id !class_address ]
        | IMethodCall { method_index; num_args } -> [ method_index; num_args ]
        | ILocalAssign { stack_loc } -> [ stack_loc ]
        | IFieldAssign { field_index } -> [ field_index ]
        | IFetch { field_index } -> [ field_index ]
        | IBranch label -> [ LabelMap.find label !label_address ]
        | IJump label -> [ LabelMap.find label !label_address ]
        | _ -> []
      in
      List.iter insert_int payload
  in
  let fill_class_table (num_fields, dispatch_table) =
    insert_int num_fields;
    List.iter (fun l -> insert_int (LabelMap.find l !label_address)) dispatch_table
  in
  List.iter fill_instr instrs;
  List.iter fill_class_table class_tables;
  assert (bytecode_size == !current_byte);
  bytes
;;
