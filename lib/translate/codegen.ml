module BC = Lang.Bytecode

type codegen_state =
  { mutable compiled_functions : BC.bytecode_fn list
  ; mutable class_tables : BC.class_table list
  ; mutable function_name : string
  ; mutable partial_code : BC.instr list
  ; mutable cur_stack_size : int
  }

type stack_loc = int

let state =
  { compiled_functions = []
  ; class_tables = []
  ; function_name = ""
  ; partial_code = []
  ; cur_stack_size = 0
  }
;;

let get_bytecode () =
  BC.BC_Program
    { functions = state.compiled_functions; class_tables = state.class_tables }
;;

let get_cur_stack_loc () = state.cur_stack_size - 1
let append_instr (i : BC.instr) : unit = state.partial_code <- state.partial_code @ [ i ]
let change_stack_size (d : int) : unit = state.cur_stack_size <- state.cur_stack_size + d
let emit_label label = append_instr (BC.ILabel label)

let start_new_function name num_params label_opt =
  state.function_name <- name;
  state.partial_code <- [];
  state.cur_stack_size <- num_params;
  (match label_opt with
   | Some label -> emit_label label
   | None -> ());
  List.init num_params (fun x -> x)
;;

let emit_function is_toplevel =
  if is_toplevel then append_instr BC.IExit else append_instr BC.IReturn;
  state.compiled_functions
  <- (state.function_name, state.partial_code) :: state.compiled_functions
;;

let register_class_table num_fields dispatch_table =
  state.class_tables <- state.class_tables @ [ num_fields, dispatch_table ];
  List.length state.class_tables - 1
;;

let fresh_label_id = ref 0

let gen_fresh_label () : BC.label =
  let l = "L" ^ string_of_int !fresh_label_id in
  incr fresh_label_id;
  l
;;

let emit_null () =
  append_instr BC.ILoadNull;
  change_stack_size 1
;;

let emit_int n =
  append_instr (BC.ILoadInt n);
  change_stack_size 1
;;

let emit_local loc =
  append_instr (BC.ILoadLocal { stack_loc = loc });
  change_stack_size 1
;;

let emit_add () =
  append_instr BC.IAdd;
  change_stack_size (-1)
;;

let emit_sub () =
  append_instr BC.ISub;
  change_stack_size (-1)
;;

let emit_mul () =
  append_instr BC.IMul;
  change_stack_size (-1)
;;

let emit_eq () =
  append_instr BC.IEq;
  change_stack_size (-1)
;;

let emit_pop () =
  append_instr BC.IPop;
  change_stack_size (-1)
;;

let emit_pop_let () =
  append_instr BC.IPopLet;
  change_stack_size (-1)
;;

let emit_new_object class_id =
  append_instr (BC.INewObject { class_id });
  change_stack_size 1
;;

let emit_fetch field_index = append_instr (BC.IFetch { field_index })
let emit_local_assign stack_loc = append_instr (BC.ILocalAssign { stack_loc })

let emit_field_assign field_index =
  append_instr (BC.IFieldAssign { field_index });
  change_stack_size (-1)
;;

let emit_method_call method_index num_args =
  append_instr (BC.IMethodCall { method_index; num_args });
  change_stack_size (-num_args)
;;

let emit_branch label = append_instr (BC.IBranch label)
let emit_jump label = append_instr (BC.IJump label)
