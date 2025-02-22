module S = Lang.Syntax
module BC = Lang.Bytecode

let type_error = Error.type_error

type var_entry =
  | VarEntry of
      { var_ty : S.ty
      ; var_loc : Codegen.stack_loc
      }

type var_table = var_entry S.NameMap.t

let class_info = ref ClassInfo.init_class_info

let assert_subtype t1 t2 msg =
  if ClassInfo.subtype_of !class_info t1 t2
  then ()
  else
    type_error
      (Printf.sprintf "%s - %s is not a subtype of %s" msg (S.show_ty t1) (S.show_ty t2))
;;

let rec trans_expr (e : S.expr) (env : var_table) : S.ty =
  match e with
  | S.ENull ->
    Codegen.emit_null ();
    S.TNull
  | S.EInt n ->
    Codegen.emit_int n;
    S.TInt
  | S.EVar x ->
    (match S.NameMap.find_opt x env with
     | Some (VarEntry { var_ty; var_loc }) ->
       Codegen.emit_local var_loc;
       var_ty
     | None -> type_error (Printf.sprintf "Unbound variable '%s'" x))
  | S.EBinop (op, e1, e2) ->
    let t1 = trans_expr e1 env in
    let t2 = trans_expr e2 env in
    let t =
      match op with
      | S.BINOP_Add | S.BINOP_Sub | S.BINOP_Mul ->
        (match t1, t2 with
         | S.TInt, S.TInt -> S.TInt
         | _ -> type_error "Operands must have type int")
      | S.BINOP_Eq ->
        (match t1, t2 with
         | S.TInt, S.TInt -> S.TBool
         | _ -> type_error "Operands must have type int")
    in
    (match op with
     | S.BINOP_Add -> Codegen.emit_add ()
     | S.BINOP_Sub -> Codegen.emit_sub ()
     | S.BINOP_Mul -> Codegen.emit_mul ()
     | S.BINOP_Eq -> Codegen.emit_eq ());
    t
  | S.ENewObject n ->
    let class_id = ClassInfo.lookup_class_id !class_info n in
    Codegen.emit_new_object class_id;
    S.TClass n
  | S.EMethodCall (e, m, es) ->
    let t = trans_expr e env in
    let arg_tys = List.map (fun e -> trans_expr e env) es in
    (match t with
     | S.TClass c ->
       let param_tys, ret_ty, index = ClassInfo.lookup_method !class_info c m in
       if List.length arg_tys != List.length param_tys
       then type_error "method call arity mismatch"
       else ();
       List.iter2
         (fun t1 t2 -> assert_subtype t1 t2 "Method argument mismatch")
         arg_tys
         param_tys;
       Codegen.emit_method_call index (List.length es);
       ret_ty
     | _ -> type_error "Cannot call method on a non-object type")
  | S.ELet (x, t, e1, e2) ->
    let t' = trans_expr e1 env in
    assert_subtype t' t (Printf.sprintf "Variable '%s' type mismatch" x);
    let loc = Codegen.get_cur_stack_loc () in
    let t2 =
      trans_expr e2 (S.NameMap.add x (VarEntry { var_ty = t; var_loc = loc }) env)
    in
    Codegen.emit_pop_let ();
    t2
  | S.EAssign (S.LValueVar x, e) ->
    (match S.NameMap.find_opt x env with
     | Some (VarEntry { var_ty; var_loc }) ->
       let t = trans_expr e env in
       assert_subtype t var_ty "Variable assignment type mismatch";
       Codegen.emit_local_assign var_loc;
       S.TVoid
     | None -> type_error (Printf.sprintf "Unbound variable '%s'" x))
  | S.EAssign (S.LValueField (e1, f), e2) ->
    (match trans_expr e1 env with
     | S.TClass c ->
       let t, index = ClassInfo.lookup_field !class_info c f in
       let t2 = trans_expr e2 env in
       assert_subtype t2 t "Field assignment type mismatch";
       Codegen.emit_field_assign index;
       S.TVoid
     | _ -> type_error "Cannot assign to a non-object type")
  | S.EFetch (e, f) ->
    (match trans_expr e env with
     | S.TClass c ->
       let ty, index = ClassInfo.lookup_field !class_info c f in
       Codegen.emit_fetch index;
       ty
     | _ -> type_error "Cannot fetch from a non-object type")
  | S.ESeq (e1, e2) ->
    ignore (trans_expr e1 env);
    Codegen.emit_pop ();
    trans_expr e2 env
  | S.EIf (e1, e2, e3) ->
    let false_label = Codegen.gen_fresh_label () in
    let merge_label = Codegen.gen_fresh_label () in
    let t1 = trans_expr e1 env in
    Codegen.emit_branch false_label;
    let t2 = trans_expr e2 env in
    Codegen.emit_jump merge_label;
    Codegen.emit_label false_label;
    let t3 = trans_expr e3 env in
    Codegen.emit_label merge_label;
    (match t1 with
     | S.TBool -> ()
     | _ -> type_error "If guard should have type bool");
    if t2 == t3 then t2 else type_error "If branches should have the same type"
;;

let trans_function
  (fn_name : S.name)
  (params : (S.name * S.ty) list)
  (return_type : S.ty)
  (body : S.expr)
  (label : BC.label option)
  (is_toplevel : bool)
  : unit
  =
  let locs = Codegen.start_new_function fn_name (List.length params) label in
  let env =
    S.NameMap.of_list
      (List.map2 (fun (x, t) l -> x, VarEntry { var_ty = t; var_loc = l }) params locs)
  in
  let ty = trans_expr body env in
  assert_subtype ty return_type "Function return type mismatch";
  Codegen.emit_function is_toplevel
;;

let trans_method
  (class_name : S.name)
  (S.MethodDef { method_name; method_params; method_return_type; method_body } :
    S.method_def)
  (label : BC.label)
  : unit
  =
  let qualified_name = class_name ^ "@" ^ method_name in
  trans_function
    qualified_name
    (("this", S.TClass class_name) :: method_params)
    method_return_type
    method_body
    (Some label)
    false
;;

let trans_class
  (S.ClassDef { class_name; class_parent_name; class_fields; class_methods } :
    S.class_def)
  : unit
  =
  let method_labels =
    List.init (List.length class_methods) (fun _ -> Codegen.gen_fresh_label ())
  in
  let labeled_methods =
    List.map2
      (fun (S.MethodDef { method_name; method_params; method_return_type; _ }) label ->
        method_name, List.map snd method_params, method_return_type, label)
      class_methods
      method_labels
  in
  class_info
  := ClassInfo.define_class
       !class_info
       class_name
       class_parent_name
       class_fields
       labeled_methods;
  List.iter2 (trans_method class_name) class_methods method_labels
;;

let trans_program (S.Program { class_defs; toplevel_expr } : S.program) : unit =
  List.iter trans_class class_defs;
  trans_function "toplevel" [] S.TInt toplevel_expr None true
;;
