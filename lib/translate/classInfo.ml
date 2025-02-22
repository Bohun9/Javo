module S = Lang.Syntax
module BC = Lang.Bytecode

let type_error = Error.type_error

type method_descriptor =
  | MethodDescriptor of
      { name : S.name
      ; param_types : S.ty list
      ; return_type : S.ty
      ; label : BC.label
      }

type class_descriptor =
  | ClassDescriptor of
      { class_id : int
      ; parent_class_name : S.name
      ; fields : (S.name * S.ty) list
      ; methods : method_descriptor list
      }

type class_info = class_descriptor S.NameMap.t

let init_class_info =
  S.NameMap.singleton
    "Object"
    (ClassDescriptor { class_id = 0; parent_class_name = ""; fields = []; methods = [] })
;;

let insert_field fields (n, t) =
  if List.mem_assoc n fields
  then type_error "Can not override class fields"
  else fields @ [ n, t ]
;;

let insert_method methods (name, param_types, return_type, label) =
  let new_method = MethodDescriptor { name; param_types; return_type; label } in
  match
    List.find_opt (fun (MethodDescriptor { name = name'; _ }) -> name = name') methods
  with
  | Some _ ->
    List.map
      (fun (MethodDescriptor { name = name'; _ } as x) ->
        if name = name' then new_method else x)
      methods
  | None -> methods @ [ new_method ]
;;

let define_class class_info name parent_name fields methods =
  match S.NameMap.find_opt parent_name class_info with
  | Some (ClassDescriptor { fields = parent_fields; methods = parent_methods; _ }) ->
    let all_fields = List.fold_left insert_field parent_fields fields in
    let all_methods = List.fold_left insert_method parent_methods methods in
    let class_id =
      Codegen.register_class_table
        (List.length all_fields)
        (List.map (fun (MethodDescriptor { label; _ }) -> label) all_methods)
    in
    let new_class =
      ClassDescriptor
        { class_id
        ; parent_class_name = parent_name
        ; fields = all_fields
        ; methods = all_methods
        }
    in
    S.NameMap.add name new_class class_info
  | None -> type_error (Printf.sprintf "Unbound class name '%s'" parent_name)
;;

let lookup_class class_info name =
  match S.NameMap.find_opt name class_info with
  | Some x -> x
  | None -> type_error (Printf.sprintf "Unbound class name '%s'" name)
;;

let lookup_class_id class_info name =
  let (ClassDescriptor { class_id; _ }) = lookup_class class_info name in
  class_id
;;

let lookup_field class_info name field =
  let (ClassDescriptor { fields; _ }) = lookup_class class_info name in
  match List.assoc_opt field fields with
  | Some t ->
    (match List.find_index (fun (f, _) -> f = field) fields with
     | Some index -> t, index
     | None -> failwith "internal error")
  | None -> type_error (Printf.sprintf "Unbound field name '%s'" field)
;;

let lookup_parent class_info name =
  let (ClassDescriptor { parent_class_name; _ }) = lookup_class class_info name in
  parent_class_name
;;

let lookup_method class_info class_name method_name =
  let (ClassDescriptor { methods; _ }) = lookup_class class_info class_name in
  match
    List.find_opt (fun (MethodDescriptor { name; _ }) -> name = method_name) methods
  with
  | Some (MethodDescriptor { param_types; return_type; _ }) ->
    (match
       List.find_index (fun (MethodDescriptor { name; _ }) -> name = method_name) methods
     with
     | Some index -> param_types, return_type, index
     | None -> failwith "internal error")
  | None -> type_error (Printf.sprintf "Unbound method name '%s'" method_name)
;;

let rec subtype_of class_info t1 t2 =
  match t1, t2 with
  | _, _ when t1 = t2 -> true
  | S.TNull, S.TClass _ -> true
  | S.TClass "Object", _ -> false
  | S.TClass c1, S.TClass _ ->
    subtype_of class_info (S.TClass (lookup_parent class_info c1)) t2
  | _ -> false
;;
