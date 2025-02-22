type name = string [@@deriving show]

module NameMap = Map.Make (struct
    type t = name

    let compare = compare
  end)

type binop =
  | BINOP_Add
  | BINOP_Sub
  | BINOP_Mul
  | BINOP_Eq
[@@deriving show]

type ty =
  | TInt
  | TBool
  | TClass of name
  | TNull
  | TVoid
[@@deriving show]

type lvalue =
  | LValueVar of name
  | LValueField of expr * name
[@@deriving show]

and expr =
  | ENull
  | EInt of int
  | EVar of name
  | EBinop of binop * expr * expr
  | ENewObject of name
  | EMethodCall of expr * name * expr list
  | ELet of name * ty * expr * expr
  | EAssign of lvalue * expr
  | EFetch of expr * name
  | ESeq of expr * expr
  | EIf of expr * expr * expr
[@@deriving show]

type method_def =
  | MethodDef of
      { method_name : name
      ; method_params : (name * ty) list
      ; method_return_type : ty
      ; method_body : expr
      }
[@@deriving show]

type class_def =
  | ClassDef of
      { class_name : name
      ; class_parent_name : name
      ; class_fields : (name * ty) list
      ; class_methods : method_def list
      }
[@@deriving show]

type program =
  | Program of
      { class_defs : class_def list
      ; toplevel_expr : expr
      }
[@@deriving show]
