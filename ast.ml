
type type_t =
  | Void_t
  | Int_t
  | Bool_t
  | Str_t
  | Func_t of type_t * type_t list

module Syntax = struct
  type ident = string
  type value =
    | Void
    | Int of int
    | Bool of bool
    | Str of string
  type expr =
    | Value of { value: value
               ; pos: Lexing.position }
    | Var   of { name: ident
               ; pos: Lexing.position }
    | Call of { func: ident
              ; args: expr list
              ; pos: Lexing.position }
  type instr =
    | Expr   of { expr: expr
                ; pos: Lexing.position }
    | Assign of { var: ident
                ; expr: expr
                ; pos: Lexing.position }
    | Decl   of { name: ident
                ; type_t: type_t
                ; pos: Lexing.position }
    | Return of { expr: expr
                ; pos: Lexing.position }
    | Condit of  { expr: expr
                 ; _if: block
                 ; _else: block
                 ; pos: Lexing.position }
    | Loop    of { expr: expr
                 ; block: block
                 ; pos: Lexing.position }
  and block = instr list
  type def =
    | Func    of { ident: ident
                 ; args : instr list
                 ; block: block
                 ; type_t: type_t
                 ; pos: Lexing.position }
  and prog = def list
end

module IR = struct
  type ident = string
  type value =
    | Int  of int
    | Bool of bool
    | Void
    | Data of string
  type expr =
    | Value of value
    | Var   of ident
    | Call  of ident * expr list
  type instr =
    | Expr   of expr
    | Assign of ident * expr
    | Decl   of ident
    | Return of expr
    | Condit of expr * block * block
    | Loop   of expr * block
  and block = instr list
  type def =
    | Func of ident * instr list * block
  type prog = def list
end
