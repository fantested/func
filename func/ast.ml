(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater |
          Geq |And | Or

type uop = Neg | Not

type typ = 
    Int 
  | Bool 
  | Float 
  | Void 
  | String 
  | Array of typ * expr option
  | Func of typ * typ list

and bind = typ * string

and expr =
    IntLit of int
  | FloatLit of string
  | BoolLit of bool
  | StringLit of string
  | FuncLit of func_decl
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Call of string * expr list
  | ArrayCreate of typ * expr
  | ArrayInit of expr list
  | ArrayAccess of string * expr
  | Pipe of expr list
  | Noexpr

and stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

and func_decl = {
    rtyp : typ;
    ftype: typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(l) -> l
  | FuncLit(fdecl) -> string_of_fdecl fdecl
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ArrayCreate(t, l) ->
      "new " ^ string_of_typ t ^ "[" ^ string_of_expr l ^ "]"
  | ArrayInit(el) ->
      "{" ^ String.concat ", " (List.map string_of_expr el) ^ "}"
  | ArrayAccess(v, e) -> v ^ "[" ^ string_of_expr e ^ "]"
  | Pipe(le) -> List.fold_left (fun l e -> string_of_expr e ^ l) "" le
  | Noexpr -> ""

and string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

and string_of_typ = function
    Int    -> "int"
  | Bool   -> "bool"
  | Float  -> "float"
  | Void   -> "void"
  | String -> "string"
  | Array(t, i) -> 
      (match i with
        Some i -> string_of_typ t ^ "[" ^ string_of_expr i ^ "]"
      | None   -> string_of_typ t ^ "[]")
  | Func(t, tl) -> "func <" ^ string_of_typ t ^ "(" ^ 
      String.concat ", " (List.map string_of_typ tl) ^ ")>"

and string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

and string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
