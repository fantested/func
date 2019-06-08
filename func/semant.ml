(* Semantic checking for the FunC compiler *)

open Ast
open Sast
(* open Utility *)

module StringMap = Map.Make(String)

(* Struct for environment management: containing local and external symbol tables *)
type environment = {
  local_vars: typ StringMap.t;
  external_vars: typ StringMap.t;
}

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) (built_in_fdecls: func_decl list) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	      (Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in
  
  (* Add a list of bindings to the environment *)
  let add_binds env l = List.fold_left
    (fun env (typ, name) -> {env with local_vars = StringMap.add name typ env.local_vars})
    env
    l
  in

  (**** Check global variables ****)
  check_binds "global" globals;
  let global_vars = add_binds {
      local_vars = StringMap.empty;
      external_vars = StringMap.empty
    } globals in

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, (t, tl)) = StringMap.add name {
      rtyp = Void;
      fname = name; 
      ftype = Func(Void, tl);
      formals = [(t, "x")];
      locals = []; body = [] } map 
    in List.fold_left add_bind StringMap.empty [ 
      ("print", (Int, [Int]));
			("printb", (Bool, [Bool]));
      ("printf", (Float, [Float]));
      ("prints", (String, [String]));
      ("printbig", (Int, [Int]));
      ("map", (Void, [Void]));
      ("reduce", (Void, [Void]))]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let global_fdecls = 
    (* Add custom built-in functions first so that users can't override them *)
    let all_built_ins = 
      List.fold_left (fun map fd -> StringMap.add fd.fname fd map)
      built_in_decls
      built_in_fdecls
    in
    List.fold_left add_func all_built_ins functions
  in

  (* Merge built-in functions with user-defined functions *)
  let functions = List.fold_left (fun l e -> e :: l) functions built_in_fdecls
  in

  let global_functions = StringMap.fold
    (fun n fdecl m -> StringMap.add n fdecl.ftype m)
    global_fdecls
    StringMap.empty
  in

  (* Prepare global environment *)
  let global_env = {
    local_vars = StringMap.empty;
    external_vars = StringMap.merge 
      (fun n v1 v2 ->
        match v1, v2 with
          (Some _), (Some _) -> raise (Failure (n ^ " is ambiguous"))
        | (Some v), None -> Some v
        | None, (Some v) -> Some v
        | None, None -> None)
      global_vars.local_vars
      global_functions
  } in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s global_fdecls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let rec check_function env f =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" f.formals;
    check_binds "local" f.locals;
    let env = add_binds env f.formals in
    let env = add_binds env f.locals in

    (* Raise an exception if the given rvalue type and the given lvalue 
       are not equal *)
    let check_typ_eq lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* More flexible version for array argument: only check type *)
    let check_array_eq lvaluet rvaluet err =
      (match (lvaluet, rvaluet) with
        (Array(lt, _), Array(rt, _)) -> 
          if lt = rt then rvaluet else raise (Failure err)
      | _ -> if lvaluet = rvaluet then lvaluet else raise (Failure err))
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier env s =
      try StringMap.find s env.local_vars
      with Not_found -> 
        (try StringMap.find s env.external_vars
         with Not_found -> raise (Failure ("undeclared identifier " ^ s)))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr env = function
        IntLit l                  -> (Int, SIntLit l)
      | FloatLit l                -> (Float, SFloatLit l)
      | BoolLit l                 -> (Bool, SBoolLit l)
      | StringLit l               -> (String, SStringLit l)
      | FuncLit(fdecl)            -> check_func_literal env fdecl
      | Noexpr                    -> (Void, SNoexpr)
      | Id s                      -> check_id env s
      | Assign(lhs, rhs) as ex    -> check_assign env lhs rhs ex
      | Unop(op, e) as ex         -> check_unop env op e ex
      | Binop(e1, op, e2) as ex   -> check_binop env e1 op e2 ex
      | Call(fname, args) as call -> check_call env fname args call
      | ArrayCreate(t, l)         -> check_array_create env t l
      | ArrayInit(args)           -> check_array_init env args
      | ArrayAccess(s, e)         -> check_array_access env s e
      | Pipe(el)                  -> check_pipe env el
    
    and check_func_literal env fdecl =
      let nenv = {
        local_vars = StringMap.empty;
        external_vars = StringMap.merge (fun _ v1 v2 ->
          match v1, v2 with
              None, None -> None
            | (Some v), None -> Some v
            | None, (Some v) -> Some v
            | (Some v), (Some _) -> Some v) (* Take the local one if a variable is defined in both *)
          env.local_vars env.external_vars
      } in
      let sfdecl = check_function nenv fdecl in
      (sfdecl.sftype, SFuncLit(sfdecl))

    and check_id env s =
      (type_of_identifier env s, SId s)

    and check_assign env lhs rhs ex =
      match lhs with
      | Id(s) -> 
        let lt = type_of_identifier env s
        and (rt, re) = check_expr env rhs in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
          string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (match rt with
          Array(_, _) -> 
            check_array_eq lt rt err, SAssign((lt, SId(s)), (rt, re))
        | _ -> check_typ_eq lt rt err, SAssign((lt, SId(s)), (rt, re)))
      | ArrayAccess(_, _) ->
          let (lt, le) = check_expr env lhs 
          and (rt, re) = check_expr env rhs in
          let err = "illegal array assignment " ^ string_of_typ lt ^ 
            " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_array_eq lt rt err, SAssign((lt, le), (rt, re)))
      | _ -> raise (Failure ("invalid assignment: " ^ 
            "LHS must be variable or array"))
    
    and check_unop env op e ex=
      let (t, e') = check_expr env e in
      let ty = match op with
          Neg when t = Int || t = Float -> t
        | Not when t = Bool -> Bool
        | _ -> raise (Failure ("illegal unary operator " ^ 
                              string_of_uop op ^ string_of_typ t ^
                              " in " ^ string_of_expr ex))
      in (ty, SUnop(op, (t, e')))

    and check_binop env e1 op e2 ex =
      let (t1, e1') = check_expr env e1 
      and (t2, e2') = check_expr env e2 in
      (* All binary operators require operands of the same type *)
      let same = t1 = t2 in
      (* Determine expression type based on operator and operand types *)
      let ty = match op with
          Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
        | Add | Sub | Mult | Div when same && t1 = Float -> Float
        | Mod when same && t1 = Float -> 
          raise (Failure("mod is not supported for float"))
        | Equal | Neq            when same               -> Bool
        | Less | Leq | Greater | Geq
                  when same && (t1 = Int || t1 = Float) -> Bool
        | And | Or when same && t1 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
                      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                      string_of_typ t2 ^ " in " ^ string_of_expr ex))
      in (ty, SBinop((t1, e1'), op, (t2, e2')))

    and check_call env fname args call = 
      let check_call_helper ret_typ typ_list =
        let param_length = List.length typ_list in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^ 
                          " arguments in " ^ string_of_expr call))
        else let check_args ft e = 
          let (et, e') = check_expr env e in 
          let err = "illegal argument found " ^ string_of_typ et ^
            " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
          in
          (match ft with
            Array(_, _) -> (check_array_eq ft et err, e')
          | _ -> (check_typ_eq ft et err, e'))
        in 
        let args' = List.map2 check_args typ_list args
        in (ret_typ, SCall(fname, args'))
      in
      let ftype = type_of_identifier env fname in
      match ftype with
        Func(t, tl) -> check_call_helper t tl
      | _ -> raise (Failure ("invalid type in function call " ^ 
                    string_of_typ ftype))

    and check_array_create env t l =
      let l' = check_expr env l in
      (Array(t, Some l), SArrayCreate(l'))

    and check_array_init env args =
      let sexpr_list = List.map (fun e -> check_expr env e) args in
      let t_list = List.map (fun (t, _) -> t) sexpr_list in
      let all_eq = (match t_list with
        | []       -> true
        | hd :: tl -> List.for_all ((=) hd) tl) in
      let t = List.hd t_list
      and l = IntLit (List.length sexpr_list) in
      let l' = check_expr env l in
      if not all_eq 
      then raise (Failure "inconsistent types in array initialization")
      else (Array(t, Some l), SArrayInit(l', sexpr_list))

    and check_array_access env s e =
      let lt = type_of_identifier env s in
      let (rt, e') = check_expr env e in
      let (lt, l) = (match lt with
          Array(lt, l) -> lt, l
        | _ -> raise (Failure "invalid id type in array access")) in
      (match e' with 
        SIntLit(i) -> 
          (match l with
            Some (IntLit l) -> 
              if i > l - 1 || i < 0
              then raise (Failure "index out of range in array access")
              else (lt, SArrayAccess(s, (rt, e')))
          | _ -> (lt, SArrayAccess(s, (rt, e'))))
      | _ -> (lt, SArrayAccess(s, (rt, e'))))
    
    and check_pipe env el = 
      let check_first_item e = match e with
          Noexpr | FuncLit _ | Assign _ | ArrayCreate _ -> 
            raise (Failure "invalid first expression for pipe")
        | _ -> e
      in
      
      let rec append_arg (hd: expr) (tl: expr list) = 
        match tl with
          [] -> hd
        | Call(fname, args) :: tl  -> 
            let current_e = Call(fname, args @ [hd]) in
            append_arg current_e tl
        | _ -> raise (Failure "expression needs to be a function call")
      in

      let accum_func = append_arg (check_first_item (List.hd el)) (List.tl el) in
      check_expr env accum_func
    in

    let check_bool_expr env e = 
      let (t', e') = check_expr env e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt env = function
        Expr e -> SExpr (check_expr env e)
      | If(p, b1, b2) -> SIf(check_bool_expr env p, check_stmt env b1, check_stmt env b2)
      | For(e1, e2, e3, st) ->
	        SFor(check_expr env e1, check_bool_expr env e2, check_expr env e3, check_stmt env st)
      | While(p, s) -> SWhile(check_bool_expr env p, check_stmt env s)
      | Return e -> let (t, e') = check_expr env e in
          let err = Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                        string_of_typ f.rtyp ^ " in " ^ string_of_expr e) in 
          (match (t, f.rtyp) with
            (Array(lt, _), Array(rt, _)) -> 
              if lt = rt then SReturn (t, e')
              else raise err
          | _ ->
              if t = f.rtyp then SReturn (t, e') 
              else raise err)
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list env = function
              [Return _ as s] -> [check_stmt env s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list env (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt env s :: check_stmt_list env ss
            | []              -> []
          in SBlock(check_stmt_list env sl)

    in (* body of check_function *)
    { srtyp = f.rtyp;
      sfname = f.fname;
      sftype = f.ftype;
      sformals = f.formals;
      slocals  = f.locals;
      sbody = match check_stmt env (Block f.body) with
	              SBlock(sl) -> sl
              | _ -> raise (
                  Failure ("internal error: block didn't become a block?"))
    }
  in

  (* Check global functions *)
  let sfdecls = List.map (check_function global_env) functions in
  (globals, sfdecls)
  