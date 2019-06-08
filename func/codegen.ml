(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 
(* open Utility *)

module StringMap = Map.Make(String)

(* Struct for environment management: containing local and external symbol 
   tables and the builder *)
type environment = {
  builder: L.llbuilder;
  local_vars: L.llvalue StringMap.t;
  external_vars: L.llvalue StringMap.t;
}

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "FunC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and string_t   = L.pointer_type (L.i8_type context)
  and void_t     = L.void_type   context in

  (* Return the LLVM type for a FunC type *)
  let rec ltype_of_typ = function
      A.Int         -> i32_t
    | A.Bool        -> i1_t
    | A.Float       -> float_t
    | A.String      -> string_t
    | A.Void        -> void_t
    | A.Array(t, _) -> ltype_of_array t
    | A.Func(t, tl) -> ltype_of_func t tl

  (* Return the LLVM type for an array *)
  and ltype_of_array t = L.pointer_type (ltype_of_typ t)

  (* Return the LLVM type for a function type *)
  and ltype_of_func ret_typ typ_list = 
    let lret_typ = ltype_of_typ ret_typ in
    let largs = Array.of_list (List.fold_left (fun l t -> ltype_of_typ t :: l) 
                                              [] (List.rev typ_list))
    in
    L.pointer_type (L.function_type lret_typ largs)
  in

  (* Initialize LLVM objects *)
  let init typ = match typ with
      A.Int | A.Bool | A.Void -> L.const_int (ltype_of_typ typ) 0
    | A.Float                 -> L.const_float (ltype_of_typ typ) 0.0
    | A.String                -> L.const_pointer_null (ltype_of_typ typ)
    | A.Array(t, _)           -> L.const_pointer_null (ltype_of_typ t)
    | A.Func(t, tl)           -> L.const_pointer_null (ltype_of_func t tl)
  in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	      Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  let (main, _) = StringMap.find "main" function_decls in
  let global_builder = L.builder_at_end context (L.entry_block main) in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" global_builder
  and float_format_str = L.build_global_stringptr "%g\n" "fmt" global_builder 
  and string_format_str = L.build_global_stringptr "%s\n" "fmt" global_builder in

  (* Return the value for a variable or formal argument.
      Check local names first, then external names *)
  let lookup env n = try StringMap.find n env.local_vars
                      with Not_found -> 
                      (try StringMap.find n env.external_vars
                        with Not_found ->
                        raise (Failure (n ^ " is undefined")))
  in

  (* Construct code for an expression; return its value *)
  let rec build_expr env ((typ, e) : sexpr) = match e with
      SIntLit i -> (env, L.const_int i32_t i)
    | SBoolLit b -> (env, L.const_int i1_t (if b then 1 else 0))
    | SFloatLit l -> (env, L.const_float_of_string float_t l)
    | SStringLit s -> (env, L.build_global_stringptr s "tmp" env.builder)
    | SFuncLit sfdecl -> 
        (* Closure implementation: encapsulate parent's environment *)
        let nenv = {
          (* Local variables will be added in build_function_body later *)
          local_vars = StringMap.empty;
          external_vars = StringMap.merge 
            (fun _ v1 v2 ->
              match v1, v2 with
                (Some v), (Some _) -> Some v
              | (Some v), None -> Some v
              | None, (Some v) -> Some v
              | None, None -> None)
            env.local_vars
            env.external_vars;
          builder = env.builder
        } in
        let name = sfdecl.sfname
        and formal_types = 
          Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) sfdecl.sformals)
        in let ftype = L.function_type (ltype_of_typ sfdecl.srtyp) formal_types in
        let fval = L.define_function name ftype the_module in
        build_function_body nenv (fval, sfdecl);
        (nenv, fval)
    | SNoexpr     -> (env, L.const_int i32_t 0)
    | SId s       -> (env, L.build_load (lookup env s) s env.builder)
    | SAssign(lhs, rhs) -> 
        let _, le = lhs in
        let (env', re') = build_expr env rhs in
        let addr = (match le with
            SId s              -> lookup env' s
          | SArrayAccess(s, e) -> 
              let var = lookup env' s in
              let arr_ptr = L.build_load var s env'.builder in
              let (env', le') = build_expr env' e in
              L.build_gep arr_ptr [|le'|] s env'.builder
          | _                  -> raise (Failure "invalid type for LHS")) in
        ignore(L.build_store re' addr env'.builder);
        (env', re')
    | SBinop ((A.Float,_ ) as e1, op, e2) ->
      let (_, e1') = build_expr env e1 in
      let (env', e2') = build_expr env e2 in
      (env', (match op with 
        A.Add     -> L.build_fadd
      | A.Sub     -> L.build_fsub
      | A.Mult    -> L.build_fmul
      | A.Div     -> L.build_fdiv 
      | A.Mod     -> L.build_frem
      | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
      | A.Neq     -> L.build_fcmp L.Fcmp.One
      | A.Less    -> L.build_fcmp L.Fcmp.Olt
      | A.Leq     -> L.build_fcmp L.Fcmp.Ole
      | A.Greater -> L.build_fcmp L.Fcmp.Ogt
      | A.Geq     -> L.build_fcmp L.Fcmp.Oge
      | A.And | A.Or ->
          raise (Failure "internal error: semant should have rejected and/or on float")
      ) e1' e2' "tmp" env'.builder)
    | SBinop (e1, op, e2) ->
      let (_, e1') = build_expr env e1 in
      let (env', e2') = build_expr env e2 in
      (env', (match op with
        A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.Mult    -> L.build_mul
      | A.Div     -> L.build_sdiv
      | A.Mod     -> L.build_srem
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Equal   -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Less    -> L.build_icmp L.Icmp.Slt
      | A.Leq     -> L.build_icmp L.Icmp.Sle
      | A.Greater -> L.build_icmp L.Icmp.Sgt
      | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" env'.builder)
    | SUnop(op, ((t, _) as e)) ->
      let (env', e') = build_expr env e in
      (env', (match op with
        A.Neg when t = A.Float -> L.build_fneg 
      | A.Neg                  -> L.build_neg
      | A.Not                  -> L.build_not) e' "tmp" env'.builder)
    | SCall ("print", [e]) | SCall ("printb", [e]) ->
      let (env', e') = build_expr env e in
      (env', L.build_call printf_func [| int_format_str ; e' |] 
                "printf" env'.builder)
    | SCall ("printf", [e]) -> 
      let (env', e') = build_expr env e in
      (env', L.build_call printf_func [| float_format_str ; e' |]
                "printf" env'.builder)
    | SCall ("prints", [e]) ->
      let (env', e') = build_expr env e in
      (env', L.build_call printf_func [| string_format_str ; e' |]
                "printf" env'.builder)
    | SCall ("printbig", [e]) ->
      let (env', e') = build_expr env e in
      (env', L.build_call printbig_func [| e' |] "printbig" env'.builder)
    | SCall (f, args) ->
        let fptr = lookup env f in
        let fval = L.build_load fptr "tmp" env.builder in
        let (env', llargs) = List.fold_right 
          (fun e (env, llvals) ->
            let (env', llval) = build_expr env e in 
            (env', llval :: llvals))
          args
          (env, [])
        in
        (env', L.build_call fval (Array.of_list llargs) "" env'.builder)
    | SArrayCreate(l) ->
      (match typ with
        A.Array(t, _) ->
          let (env', l') = build_expr env l in
          if L.type_of l' != i32_t 
          then raise (Failure "array length arg doesn't evaluate to integer");
          let ptr = L.build_array_malloc
                      (ltype_of_typ t)
                      l'
                      "arrptr"
                      env.builder
          in
          (env', ptr)
      | _ -> raise (Failure "invalid type in array create"))
    | SArrayInit(lexpr, expr_list) ->
      (match typ with
        A.Array(t, l) ->
          let (env', l') = (match l with
              Some _ -> build_expr env lexpr
            | None   -> raise (Failure "array length is None")) in
          if L.type_of l' != i32_t 
          then raise (Failure "array length arg doesn't evaluate to integer");
          let env', elems = List.fold_right (fun e (env, llvals) -> 
                              let (env', llval) =  build_expr env e in
                              (env', llval :: llvals))
                              expr_list (env', [])
          in
          let ptr = L.build_array_malloc
                      (ltype_of_typ t)
                      l'
                      "arrptr"
                      env.builder
          in
          ignore(List.fold_left (fun i e ->
              let idx = L.const_int i32_t i in
              let eptr = L.build_gep ptr [|idx|] "tmp" env.builder in
              ignore(L.build_store e eptr env.builder);
              i+1)
				    0 elems);
          (env', ptr)
      | _ -> raise (Failure "invalid type in array init"))
    | SArrayAccess(s, e) ->
      let var = lookup env s in
      let arr_ptr = L.build_load var s env.builder in
      let (env', e') = build_expr env e in
      let elem_ptr = L.build_gep arr_ptr [|e'|] "tmp" env'.builder in
      (env', L.build_load elem_ptr "tmp" env'.builder)
    | SPipe(e) -> build_expr env e

  (* Fill in the body of the given function *)
  and build_function_body env (the_function, fdecl) =
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
        declared variables.  Allocate each on the stack, initialize their
        value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
        * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    let external_vars = StringMap.merge (fun _ v1 v2 ->
      match v1, v2 with
          None, None -> None
        | (Some v), None -> Some v
        | None, (Some v) -> Some v
        | (Some v), (Some _) -> Some v) (* Take the local one if a variable is defined in both *)
      env.local_vars env.external_vars
    in

    let env = {
      local_vars = local_vars;
      external_vars = external_vars;
      builder = builder
    } in
  
    (* LLVM insists each basic block end with exactly one "terminator" 
        instruction that transfers control.  This function runs "instr builder"
        if the current block does not already have a terminator.  Used,
        e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal env instr =
      match L.block_terminator (L.insertion_block env.builder) with
        Some _ -> ()
      | None -> ignore (instr env.builder) in

    (* Build the code for the given statement; return the builder for
        the statement's successor (i.e., the next instruction will be built
        after the one generated by this call) *)

    let rec build_stmt env = function
        SBlock sl -> List.fold_left build_stmt env sl
      | SExpr e -> fst (build_expr env e)
      | SReturn e -> 
          let (env', e') = build_expr env e in
          ignore(match fdecl.srtyp with
                  (* Special "return nothing" instr *)
                  A.Void -> L.build_ret_void env'.builder 
                  (* Build return statement *)
                | _ -> L.build_ret e' env'.builder);
          env'
      | SIf (predicate, then_stmt, else_stmt) ->
          let (env', bool_val) = build_expr env predicate in
          let merge_bb = L.append_block context "merge" the_function in
          let build_br_merge = L.build_br merge_bb in (* partial function *)

          let then_bb = L.append_block context "then" the_function in
          let builder = L.builder_at_end context then_bb in
          add_terminal (build_stmt {env' with builder = builder} then_stmt) build_br_merge;

          let else_bb = L.append_block context "else" the_function in
          let builder = L.builder_at_end context else_bb in
          add_terminal (build_stmt {env' with builder = builder} else_stmt) build_br_merge;

          ignore(L.build_cond_br bool_val then_bb else_bb env'.builder);
          {env' with builder = L.builder_at_end context merge_bb}

      | SWhile (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore(L.build_br pred_bb env.builder);

          let pred_builder = L.builder_at_end context pred_bb in
          let (env', bool_val) = build_expr {env with builder = pred_builder} predicate in

          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (build_stmt {env' with builder = (L.builder_at_end context body_bb)} body)
            (L.build_br pred_bb);

          let merge_bb = L.append_block context "merge" the_function in
          ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
          {env with builder = L.builder_at_end context merge_bb}

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> build_stmt env
          ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let env = build_stmt env (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal env (match fdecl.srtyp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      StringMap.add n (L.define_global n (init t) the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Create a map of global functions with pointers to them *)
  let global_functions = StringMap.mapi
    (fun fname (fval, _) ->
      let ft = L.type_of fval in
      let fptr = L.define_global (fname ^ "_ptr") (L.const_pointer_null ft) the_module in
      ignore (L.build_store fval fptr global_builder);
      fptr)
    function_decls
  in

  (* Merge global variables and global functions *)
  let globals = StringMap.merge 
    (fun n v1 v2 ->
      match v1, v2 with
        (Some _), (Some _) -> raise (Failure (n ^ " is ambiguous"))
      | (Some v), None -> Some v
      | None, (Some v) -> Some v
      | None, None -> None)
    global_vars
    global_functions
  in

  (* Prepare global environment *)
  let global_env = {
    local_vars = StringMap.empty;
    external_vars = globals;
    builder = global_builder
  }
  in
  
  StringMap.iter (fun _ fdef -> 
    build_function_body global_env fdef)
    function_decls;

  the_module
