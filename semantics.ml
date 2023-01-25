open Ast
open Ast.IR
open Baselib

exception Error of string * Lexing.position

let rec string_of_type_t t =
  match t with
  | Void_t -> "void"
  | Int_t  -> "int"
  | Bool_t -> "bool"
  | Str_t  -> "str"
  | Func_t (r, a) ->
     (if (List.length a) > 1 then "(" else "")
     ^ (String.concat ", " (List.map string_of_type_t a))
     ^ (if (List.length a) > 1 then ")" else "")
     ^ " -> " ^ (string_of_type_t r)

let expr_pos expr =
  match expr with
  | Syntax.Value v -> v.pos
  | Syntax.Var v   -> v.pos
  | Syntax.Call v   -> v.pos                 

                    
let errt expected given pos =
  raise (Error (Printf.sprintf "expected %s but given %s"
                  (string_of_type_t expected)
                  (string_of_type_t given),
                pos))

                 
let collect_constant_string code =
    let counter = ref 0 in
    let rec analyze_value value env=
      match value with
      | Syntax.Int n -> Int n, [], env, Int_t
      | Syntax.Bool n -> Bool n, [], env, Bool_t
      | Syntax.Void -> Void, [], env, Void_t
      | Syntax.Str n ->begin
         match Env.mem n env with
         |true ->  Data (Env.find n env), [], env, Str_t
         |false ->     
           incr counter;
           let lbl =  "str" ^ (string_of_int !counter) in
           let nenv = Env.add n lbl env in
           Data lbl, [(lbl,n)], nenv, Str_t
        end

    and analyze_expr expr env env_type env_fun=
      match expr with
      | Syntax.Value v ->
         let res, ccs , nenv, tpy= analyze_value v.value env in
         Value(res), ccs, nenv, tpy
      | Syntax.Var v ->begin
          match Env.find_opt v.name env_type with
          | None -> raise (Error ("unbound variable: " ^ v.name, v.pos))
          | Some (vt, bl) ->
             if not(bl) then
               raise (Error("variable does not init; " ^ v.name, v.pos))
             else Var v.name,[], env,fst (Env.find v.name env_type)
            
        end
      | Syntax.Call c -> begin
          match Env.find_opt c.func env_fun with
          | None -> raise (Error ("undefined function: " ^ c.func, c.pos))
          | Some (Func_t (rt, args_t)) ->
             if (List.length args_t) <> (List.length c.args) then
               raise (Error (Printf.sprintf "function %s expects %d arguments but was given %d"
                               c.func (List.length args_t) (List.length c.args), c.pos)) ;
             let tmp = List.map2
                          (fun et a ->
                            let aa, ccs,_,at = analyze_expr  a env env_type env_fun in
                            if et <> at then errt et at (expr_pos a) ;
                            aa,ccs)
                          args_t c.args in
             let args = List.map (fun a -> fst a)  tmp in
             let cct = List.map (fun a -> snd a)  tmp in
             Call (c.func,args), (List. flatten cct), env, rt
          | _ -> raise (Error ("not a function: " ^ c.func, c.pos))
        end
                       
   
    and analyze_instr  init_var instr  env env_type type_fun env_fun=
      match instr with
      | Syntax.Assign a -> begin
         match Env.find_opt a.var env_type with
         | None -> raise (Error ("unbound variable: " ^ a.var, a.pos))
         | Some (vt, _) ->
            let ae, ccs, et,tpy = analyze_expr a.expr env env_type env_fun in
            if vt <> tpy then errt vt tpy (expr_pos a.expr) ;
            Assign (a.var, ae), ccs ,env , (Env.add a.var (vt , true) env_type)
        end
      | Syntax.Decl d ->
         Decl d.name, [], env, (Env.add d.name (d.type_t, if init_var == 1 then true else false) env_type)
      | Syntax.Return r ->
         let ae, ccs, nenv,tpy  = analyze_expr r.expr env  env_type env_fun in
         if  type_fun != tpy then errt type_fun tpy (expr_pos r.expr);         
         Return ae, ccs, nenv, env_type
      | Syntax.Expr r ->
         let ae, ccs, nenv,tpy  = analyze_expr r.expr env  env_type env_fun in
         Expr ae, ccs, nenv, env_type
      | Syntax.Condit d ->
         let ae, ccs, nenv,tpy  = analyze_expr d.expr env  env_type env_fun in
         if tpy != Bool_t then errt tpy Bool_t (expr_pos d.expr) ;
           let css, vtt, nenv2, nent2 = analyze_block init_var d._if env env_type  type_fun env_fun in
           let cst, vst, n, t = analyze_block init_var d._else env env_type  type_fun  env_fun in           
           Condit(ae,css,cst), vst @ vtt , nenv, env_type
      | Syntax.Loop l ->
         let ae, ccs, nenv,tpy  = analyze_expr l.expr env  env_type env_fun in
         if tpy != Bool_t then errt tpy Bool_t (expr_pos l.expr) ;
         let css, vtt, n ,t = analyze_block  init_var l.block env env_type  type_fun env_fun in
         Loop(ae,css),vtt,nenv,env_type

    
    and analyze_block init_var block env env_type type_fun env_fun =      
      match block with
      | [] -> [], [], env, env_type
      | a :: b ->
         let ae, ccs , nenv, nent= analyze_instr init_var a env env_type type_fun env_fun in
         let r2, ccs_r, nenv2, nent2 = analyze_block   init_var b nenv nent type_fun  env_fun in
         ae :: r2, List.flatten [ ccs; ccs_r ], nenv2, nent2

    and analyze_def def env env_type env_fun =
      match def with
      | Syntax.Func a ->
         let type_args list = begin
           match list with
           | Syntax.Decl a ->
              a.type_t
           | _ -> Void_t
           end
         in
         let nfun = Env.add a.ident (Func_t(a.type_t, (List.map type_args a.args))) env_fun in
         let _, _ , nenv, nent = analyze_block  1 a.args  env env_type a.type_t nfun in
         let body2, ccs, nenv2, nent2 = analyze_block 0 a.block  nenv nent a.type_t nfun in
         let mv list = begin
             match list with
             | Syntax.Decl a -> Decl a.name
             | _ -> Decl "Error"
           end
         in         
         Func(a.ident, (List.map mv a.args), body2), ccs, nenv2, nent2, nfun

    and analyze_prog code env env_type env_fun =
      match code with
      | [] -> [], []
      | d :: r ->
         let d2, ccs_d, nenv, nent, nfun = analyze_def d env env_type env_fun in
         let r2, ccs_r = analyze_prog r nenv nent nfun in
         d2 :: r2, List.flatten [ ccs_d ; ccs_r ]
    in
    analyze_prog code Env.empty Env.empty _types_
     
let analyze parsed =
  collect_constant_string parsed 
