open Ast.IR
open Mips

module Env = Map.Make(String)

type cinfo = { code: Mips.instr list
             ; env: Mips.loc Env.t
             ; fpo: int
             ; counter: int
             ; ret: string
             }
           
let rec compile_value  = function
  | Void  -> [ Li (V0, 0) ]
  | Int n  -> [ Li (V0, n) ]
  | Bool b -> [ Li (V0, (if b then 1 else 0))]
  | Data l -> [ La (V0, Lbl l) ]

and compile_expr env= function
  | Value e -> compile_value e
  | Var v   -> [ Lw (V0, Env.find v env) ]
  | Call (f, args) ->
     let ca = List.map (fun a ->
                  compile_expr  env a
                  @ [ Addi (SP, SP, -4)
                    ; Sw (V0, Mem (SP, 0)) ])
                args in
     List.flatten ca
     @ [ Jal f
       ; Addi (SP, SP, 4 * (List.length args)) ]
     
and compile_instr instr info =
  match instr with
  | Decl v ->
     { info with
       env = Env.add v (Mem (FP, -info.fpo)) info.env
     ; fpo = info.fpo + 4 }
  | Assign (lv, e) ->
     { info with
       code = info.code
              @ compile_expr  info.env e
              @ [ Sw (V0, Env.find lv info.env) ] }
  | Return e ->
     { info with
       code = info.code
              @ compile_expr info.env e 
              @ [ B info.ret ] }
  | Expr e ->
     { info with
       code = info.code
              @ compile_expr info.env e }
  | Condit (c, t, e) ->
     let uniq = string_of_int info.counter in
     let ct = compile_block t { info with code = []
                                        ; counter = info.counter + 1 } in
     let ce = compile_block e { info with code = []
                                        ; counter = ct.counter } in
     { info with
       code = info.code
              @ compile_expr  info.env c
              @ [ Beqz (V0, "else" ^ uniq) ]
              @ ct.code
              @ [ B ("endif" ^ uniq)
                ; Label ("else" ^ uniq) ]
              @ ce.code
              @ [ Label ("endif" ^ uniq) ]
     ; counter = ce.counter }
     
  | Loop (e,b) ->
     let uniq = string_of_int info.counter in

     { info with
       counter = info.counter + 1     
     ; code =   info.code                
                @ [ B ("while" ^ uniq ) ]                
                @ [ Label ("while" ^ uniq) ]
                @ compile_expr  info.env e
                @ [ Beqz (V0, "endwhile" ^ uniq) ]
                @  (compile_block b { info with code = []
                                        ; env = info.env
                                        ; counter = info.counter +1 }).code
                @ [ B ("while" ^ uniq ) ]
                @ [ Label ("endwhile" ^ uniq) ]      
     }

  
and compile_block b info =
  match b with
  | [] -> info
  | i :: r ->
     compile_block r (compile_instr i info)

and  compile_def (Func (name, args, b)) counter =
  let rec nb_arg = begin
      function
      | Decl(a) :: v -> a :: nb_arg v
      | [] -> []
      | _ -> []
    end
  in
  let ident_arg = nb_arg args in
  let cb = compile_block b
             { code = []
             ; env = 

             List.fold_left
             (fun e(i, a) -> Env.add i a e)
             Env.empty
             (List.mapi (fun i a -> a, Mem (FP, 4 * (i + 1))) (List.rev ident_arg))
             ; fpo = 8
             ; counter = counter + 1
             ; ret = "ret" ^ (string_of_int counter) }
  in cb.counter,
     []
     @ [ Label name
       ; Addi (SP, SP, -cb.fpo)
       ; Sw (RA, Mem (SP, cb.fpo - 4))
       ; Sw (FP, Mem (SP, cb.fpo - 8))
       ; Addi (FP, SP, cb.fpo - 4) ]
     @ cb.code
     @ [ Label cb.ret
       ; Addi (SP, SP, cb.fpo)
       ; Lw (RA, Mem (FP, 0))
       ; Lw (FP, Mem (FP, -4))
       ; Jr (RA) ]

let rec compile_prog p counter =
  match p with
  | [] -> []
  | d :: r ->
     let new_counter, cd = compile_def d counter in
     cd @ (compile_prog r new_counter)

  
let compile (code, data) =
  
  { text = Baselib.builtins @ compile_prog code 0  
  ; data = List.map (fun (l, s) -> (l, Asciiz s)) data  }
