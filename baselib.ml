open Ast
open Mips
   
module Env = Map.Make(String)

let _types_ =
    Env.of_seq
    (List.to_seq
       [ "_add", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_sub", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_mul", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_div", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_equ", Func_t (Bool_t, [ Int_t ; Int_t ])
       ; "_sge", Func_t (Bool_t, [ Int_t ; Int_t ])
       ; "_sgt", Func_t (Bool_t, [ Int_t ; Int_t ])
       ; "_sle", Func_t (Bool_t, [ Int_t ; Int_t ])
       ; "_slt", Func_t (Bool_t, [ Int_t ; Int_t ])
       ; "_sne", Func_t (Bool_t, [ Int_t ; Int_t ])
       ; "_puti", Func_t (Void_t, [ Int_t ])
       ; "_geti", Func_t (Int_t, [])
       ; "_puts", Func_t (Void_t, [ Str_t ])
       ; "_abs", Func_t (Int_t, [ Int_t ])
       ; "_and", Func_t (Bool_t, [ Bool_t; Bool_t ])
       ; "_or", Func_t (Bool_t, [ Bool_t; Bool_t ])
       ; "_not", Func_t (Bool_t, [ Bool_t ])
       ; "_neg", Func_t (Int_t, [ Int_t ])
    ])


let builtins = [
      Label "_mul"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Mul (V0, T0, T1)
    ; Jr RA

    ; Label "_puti"
    ; Lw (A0, Mem (SP, 0))
    ; Li (V0, Syscall.print_int)
    ; Syscall
    ; Jr RA

    ; Label "_geti"
    ; Lw (A0, Mem (SP, 0))
    ; Li (V0, Syscall.read_int)
    ; Syscall
    ; Jr RA

    ; Label "_puts"
    ; Lw (A0, Mem (SP, 0))
    ; Li (V0, Syscall.print_str)
    ; Syscall
    ; Jr RA
    
    ; Label "_equ"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Seq (V0, T0, T1)
    ; Jr RA
    
    ; Label "_sge"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Sge (V0, T1, T0)
    ; Jr RA

    ; Label "_sgt"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Sgt (V0, T1, T0)
    ; Jr RA
    
    ; Label "_sle"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Sle (V0, T1, T0)
    ; Jr RA

    ; Label "_slt"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Slt (V0, T1, T0)
    ; Jr RA

    ; Label "_sne"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Sne (V0, T1, T0)
    ; Jr RA

    ; Label "_add"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Add (V0, T0, T1)
    ; Jr RA

    ; Label "_div"
    ; Lw (T1, Mem (SP, 0))
    ; Lw (T0, Mem (SP, 4))
    ; Div (V0, T0, T1)
    ; Jr RA

    ; Label "_sub"
    ; Lw (T1, Mem (SP, 0))
    ; Lw (T0, Mem (SP, 4))
    ; Sub (V0, T0, T1)
    ; Jr RA
    
    ; Label "_abs"
    ; Lw (T0, Mem (SP, 0))
    ; Abs (V0, T0)
    ; Jr RA

    ; Label "_and"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; And (V0, T0, T1)
    ; Jr RA

    ; Label "_or"
    ; Lw (T0, Mem (SP, 0))
    ; Lw (T1, Mem (SP, 4))
    ; Or (V0, T0, T1)
    ; Jr RA

    ; Label "_not"
    ; Lw (T0, Mem (SP, 0))
    ; Not (V0, T0)
    ; Jr RA

    ; Label "_neg"
    ; Lw (T0, Mem (SP, 0))
    ; Neg (V0, T0)
    ; Jr RA
    
  ]
