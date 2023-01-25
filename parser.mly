%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lvar Lgui
%token Lend Lsc  Lintd Lboold Leq Leqb Lstr Lreturn Lwhile
%token Lsge Lsgt Lsle Lslt Lsne
%token Lmul Ldiv
%token Lsub Ladd Labs
%token Land Lnot 
%token Lif Lelse Lparo Lparf Lcro Lcrf Lputi Lputs Lgeti
%left Lmul Ldiv
%left Ladd Lsub
%token Lvoid Lvrg
%start prog

%type <Ast.Syntax.prog> prog

%%

prog:
  | f = fnct; p = prog {f :: p}
  | Lend {[]}

fnct:
  | Lintd; a = Lvar; Lparo; l = separated_list(Lvrg, argrs) ; Lparf; b = block { Func {ident = a; args = l; block = b; type_t = Int_t; pos = $startpos}}

  | Lboold ; a = Lvar; Lparo; l = separated_list(Lvrg, argrs) ; Lparf; b = block { Func {ident = a; args = l; block = b; type_t = Bool_t; pos = $startpos}}

  | Lstr ; a = Lvar; Lparo; l = separated_list(Lvrg, argrs) ; Lparf; b = block { Func {ident = a; args = l; block = b; type_t = Str_t; pos = $startpos}}

  | Lvoid ; a = Lvar; Lparo; l = separated_list(Lvrg, argrs) ; Lparf; b = block { Func {ident = a; args = l; block = b; type_t = Void_t; pos = $startpos}}


argrs:
  | Lintd; a = Lvar { Decl { type_t = Int_t; name = a; pos = $startpos } }
  | Lboold; a = Lvar { Decl { type_t = Bool_t; name = a; pos = $startpos } }
  | Lstr; a = Lvar { Decl { type_t = Str_t; name = a; pos = $startpos } }
				      

block:
  | Lcro ; b= block {b}
  | Lcrf {[]}
  | e = instr; b = block{ e @ b }
;

instr:

  |  a = Lvar; Ladd; Leq; e = expr; Lsc { [ Assign{
						var = a
					       ; expr =Call {
						      func = "_add";
						      args = [ Var{ name = a  ; pos = $startpos };e] ;
						      pos = $startpos($2)}
					      ; pos= $startpos
					}] }
  |  a = Lvar; Ladd; Ladd; Lsc { [ Assign{
				       var = a
				     ; expr =Call {
						 func = "_add";
						 args = [ Var{ name = a  ; pos = $startpos }; Value { value = Int(1)  ; pos = $startpos }] ;
						 pos = $startpos($2)}
				     ; pos= $startpos
			       }] }
  |  a = Lvar; Lsub; Leq; e = expr; Lsc { [ Assign{
						var = a
					       ; expr =Call {
						      func = "_sub";
						      args = [ Var{ name = a  ; pos = $startpos };e] ;
						      pos = $startpos($2)}
					      ; pos= $startpos
					}] }
  |  a = Lvar; Lsub; Lsub; Lsc { [ Assign{
				       var = a
				     ; expr =Call {
						 func = "_sub";
						      args = [ Var{ name = a  ; pos = $startpos };Value { value = Int(1)  ; pos = $startpos }] ;
						      pos = $startpos($2)}
				     ; pos= $startpos
			       }] }
  |  Lputi; Lparo; e = expr; Lparf; Lsc { [ Expr{
						expr =Call {
						      func = "_puti";
						      args = [e] ;
						      pos = $startpos($2)}
					      ; pos= $startpos
					}] }

  |  Lputs; Lparo; e = expr; Lparf; Lsc { [ Expr{
						expr =Call {
						      func = "_puts";
						      args = [e] ;
						      pos = $startpos($2)}
					      ; pos= $startpos
					}] }

  |  a = Lvar; Lparo; l = separated_list(Lvrg, expr); Lparf; Lsc { [ Expr{
						expr = Call {
						      func = a;
						      args = l ;
						      pos = $startpos($2)}
					      ; pos= $startpos
					}] }
  |  Lwhile; Lparo; e = expr; Lparf; b = block { [Loop {expr = e; block = b; pos = $startpos($2)}] }
  |  Lif; Lparo; e = expr; Lparf; b = block; Lelse; bo = block { [ Condit { expr = e; _if = b; _else = bo;  pos = $startpos($2) } ] }
  |  Lif; Lparo; e = expr; Lparf; b = block;{ [ Condit { expr = e; _if = b; _else = [];  pos = $startpos($2) } ] }
  | Lreturn; e = expr ; Lsc{ [ Return { expr = e ; pos = $startpos } ] } 
  | v = Lvar; Leq; e = expr ; Lsc{[Assign{ var = v; expr = e; pos = $startpos } ] }
  | Lintd; a = Lvar; Lsc { [Decl { type_t = Int_t; name = a ; pos = $startpos } ]}
  | Lboold; a = Lvar; Lsc { [Decl { type_t = Bool_t; name = a; pos = $startpos }] }
  | Lstr; a = Lvar; Lsc {[ Decl { type_t = Str_t; name = a; pos = $startpos } ]}

  | Lintd; a = Lvar ; Leq; e = expr; Lsc{ [Decl { type_t = Int_t; name = a ; pos = $startpos } 
				   ; Assign{ var = a; expr = e; pos = $startpos }] }

  | Lboold; a = Lvar ; Leq; e = expr; Lsc{ [Decl { type_t = Bool_t; name = a; pos = $startpos }
				      ; Assign{ var = a; expr = e; pos = $startpos }] }

  | Lstr; a = Lvar ; Leq; e = expr; Lsc{[ Decl { type_t = Str_t; name = a; pos = $startpos }
				   ; Assign{ var = a; expr = e; pos = $startpos }]}


expr:
  | Lsub; e = expr {
		  Call {
		      func = "_neg";
		      args = [e] ;
		      pos = $startpos
		    }
		}
  | Lnot; e = expr {
		  Call {
		      func = "_not";
		      args = [e] ;
		      pos = $startpos
		    }
		}
  | e = expr; Labs; Labs; a = expr {
			       Call {
				   func = "_or";
				   args = [e;a] ;
				   pos = $startpos
				 }
			  }
  | e = expr; Land; a = expr  {
			       Call {
				   func = "_and";
				   args = [e;a] ;
				   pos = $startpos
				 }
			  }

  | Labs; e = expr; Labs {
			       Call {
				   func = "_abs";
				   args = [e] ;
				   pos = $startpos
				 }
		      }

  | Lparo; e = expr; Lparf { e }
  |  a = Lvar; Lparo; l = separated_list(Lvrg, expr); Lparf { 
								 Call {
								     func = a;
								     args = l ;
								     pos = $startpos($2)
								   }
							       }
  | Lgeti; Lparo; Lparf {
			       Call {
				   func = "_geti";
				   args = [] ;
				   pos = $startpos
				 }
			 }
  | a =expr; Lmul; e = expr {
			       Call {
				   func = "_mul";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }
  | a =expr; Ladd; e = expr {
			       Call {
				   func = "_add";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }
  | a =expr; Ldiv; e = expr {
			       Call {
				   func = "_div";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }
  | a =expr; Lsub; e = expr {
			       Call {
				   func = "_sub";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }
  | a =expr; Leqb; e = expr {
			       Call {
				   func = "_equ";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }
  | a =expr; Lsge; e = expr {
			       Call {
				   func = "_sge";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }
  | a =expr; Lsgt; e = expr {
			       Call {
				   func = "_sgt";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }
  | a =expr; Lsle; e = expr {
			       Call {
				   func = "_sle";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }

  | a =expr; Lslt; e = expr {
			       Call {
				   func = "_slt";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }
  | a =expr; Lsne; e = expr {
			       Call {
				   func = "_sne";
				   args = [a;e] ;
				   pos = $startpos($2)
				 }
			 }

  | n = Lvar { Var{ name = n  ; pos = $startpos } }

  | n = Lgui {
		Value { value = Str(n)  ; pos = $startpos }
	  }

  | n = Lint {
	    Value { value = Int(n)  ; pos = $startpos }
	  }

  | n =  Lbool {
	     Value { value = Bool(n) ; pos = $startpos } 
	   }
  | Lvoid {
	     Value { value = Void ; pos = $startpos } 
	   }
;
