/* Ocamlyacc parser for FunC */

%{
  open Ast
  let anon_cnt = ref 0
%}

%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA 
%token PLUS MINUS TIMES DIVIDE MODULO ASSIGN ANON PIPE BAR
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR WHILE
%token INT BOOL FLOAT VOID STRING FUNC NEW
%token <int> ILIT
%token <bool> BLIT
%token <string> ID FLIT SLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left PIPE BAR
%right ANON NEW
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT
%nonassoc LBRACKET RBRACKET

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */{ ([], [])                 }
  | decls vdecl { (($2 :: fst $1), snd $1) }
  | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
    typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      { { rtyp = $1;
          fname = $2;
          ftype = Func($1, (List.rev (List.map fst $4)));
          formals = List.rev $4;
          locals = List.rev $7;
          body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1, $2)]     }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
    INT                                  { Int                   }
  | BOOL                                 { Bool                  }
  | FLOAT                                { Float                 }
  | VOID                                 { Void                  }
  | STRING                               { String                }
  | typ LBRACKET RBRACKET                { Array($1, None)       }
  | typ LBRACKET expr RBRACKET           { Array($1, Some $3)    }
  | FUNC LT typ LPAREN typ_opt RPAREN GT { Func($3, List.rev $5) }

typ_opt:
  /* nothing */ { [] }
  | typ_list    { $1 } 

typ_list:
    typ                { [$1]     }
  | typ_list COMMA typ { $3 :: $1 }  

vdecl_list:
    /* nothing */    { []       }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { []       }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1     }

expr:
    ILIT                           { IntLit($1)             }
  | FLIT	                         { FloatLit($1)           }
  | BLIT                           { BoolLit($1)            }
  | SLIT                           { StringLit($1)          }
  | func_lit                       { $1                     }
  | ID                             { Id($1)                 }
  | expr PLUS   expr               { Binop($1, Add,   $3)   }
  | expr MINUS  expr               { Binop($1, Sub,   $3)   }
  | expr TIMES  expr               { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr               { Binop($1, Div,   $3)   }
  | expr MODULO expr               { Binop($1, Mod,   $3)   }
  | expr EQ     expr               { Binop($1, Equal, $3)   }
  | expr NEQ    expr               { Binop($1, Neq,   $3)   }
  | expr LT     expr               { Binop($1, Less,  $3)   }
  | expr LEQ    expr               { Binop($1, Leq,   $3)   }
  | expr GT     expr               { Binop($1, Greater, $3) }
  | expr GEQ    expr               { Binop($1, Geq,   $3)   }
  | expr AND    expr               { Binop($1, And,   $3)   }
  | expr OR     expr               { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT           { Unop(Neg, $2)          }
  | NOT expr                       { Unop(Not, $2)          }
  | expr ASSIGN expr               { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN      { Call($1, $3)           }
  | LPAREN expr RPAREN             { $2                     }
  | NEW typ LBRACKET expr RBRACKET { ArrayCreate($2, $4)    }
  | LBRACKET args_opt RBRACKET     { ArrayInit($2)          }
  | ID LBRACKET expr RBRACKET      { ArrayAccess($1, $3)    }
  | pipe_expr BAR                  { Pipe(List.rev $1)      }

func_lit:
    LPAREN formals_opt RPAREN ANON typ LBRACE vdecl_list stmt_list RBRACE { 
      anon_cnt := !anon_cnt + 1;
      FuncLit({ 
        rtyp = $5;
        fname = "_anon" ^ string_of_int !anon_cnt; (* reserved name *)
        ftype = Func($5, (List.rev (List.map fst $2)));
        formals = List.rev $2;
        locals = List.rev $7;
        body = List.rev $8;
      })
    }

args_opt:
    /* nothing */ { []       }
  | args_list  { List.rev $1 }

args_list:
    expr                 { [$1]     }
  | args_list COMMA expr { $3 :: $1 }

pipe_expr:
| expr PIPE expr      { $3 :: [$1] }
| pipe_expr PIPE expr { $3 :: $1   }
