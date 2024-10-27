# RCC
The Refinement Calculus Compiler

## Hello World
```
PROCEDURE Main()
BEGIN
    Print("Hello World!");
END.
```

```
Procedure Main()
Begin
    Print("Hello World!");
End.
```

```
procedure Main()
begin
    Print("Hello World!")
end.
```

## Fibonacci

```
PROCEDURE Fib(x : Int64)
BEGIN
    IF x == 0 \/ x == 1
        THEN RETURN 1
        ELSE Fib(x - 1) + Fib(x - 2);
END
```
## Swap
```
procedure Swap(x, y : Int64)
var tmp : Int64.  // create a local variable
begin
    tmp := x;
    x := y;
    y := tmp;
end.
```

## BNF
```bnf
<program> ::= procedure [procedure];

<procedure> ::= ("PROCEDURE" | "Procedure" | "procedure") Ident "(" <param_list> ")" <body> ;

<param_list> ::= <type_decl> ; 
             | type_decl "," [type_decl];

<type_decl> ::= LIdent ":" <type>;
            | LIdent "," (LIdent : LIdents) ":" <type>;

<type> ::= <type_name>;
<type_name> ::= "Int64"; 
            | "Int32";
            | "UInt64";
            | "UInt32";
            | "Double"; 
            | "Float";  
            | "Bool";

<body> ::= [var_intro] <begin> <stm> <end>;

<var_intro> ::= ("VAR" | "Var" | "var") <type_decl> ".";
<var_intro> ::= ("CON" | "Con" | "con") <type_decl> ".";

<begin> ::= ("BEGIN" | "Begin" | "begin");

<end> ::= ("END." | "End." | "end.");

<stm> ::= [<exp_stm>] | <if> | <return_stm>;

<dec> ::= <type_decl>  
      | LIdent ":=" <const>

<const> ::= numbers | <bool> 

<bool> ::= "true" | "false"

<exp_stm> ::= <exp> ";"

<exp> ::= <dec>
      | <digit> "%" <exp>;
      | <digit> "/" <exp>;
      | <digit> "*" <exp>;
      | <digit> "-" <exp>;
      | <digit> "+" <exp>;
      | <bool> "/\ <exp>;
      | <bool> "^" <exp>;
      | <bool> "\/ <exp>;
      | dec;

<if> ::= ("IF" | "If" | "if" ) <exp> 
            ( "THEN" | "Then" | "then")  [<exp_stm>]
            ("ELSE | "Else" | "else") [<exp_stm>]

<do> :: ("DO" | "Do" | "do" ) <exp> "->" [<exp_stm] ("OD" | "Od" | "od")

<return_stm> ::= ("RETURN" | "Return" | "return" ) <exp_stm>;
```
