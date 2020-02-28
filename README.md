# MFC

MFC stands for "My First Compiler". It is a little compiler for a language created by [SolarLiner](https://github.com/solarliner). It targets ARM or [EVA](https://github.com/eva-vm).

## Structure of the compiler

MFC is designed in OCaml. For now, it's a 3 passes compiler :
1. Lexical analysis (using our own functional parser)
2. Quad generation (intermediate "3 addresses" language)
3. Arm generation (Register allocation)

Ocaml modules are like follow :
+ **Mfc_ast** : Module implementing the AST 
+ **Mfc_ast_processing** : Module implementing AST verification functions
+ **Mfc_cfg** : Control Flow Graph extraction (for visual rendering only)
+ **Mfc_parser** : The parser
+ **Mfc_parsing** : The parsing lib
+ **Mfc_reg_alloc** : Algorithm(s) for register allocation
+ **Mfc_quad** : Quad generation module
+ **Mfc_env** : Environnement manipulation (symbol table etc)

## Todo(s)

Even if MFC is working on [small examples](https://github.com/jdrprod/mfc/examples), there is still a lot to do.

+ **Real intermediate language** : in fact, our quad language is an abstraction over ARM. and is not generic at all.
+ **Ast verification** : The AST is currently poorly verified
+ **Code optimization** : Even if we perform register allocation 

