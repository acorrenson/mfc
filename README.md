# MFC

![CI](https://github.com/jdrprod/mfc/workflows/CI/badge.svg)

MFC stands for "My First Compiler". It is a little compiler for a language created by [SolarLiner](https://github.com/solarliner). It targets ARM.

## Using it

Compiling MFC requires dune and the ocamlgraph package.

**[1] Dependencies installation** :
```
opam install dune ocamlgraph
```

**[2] Compiling Mfc** :
```
dune build
```

**[3] Using Mfc** :
```
./_build/default/bin/main.exe -i input.get -o output.s [-r]
```

Help can be displayed passing the `-h` flag.

```
./_build/default/bin/main.exe -h
```

## Overview of the Gen language

The language Mfc compiles is minimalist. It manipulates only integers.

Variables declarations
```
var x
```

Variables affectation
```
x = 5
```

**Note** : As in old C standards, declarations and affectations should be separated.

Loops
```
while (x < 10) {
  ...
}
```

Conditionals
```
if (x < 10) {
  ...
} else {
  ...
}
```

Arithmetic
```
x = x + x
x = x - x
x = x * x
```

## Structure of the compiler

MFC is designed in OCaml. For now, it's a 3 passes compiler :
1. Parsing (using our own functional parser)
2. Quad generation (intermediate "3 addresses" language)
3. Arm generation (Register allocation)

Ocaml modules are like follow :
+ **Mfc_ast** : Module implementing the AST
+ **Mfc_parser** : The parser
+ **Mfc_parsing** : The parsing lib
+ **Mfc_reg_alloc** : Algorithm(s) for register allocation
+ **Mfc_quad** : Quad generation module
+ **Mfc_env** : Environnement manipulation (symbol table etc)

## Todo(s)

Even if MFC is working on [small examples](https://github.com/jdrprod/mfc/examples), there are possible improvements.

+ **Real intermediate language** : in fact, our quad language is an abstraction over ARM. and is not generic at all.
+ **Ast verification** : The AST is currently poorly verified
+ **Code optimization** : Some simple optimizations could be implemented (DAGs to avoid recomputing arithmetic expressions for example).

