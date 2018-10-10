# Conflict

A small esoteric language giving semantics to git merge conflict.

Conflict is a small esoteric language that was inspired by all these annoying
git merge conflicts that sometime get committed by mistake. What if these merge
conflicts actually had meaningful semantics in the execution of the program?
It is this question I am attempting to answer with this silly programming language.
So what does happen when a merge conflict block is reached? Simple, the main
thread is forked in two, where both threads keep on using the same global scope
while executing their side of the conflict block. This of course comes with all
the concurrency issues one might expect. The rest of the language is loosely
based on some flavour of BASIC and is kept as small as possible.

## Build

Build with `nix` (recommended):

```sh
nix-build
```

Build with `cabal`:

```sh
cabal new-build
```

## Examples

A few example programs can be found in `examples`:

```sh
$ conflict examples/hello.cflt 
Hello world!
```

```sh
$ conflict examples/factorial.cflt <<< 50
30414093201713378043612608166064768844377641568960512000000000000
```

An example of a conflict with concurrent semantics:

```sh
$ cat examples/hello-conf.cflt 
<<<<<<<
print "Hello"
=======
print "World"
>>>>>>>
$ conflict examples/hello-conf.cflt
World
Hello
```

And for debugging the AST is available from the command line:

```sh
$ conflict --ast examples/hello.cflt
Program [Print (Expr (LExpr (Term (Lit (StringLit "Hello world!")))))]
```

## License

This project is licensed under the MIT License.

```
Copyright (c) 2018 Basile Henry
```
