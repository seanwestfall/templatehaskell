Template Haskell
================

Template Haskell is an extension to Haskell 98 that allows you to do type-safe compile-time meta-programming, with Haskell both as the manipulating language and the language being manipulated.

Intuitively Template Haskell provides new language features that allow us to convert back and forth between concrete syntax, i.e. what you would type when you write normal Haskell code, and abstract syntax trees. These abstract syntax trees are represented using Haskell datatypes and, at compile time, they can be manipulated by Haskell code. This allows you to reify (convert from concrete syntax to an abstract syntax tree) some code, transform it and splice it back in (convert back again), or even to produce completely new code and splice that in, while the compiler is compiling your module.

It is reminiscent of the macro mechanism as found in Scheme or Lisp. While in Lisp the concrete syntax and the representation of the code, i.e. the abstract syntax tree, have no difference, the abstract syntax tree for TH is represented with normal Haskell data types.

To get started, you need to use the flag `-XTemplateHaskell` to switch these syntactic extensions on.
