Template Haskell
================
A contribution to Oliver Charles' [24 Days of GHC Extensions](https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html)


~~Template Haskell is a Haskell extension that supports compile­time meta­programming. The purpose of the system is to support the algorithmic construction of programs at compile­time. The ability to generate code at compile time allows the programmer to use programming techniques not available in Haskell itself, such as macro­like expansion, user directed optimization (such as inlining), polytypic programs, generation of supporting data structures and functions from existing data structures and functions.~~

Template Haskell is an extension to Haskell 98 that allows you to do type-safe compile-time meta-programming, with Haskell both as the manipulating language and the language being manipulated. It was introduced in 2002 by Tim Sheard and Simon Peyton Jones. The original paper can be found [here](http://research.microsoft.com/en-us/um/people/simonpj/papers/meta-haskell/meta-haskell.pdf), and the current API can be found [here](http://hackage.haskell.org/package/template-haskell).

Intuitively Template Haskell provides new language features that allow us to convert back and forth between concrete syntax, i.e. what you would type when you write normal Haskell code, and abstract syntax trees. These abstract syntax trees are represented using Haskell datatypes and, at compile time, they can be manipulated by Haskell code. This allows you to reify (convert from concrete syntax to an abstract syntax tree) some code, transform it and splice it back in (convert back again), or even to produce completely new code and splice that in, while the compiler is compiling your module.

It is reminiscent of the macro mechanism as found in Scheme or Lisp. While in Lisp the concrete syntax and the representation of the code, i.e. the abstract syntax tree, have no difference, the abstract syntax tree for TH is represented with normal Haskell data types.

At a glance, 'Quasi-quote' brackets `[|` and `|]` are used to get the abstract syntax tree for the enclosed expression and 'splice' brackets `$(` and `)` are used to convert from abstract syntax tree into code.

Metaprogramming can be used for the creation of DSL (Domain Specific Languages) and generative programming. Current common day uses of such techniques include DSLs, specificially for testing (see Jasmine, Mocha) and modeling (html) -- and generative meta-programming is used in ORMs (Object-relational mapping) scripts/tools to map database schemas to objects typically for web frameworks.

Within the haskell community itself, much of Yesod uses Template Haskell. Specifically it's used in the routing module for type-safe URLs and as a DSL for HTML template variable bindings. Template Haskell allows Yesod to use a single, well-tested piece type-safe module to do all of its routing and templating, avoiding a lot of boilerplate and sidestep bugs that would come with using combinators instead. It's also one of the key features that separate Yesod approach from all the other web frameworks.

In this article, I'll explain the basic features of template haskell -- how to use it -- go through some basic examples, and also talk about some of the downsides of using template haskell.

---

To get started, you need to use the flag `-XTemplateHaskell` to switch these syntactic extensions on. Start ghci with TH enabled, and then load the AST data types:
```bash
$ ghci -XTemplateHaskell
Prelude> :m + Language.Haskell.TH
Prelude>
```

#### Syntax
* A splice is written $x, where x is an identifier, or $(...), where the "..." is an arbitrary expression. There must be no space between the "$" and the identifier or parenthesis. This use of "$" overrides its meaning as an infix operator, just as "M.x" overrides the meaning of "." as an infix operator. If you want the infix operator, put spaces around it.

A splice can occur in place of
  + an expression; the spliced expression must have type Q Exp
  + a pattern; the spliced pattern must have type Q Pat
  + a type; the spliced expression must have type Q Type
  + a list of declarations; the spliced expression must have type Q [Dec]
Inside a splice you can only call functions defined in imported modules, not functions defined elsewhere in the same module.

* A expression quotation is written in Oxford brackets, thus:
  + [| ... |], or [e| ... |], where the "..." is an expression; the quotation has type Q Exp.
  + [d| ... |], where the "..." is a list of top-level declarations; the quotation has type Q [Dec].
  + [t| ... |], where the "..." is a type; the quotation has type Q Type.
  + [p| ... |], where the "..." is a pattern; the quotation has type Q Pat.

* A typed expression splice is written $$x, where x is an identifier, or $$(...), where the "..." is an arbitrary expression.
A typed expression splice can occur in place of an expression; the spliced expression must have type Q (TExp a)

* A typed expression quotation is written as [|| ... ||], or [e|| ... ||], where the "..." is an expression; if the "..." expression has type a, then the quotation has type Q (TExp a).
Values of type TExp a may be converted to values of type Exp using the function unType :: TExp a -> Exp.

* A quasi-quotation can appear in either a pattern context or an expression context and is also written in Oxford brackets:
  + [varid| ... |], where the "..." is an arbitrary string.

* A name can be quoted with either one or two prefix single quotes:
  + 'f has type Name, and names the function f. Similarly 'C has type Name and names the data constructor C. In general 'thing interprets thing in an expression context.
  A name whose second character is a single quote (sadly) cannot be quoted in this way, because it will be parsed instead as a quoted character. For example, if the function is called f'7 (which is a legal Haskell identifier), an attempt to quote it as 'f'7 would be parsed as the character literal 'f' followed by the numeric literal 7. There is no current escape mechanism in this (unusual) situation.
  + ''T has type Name, and names the type constructor T. That is, ''thing interprets thing in a type context.
These Names can be used to construct Template Haskell expressions, patterns, declarations etc. They may also be given as an argument to the reify function.

* You may omit the $(...) in a top-level declaration splice. Simply writing an expression (rather than a declaration) implies a splice. For example, you can write
```haskell
module Foo where
import Bar

f x = x

$(deriveStuff 'f)   -- Uses the $(...) notation

g y = y+1

deriveStuff 'g      -- Omits the $(...)

h z = z-1
```
This abbreviation makes top-level declaration slices quieter and less intimidating.

* Binders are lexically scoped. For example, consider the following code, where a value g of type Bool -> Q Pat is in scope, having been imported from another module
```haskell
y :: Int
y = 7

f :: Int -> Int -> Int
f n = \ $(g True) -> y+n
```
The y in the right-hand side of f refers to the top-level y = 7, even if the pattern splice $(g n) also generates a binder y.
Note that a pattern quasiquoter may generate binders that scope over the right-hand side of a definition because these binders are in scope lexically. For example, given a quasiquoter haskell that parses Haskell, in the following code, the y in the right-hand side of f refers to the y bound by the haskell pattern quasiquoter, not the top-level y = 7.
```haskell
y :: Int
y = 7

f :: Int -> Int -> Int
f n = \ [haskell|y|] -> y+n
```

* The type environment seen by reify includes all the top-level declaration up to the end of the immediately preceding declaration group, but no more.

A declaration group is the group of declarations created by a top-level declaration splice, plus those following it, down to but not including the next top-level declaration splice. The first declaration group in a module includes all top-level definitions down to but not including the first top-level declaration splice.
Concretely, consider the following code
```haskell
module M where
   import ...
   f x = x
   $(th1 4)
   h y = k y y $(blah1)
   $(th2 10)
   w z = $(blah2)
```
In this example
  1. A reify inside the splice $(th1 ..) would see the definition of f.
  2. A reify inside the splice $(blah1) would see the definition of f, but would not see the definition of h.
  3. A reify inside the splice $(th2..) would see the definition of f, all the bindings created by $(th1..), and the definition of h.
  4. A reify inside the splice $(blah2) would see the same definitions as the splice $(th2...).

#### Example
  1. **A `printf` function**

The most common example of using TH is the implementation of C's printf function. 

```haskell
{- Main.hs -}
module Main where

-- Import our template "pr"
import Printf ( pr )

-- The splice operator $ takes the Haskell source code
-- generated at compile time by "pr" and splices it into
-- the argument of "putStrLn".
main = putStrLn ( $(pr "Hello") )
```

```haskell
{- Printf.hs -}
module Printf where

-- Skeletal printf from the paper.
-- It needs to be in a separate module to the one where
-- you intend to use it.

-- Import some Template Haskell syntax
import Language.Haskell.TH

-- Describe a format string
data Format = D | S | L String

-- Parse a format string.  This is left largely to you
-- as we are here interested in building our first ever
-- Template Haskell program and not in building printf.
parse :: String -> [Format]
parse s   = [ L s ]

-- Generate Haskell source code from a parsed representation
-- of the format string.  This code will be spliced into
-- the module which calls "pr", at compile time.
gen :: [Format] -> Q Exp
gen [D]   = [| \n -> show n |]
gen [S]   = [| \s -> s |]
gen [L s] = stringE s

-- Here we generate the Haskell code for the splice
-- from an input format string.
pr :: String -> Q Exp
pr s = gen (parse s)
```
Now run:
```bash
$ ghc --make -XTemplateHaskell main.hs -o main
```
and then run
```bash
$ ./main
```
Which should print out:
```bash
Hello
```
  2. **Generic `zipWith`**

Another cool example is a generic zipWith function:

`zipCons` allows one to zipWith almost any data. Demonstrates the ability to do dynamic binding with TH splices:

```haskell
zipCons :: Name -> Int -> [String] -> ExpQ
zipCons tyName ways functions = do
    let countFields :: Con -> (Name,Int)
        countFields x = case x of
            NormalC n (length -> fields) -> (n, fields)
            RecC n (length -> fields) -> (n,fields)
            InfixC _ n _ -> (n,2)
            ForallC _ _ ct -> countFields ct
 
    TyConI (DataD _ _ _ [countFields -> (c,n)] _) <- reify tyName
    when (n /= length functions) $ fail "wrong number of functions named"
    vs <- replicateM ways $ replicateM n $ newName "x"
    lamE (map (conP c . map varP) vs) $
        foldl (\con (vs,f) ->
                  con `appE`
                    foldl appE
                        (dyn f)
                        (map varE vs))
              (conE c)
              (transpose vs `zip` functions)
```

This example uses whichever '+' is in scope when the expression is spliced:

```haskell
:type $(zipCons ''(,,,) 2 (replicate 4 "+"))
 
  $(zipCons ''(,,,) 2 (replicate 4 "+"))
    :: (Num t, Num t1, Num t2, Num t3) =>
        (t, t1, t2, t3) -> (t, t1, t2, t3) -> (t, t1, t2, t3)
```

#### License

(The MIT License)

Copyright (c) 2014 Sean Westfall

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
