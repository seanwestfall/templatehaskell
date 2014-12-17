Template Haskell
================
A contribution to Oliver Charles' [24 Days of GHC Extensions](https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html)


~~Template Haskell is a Haskell extension that supports compile­time meta­programming. The purpose of the system is to support the algorithmic construction of programs at compile­time. The ability to generate code at compile time allows the programmer to use programming techniques not available in Haskell itself, such as macro­like expansion, user directed optimization (such as inlining), polytypic programs, generation of supporting data structures and functions from existing data structures and functions.~~

Template Haskell is an extension to Haskell 98 that allows you to do type-safe compile-time meta-programming, with Haskell both as the manipulating language and the language being manipulated. It was introduced in 2002 by Tim Sheard and Simon Peyton Jones. The original paper can be found [here](http://research.microsoft.com/en-us/um/people/simonpj/papers/meta-haskell/meta-haskell.pdf), and the current API can be found [here](http://hackage.haskell.org/package/template-haskell).

Intuitively Template Haskell provides new language features that allow us to convert back and forth between concrete syntax, i.e. what you would type when you write normal Haskell code, and abstract syntax trees. These abstract syntax trees are represented using Haskell datatypes and, at compile time, they can be manipulated by Haskell code. This allows you to reify (convert from concrete syntax to an abstract syntax tree) some code, transform it and splice it back in (convert back again), or even to produce completely new code and splice that in, while the compiler is compiling your module.

It is reminiscent of the macro mechanism as found in Scheme or Lisp. While in Lisp the concrete syntax and the representation of the code, i.e. the abstract syntax tree, have no difference, the abstract syntax tree for TH is represented with normal Haskell data types.

At a glance, 'Quasi-quote' brackets `[|` and `|]` are used to get the abstract syntax tree for the enclosed expression and 'splice' brackets `$(` and `)` are used to convert from abstract syntax tree into code.

Metaprogramming can be used for the creation of DSL (Domain Specific Languages) and generative programming. Current common day uses of such techniques include DSLs for testing (see Jasmine, Mocha) and modeling (html) -- and generative meta-programming is used in ORMs (Object-relational mapping) to map database schemas to objects (ubiquitiously used in web frameworks).

Within the haskell community itself, much of Yesod uses Template Haskell. Specifically it's used in the routing module for type-safe URLs and as a DSL for HTML template variable bindings. Template Haskell allows Yesod to use a single, well-tested piece type-safe module to do all of its routing and templating, avoiding a lot of boilerplate and sidestep bugs that would come with using combinators instead. It's Yesod key feature that separates Yesod from all other web frameworks.

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

#### Template Haskell AST
In Template Haskell, ordinary algebraic data types represent Haskell program fragments. These types modeled after Haskell language syntax and represents AST (abstract syntax tree) of corresponding Haskell code. There is an Exp type to represent Haskell expressions, Pat – for patterns, Lit – for literals, Dec – for declarations, Type – for data types and so on. You can see definitions of all these types in the module Language.Haskell.TH.Syntax. These types refer to each other according to rules of Haskell syntax, so using them you can construct values representing any possible Haskell program fragments. Just some simple examples:
 * `varx = VarE (mkName "x")` represents expression x, i.e. simple variable “x”
 * `patx = VarP (mkName "x")` represents pattern x, i.e. the same variable “x” used in pattern
 * `str = LitE (StringL "str")` represents constant expression `"str"`
 * `tuple = TupE [varx, str]` represents tuple expression `(x,"str")`
 * `LamE [patx]` tuple represents lambda form `(\x -> (x,"str"))`
To make our life easier, all constructors of Exp type have names ending with “E”, of Pat type – ending with “P” and so on. Function mkName, used here, creates value of type Name (representing identifier) from String with name of this identifier.

So, to generate some Haskell code, TH function must just create and return value of type Exp, which serve as representation for this chunk of code. Many developers drop TH because of the complexities of this step.

#### Generating Unique Names with the Quotation Monad

But TH functions are not pure functions returning values of type Exp. Instead, they are computations executed in special monad Q (called “quotation monad”), which allows to automatically generate unique names for variables using monadic operation `newName::String->Q Name`. This operation on each call generates unique name with given prefix. This name then may be used as part of pattern (by using constructor `VarP::Name->Pat`) and expressions (via `VarE::Name->Exp`).

Let’s write simple TH example – TH function tupleReplicate, which when used as `$(tupleReplicate n) x` will return n-element tuple containing x in all positions (just like replicate does for lists). Please draw attention that “n” is an argument of TH function, while “x” is an argument to anonymous function (lambda form) it generates! I provide the whole module containing this function definition (module Language.Haskell.TH is an “external interface” to TH – it provides all the data types and functions which are used to write TH programs):

```haskell
module TupleReplicate where
 
import Language.Haskell.TH
 
tupleReplicate :: Int -> Q Exp
tupleReplicate n = do id <- newName "x"
                      return $ LamE (VarP id)
                                    (TupE $ replicate n $ VarE id)
```

For example, call `tupleReplicate 3` returns `Exp` equivalent to Haskell expression `(\x -> (x,x,x))`.

#### Quotation monad

Because top-level TH functions must return values in Q monad, there are a number of helper functions, which lifts constructors of Exp/Lit/Pat data types into the Q monad: lamE (lifted LamE), varE, appE, varP and so on. Their declarations also use lifted data types: ExpQ = Q Exp, LitQ = Q Lit, PatQ = Q Pat... (you can find all these lifted functions and types in module Language.Haskell.TH.Lib). Using these functions allow to decrease number of cases where do statement is needed.

There is also function lift, which converts any value, which has literal representation, to value of type Exp which represents this literal.

In some rare cases you don’t need unique variable name to be generated; instead, you need to specify the exact name of variable which must be generated in output code. For these cases, there is a (pure) function mkName::String->Name. There is also corresponding helper function “dyn s = return (VarE (mkName s))”, which returns Exp representing variable with exact the given name.

#### Quotation brackets

While the Exp can represent any Haskell expression, programmatic building of Exp values is not so easy work. In order to address this problem, Template Haskell supports quotation brackets, which is a way to convert literal Haskell code to data value representing it. There are four types of quotation brackets:

 * [| ... |], where the "..." is an expression; the quotation has type Q Exp
 * [p| ... |], where the "..." is a pattern; the quotation has type Q Pat
 * [d| ... |], where the "..." is a list of top-level declarations; the quotation has type Q [Dec]
 * [t| ... |], where the "..." is a type; the quotation has type Q Type

For example, `[| \_ -> 0 |]` will be translated to `(return $ LamE [WildP] (LitE (IntegerL 0)))`. The quotation has type Q Exp (rather than just Exp), so that it need to be executed in Q monad to return appropriate Exp. This execution stage allows Template Haskell to replace all identifiers, introduced inside quotation brackets, by unique ones, generated internally with help of newName. For example, quotation `[| \x -> x |]` will be translated to the following code:
```haskell
(do id <- newName "x"; return $ LamE [VarP id] (VarE id))
```
Moreover, inside quotation brackets we can again use splices, making TH some form of macro preprocessor, where a part of code written literally and part of code generated programmatically. For example, the quotation `[| 1 + $(f x) |]` will execute `(f x)` – which must have type Q Exp, translate returned Exp value to literal Haskell code, substitute it instead of splice call, and then reconvert the full expression inside brackets into the code which builds the appropriate Exp. Thanks to automatic renaming of internally used identifiers, the different quotations and even different invocations of the same quotation will never refer to the each others local variables. Consider the following definition:
```haskell
summ n = summ' n [| 0 |]
summ' 0 code = code
summ' n code = [| \x -> $(summ' (n-1) [|$code+x|] ) |]
```
This definition generates lambda form with n parameters which sums up all its arguments, for example `$(summ 3) -> (\x1 -> \x2 -> \x3 -> 0+x1+x2+x3)`. Please draw attention that generated code uses three different names for lambda parameters despite the fact that they all were generated by the same quotation. As you can see in this fragment, depth of quotation and splicing brackets can be arbitrary, the only rule is that they must interchange – no quotations inside quotations, and no splices inside splices.

The quasi­quote notation is a convenient shorthand for representing Haskell programs, and as such it is lexically scoped. More precisely: every occurrence of a variable is bound to the value that is lexically in scope at the occurrence site in the original source program, before any template expansion. This rule has 3 cases:
 * Quotation brackets prevent “capturing” of local variables, declared in one quotation, by another (like the usual Haskell prevents capturing of local variables, used in closures). I already described how that is accomplished by automatic renaming of all locally introduced identifiers. Only [p| ... |] quotation doesn’t rename variables this pattern introduces. Instead, TH provides function genpat, which generates unique pattern from the given one.
 *  Global identifiers, referred inside quotation, “capture” the identifiers available in the environment where this quotation is defined (again, like usual Haskell), so you can pass without any problems value of quotation to functions in other modules, which don’t have these definitions or even have another definitions for the same names. This rule uses internal GHC mechanism of references to symbols in another modules, for example quotation [| map |] may be translated to reference to symbol “Data.List.map” or “++” operation, used in quotation, may be translated to reference to “GHC.Base.++”. If you need to use identifiers, available at place of splicing call, use the $(dyn "str") form.
 * Also inside quotation brackets you can use local variables of currently executed functions. These compile-time variables are run-time constants, so on translating brackets contents TH just substitute current values of these variables as literals. So, in this case [|... x ...|] is converted to [| ... $(lift x) ... |].

Splicing and quoting is opposite operations – one translates Exp to Haskell code, another – Haskell code to Exp, so their co-usage is disappear – $([| ... |]) is equivalent to (...), and so [| $(...) |]. This has inimitable value for development of TH programs – in many cases we can think entirely in terms of Haskell code generated, and don’t bother about Exp values it internally uses.

For example, consider the execution of splice $(summ 3). Just replace this call with its body:
```haskell
$(summ 3) ->
$(summ' 3 [| 0 |]) ->
$([| \x -> $(summ' (3-1) [| $([|0|]) + x |] ) |]) ->
```
Now, we can kill occurrences of the $([| ... |]) and [| $(...) |], at the same time replacing “x” with unique identifier:
```haskell
\x1 -> $(summ' (3-1) [|0+x1|]) ->
```
Again replace call to summ' with its body:
```haskell
\x1 -> $([| \x -> $(summ' (2-1) [| $([|0+x1|]) + x |] ) |]) ->
```
And repeat the last two steps until the end:
```haskell
\x1 -> \x2 -> $(summ' (2-1) [| 0+x1+x2 |]) ->
\x1 -> \x2 -> $([| \x -> $(summ' (1-1) [| $([|0+x1+x2|]) + x |] ) |]) ->
\x1 -> \x2 -> \x3 -> $(summ' (1-1) [| 0+x1+x2+x3 |]) ->
\x1 -> \x2 -> \x3 -> $([| 0+x1+x2+x3 |]) ->
\x1 -> \x2 -> \x3 -> 0+x1+x2+x3
```
It is interesting, that in this definition left side of lambda form (\x0 -> \x1...) is build recursively right on the stack of calls, while the right side (0+x1+...) is accumulated in the code variable. 

#### Reification
Reification is a Template Haskell’s way of allowing the programmer to query the state of the compiler’s internal (symbol) table. The monadic operation `reify::Name->Q Info` returns information about given name: if it’s a global identifier (function, constant, constructor) – you can get it’s type, if it’s a type or class – you can get its structure. By using reify you are get “entry point” to symbol’s table, which then can be used to find information about other types, constructors, classes related to this identifier. You can find definition of type Info in the module Language.Haskell.TH.Syntax.

To get a Name, corresponding to identifier you are interested, you can, theoretically, use function mkName, but this solution is unsafe, because mkName returns unqualified name, which interpretation may be changed depending on context. On the other side, code `VarE id <- [| name |]` is safe, because id will be linked to qualified name (like `My.Own.Module.name`), but too verbose and need monadic context to run. So, Template Haskell supports another lightweight form of quotation: 'identifier returns Name, corresponding to identifier; `let id = 'name` is fully equivalent to `VarE id <- [| name |]`. Please note that this syntax construction has type Name (not Q Exp, nor Q Name), so it can be used in contexts where monadic computations are impossible, for example:

```haskell
f :: Exp ­> Exp
f (App (Var m) e)  |  m=='map  =  ...
```

This new form is still a quotation construct, just like `[| v |]`, and follows the same rules as quotation brackets. For example, one cannot quote inside quotes, so this is illegal: `[| 'v |]`. The more important, that it is resolved statically, and returns fully qualified Name, whose meaning will be persistent.

Haskell's name­spaces make things just slightly more complicated. The quotation `[| P |]` would mean the data constructor P, whereas `[t| P |]` would mean the type constructor P. So we need the same distinction for lightweight quoting. We use two single-quotes to distinguish the type context:

'v means “The name v interpreted in an expression context”
''v means “The name v interpreted in an type context”
So ''a means the type variable a, for example.

The reify function can be used to get structure of type, but it cannot be used to get Exp representing the body of already defined function. If you need to reify function body – put declaration of this function in quotation brackets and explore returned result, like this:
```haskell
$(optimize [d| fib = .... |])
```
or
```haskell
fib = $(optimize [| .... |])
```
#### Error reporting and recovery

#### Debugging

#### Examples
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

