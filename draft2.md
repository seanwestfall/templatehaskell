Template Haskell
================
A contribution to Oliver Charles' [24 Days of GHC Extensions](https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html)

Template Haskell is an extension of Haskell 98 that allows for compile-time metaprogramming -- allowing one to directly convert back and forth between concrete Haskell syntax and the underlying abstract syntax tree (AST) of GHC. Anyone familar with Lisp's macro system will immediately recognize the similarities -- though in Haskell, specific datatypes are used to represent the AST. The ability to generate code at compile time allows one to implement macro-like expansions, as well as polytypic programs, user directed optimization (such as inlining), and the generation of supporting data structures and functions from existing data structures and functions.

In brief,  Oxford brackets `[|` and `|]` are used to get the abstract syntax tree for the enclosed expression and 'splice' brackets `$(` and `)` are used to convert from the abstract syntax tree back into the haskell code. The quotation monad is used to give unique names to the parsed tokens from the supplied haskell code, and reification can be used to look up the state, type, global indentifies within GHC internal symbol table.

Template Haskell was introduced by Tim Sheard and Simon Peyton Jones in their paper "Template Meta-Programming for Haskell" (The original paper can be found [here]()) in 2002, though its changed quite a bit since (see [here]()). It was inspired by C++ templates, though TH is functional more similar to a macro system. The Haskell extension Quasiquotation is often used in conjuntion with Template Haskell, so I will briefly describe it here, but is extensive enough only another full post could do it justice.

In the wild, Template Haskell is used extensively by Yesod for routing and HTML template binding. Outside of Haskell, compile-time metaprogramming is used for the creation of Domain Specific Languages (DSLs), typically in the domains of testing and modeling, and generative metaprogramming (compile-time or not) for object relational mapping, typically for mapping database schemas to non-compiled code. And within Lisp, which is famous for it's macro system, metaprogramming is used to create syntax extensions (syntantic sugar), such as the syntax used for lisp comprehensions.

---
_All code in this guide was excuted with GHCi version 7.6.3 and Template Haskell version 2.9.0.0_


To get started, start up GHCi with the Template Haskell extension by including `-XTemplateHaskell`, then load the AST datatypes:
```bash
$ ghci -XTemplateHaskell
Prelude> :m + Language.Haskell.TH
Prelude Language.Haskell.TH> 
```
To see the AST syntax of some haskell code insert valid haskell syntax into oxford brackets and run it through `runQ` which stands for the Q monad (quotation monad):
```bash
Prelude Language.Haskell.TH> runQ [| 1 + 2 |]
InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))
```
If you parse through the parentices you'll see the return expression forms a tree -- an abstract syntax tree!
![abstract syntax tree](https://github.com/seanwestfall/templatehaskell/blob/master/syntax_tree.png)

Checkout the lift class [source](http://hackage.haskell.org/package/template-haskell-2.7.0.0/docs/src/Language-Haskell-TH-Syntax.html#Lift), which is what's being invoked by the oxford brackets. The Language.Haskell.TH.Syntax contains the defintions of all the types used in the AST. Using these types, it's possible to construct any fragment of the Haskell language. Have a look at the Lit data type as an example. Lit stands for literal,
```haskell
data Lit = CharL Char 
         | StringL String 
         | IntegerL Integer     -- ^ Used for overloaded and non-overloaded
                                -- literals. We don't have a good way to
                                -- represent non-overloaded literals at
                                -- the moment. Maybe that doesn't matter?
         | RationalL Rational   -- Ditto
         | IntPrimL Integer
         | WordPrimL Integer
         | FloatPrimL Rational
         | DoublePrimL Rational
         | StringPrimL String	-- ^ A primitive C-style string, type Addr#
    deriving( Show, Eq, Data, Typeable )
```
tokens represented by it make up literals defined throughout your syntax in the AST, as you can see in our example AST above. Within Language.Haskell.TH.syntax 35 generic data types are declared with the [Data.Data](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Data.html) module. If you qurious about what the AST syntax is refering to study the [source](http://hackage.haskell.org/package/template-haskell-2.7.0.0/docs/src/Language-Haskell-TH-Syntax.html#line-716).

The [Q](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Q) monad handles the expressions typing via context, and also gives it a unique [name](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/src/Language-Haskell-TH-Syntax.html#newName) by appending an integer at the end of the expression name to handle scoping distinction (I'll show you an example in the next section). These unique integer indentifier are used to keep quotations lexically scoped. (see the users guide [wiki](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/template-haskell.html) for a more in depth explanation of TH's lexical scoping).

Let's bind the returned AST expression to a variable:
```bash
Prelude Language.Haskell.TH> let myExp :: Q Exp; myExp = runQ [| 1 + 2 |]
```
`myExp` contains the AST expression (notice the `Q Exp` type, more on this later) -- now lets use the splice brackets, `$( ... )`, to return it back to haskell:
```bash
Prelude Language.Haskell.TH> $(myExp)
3
```
Ta da, you converted concrete haskell to AST and back again.

Now lets try this on some thing slightly more sophisticated: the fibonacci sequence using zipWith:
```haskell
let fibs :: [Integer]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- care to see its AST, use runQ

let fibsQ :: Q Exp
    fibsQ = [| fibs |]

let fibQ :: Int -> Q Exp
    fibQ n = [| fibs !! n |]
```
Now run `$( ... )` to excute the expansion:
```bash
Prelude Language.Haskell.TH> $(fibQ 22)
17711
```
Note, expressions and splices can be nested:
```bash
Prelude Language.Haskell.TH> $(runQ [| fibs !! $( [| 8 |]) |])
21
```

#### Syntax
Template Haskell quotation expression come with 4 different parser types, and an extensive 5th optional type that allows one to define their own types of quotations, called quasi-quotations.
 * `[| ... |]`, or `[e| ... |]`, generates expression AST syntax; it has the type `Q Exp`.
   
   ```bash
   Prelude Language.Haskell.TH> runQ [| 1 + 2 |]
   InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))
   ```
 * `[d| ... |]`, generates a list of top-level declaration AST sytnax; it has the type `Q [Dec]`.
   
   ```bash
   Prelude Language.Haskell.TH> runQ [d|x = 5|]
   [ValD (VarP x_4) (NormalB (LitE (IntegerL 5))) []]
   ```
 * `[t| ... |]`, generates a type AST syntax; it has the type `Q Type`.
   
   ```bash
   Prelude Language.Haskell.TH> runQ [t|Int|]
   ConT GHC.Types.Int
   ```
 * `[p| ... |]`, generates a pattern AST syntax; it has the type `Q Pat`.
   
   ```bash
   Prelude Language.Haskell.TH> runQ [p|(x,y)|]
   TupP [VarP x_5,VarP y_6]
   ```
 * Custom "quasi-quotations", have the form `["quoter"| ... |]`. The "quoter" can be anything except e, d, t, and p, and the token cannot contain spaces. Though, all GHC is doing is determing which parser to use based on the context within the oxford brackets.

An important restriction on Template Haskell to remember is _when inside a splice you can only call functions defined in imported modules, not functions defined elsewhere in the same module._ Quotations and splice have to be defined in separate modules, otherwise you'll see this error:
```bash
GHC stage restriction:
  `...' is used in a top-level splice or annotation,
  and must be imported, not defined locally
```

#### Debugging and Reification
If you want to see the expansion use the flag `-ddump-splices` when starting GHCi. 

#### Examples
A good example to show what one can do with Template Haskell is a type safe haskell version of c's printf function (from [stdio.h](http://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html)):

*Main.hs*
```haskell
{-# LANGUAGE TemplateHaskell #-}
 
-- Import our template "printf"
import PrintF (printf)
 
-- The splice operator $ takes the Haskell source code
-- generated at compile time by "printf" and splices it into
-- the argument of "putStrLn".
main = do
    putStrLn $ $(printf "Hello %s %%x%% %d %%x%%") "World" 12
    putStrLn $ $(printf "Hello %s %s %s %d") "Russian" "with" "Love" 5000
```

*PrintF.hs*
```haskell
{-# LANGUAGE TemplateHaskell #-}
module PrintF where
 
-- NB: printf needs to be in a separate module to the one where
-- you intend to use it.
 
-- Import some Template Haskell syntax
import Language.Haskell.TH
 
-- Possible string tokens: %d %s and literal strings
data Format = D | S | L String
    deriving Show
 
-- a poor man's tokenizer
tokenize :: String -> [Format]
tokenize [] = []
tokenize ('%':c:rest) | c == 'd' = D : tokenize rest
                      | c == 's' = S : tokenize rest
tokenize (s:str) = L (s:p) : tokenize rest -- so we don't get stuck on weird '%'
    where (p,rest) = span (/= '%') str
 
-- generate argument list for the function
args :: [Format] -> [PatQ]
args fmt = concatMap (\(f,n) -> case f of
                                  L _ -> []
                                  _   -> [varP n]) $ zip fmt names
    where names = [ mkName $ 'x' : show i | i <- [0..] ]
 
-- generate body of the function
body :: [Format] -> ExpQ
body fmt = foldr (\ e e' -> infixApp e [| (++) |] e') (last exps) (init exps)
    where exps = [ case f of
                    L s -> stringE s
                    D   -> appE [| show |] (varE n)
                    S   -> varE n
                 | (f,n) <- zip fmt names ]
          names = [ mkName $ 'x' : show i | i <- [0..] ]
 
-- glue the argument list and body together into a lambda
-- this is what gets spliced into the haskell code at the call
-- site of "printf"
printf :: String -> Q Exp
printf format = lamE (args fmt) (body fmt)
    where fmt = tokenize format
```
Compile the following with:
```bash
$ ghc --make Main.hs -o main
```
running main will print out:
```bash
$ ./main
Hello World %%x%% 22 %%x%%
Hello Russian with Love 5000
```

#### Conclusion


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
