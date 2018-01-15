# Metamathica

Metamathica is a toy Mathematica interpreter written in Mathematica. The vanilla
setup is essentially the call-by-value λ-calculus in the presence of regular
Mathematica lists, the conditional function `If` and assignment. Include as many
primitive functions as you like.

Lisp-style quotation is used to delay evaluation of the input, as I found
Mathematica's native techniques lacking. In this implementation, quotation works
by separating the head of the expression from the sequence of arguments. Thus,
`quote[a+b]` evaluates to `quoted[Plus][a,b]`, ensuring it remains unevaluated.

Including the primitives `Plus`, `Times` and `Equal`, a recursive factorial
function looks essentially the same (up to pattern matching, yet to be
implemented) in both vanilla Mathematica and Metamathica:
```mathematica 
Meta> factorial = Function[{n}, If[n==1, 1, n factorial[n-1]]];
Null
Meta> factorial[5] 
120
``` 

I've included a number of methods for representing data in the `data/`
directory, if Mathematica's structures want to be avoided. Specifically,
implementations of the [Church encoding](https://en.wikipedia.org/wiki/Church_encoding)
and [Peano numbers](https://wiki.haskell.org/Peano_numbers).

## Example

By making changes to the core interpreter, you can alter the behaviour of the
language to whatever degree you like. `core.wl` serves as a minimal template for
this sort of play. I've included an example in the `examples/` directory:
Mathematica augmented with lazy evaluation.

### Lazy evaluation

The directory `lazy/` contains the example implementation of an interpreter in
which function arguments and list elements can be left unevaluated until they are needed in a
computation. To illustrate, consider 
```mathematica
Meta> foo = 1; bar = {1, foo++, 3};
Null
Meta> Length[bar]
3
Meta> foo
1
```

In the context of the lazy interpeter, the Church two-tuples in
`data/churchEncoding.wl` automatically become lazy, since they are
implemented as functions. Nested two-tuples can be used to build a lazy, linked
list:
```mathematica 
first = Function[{q}, q[Function[{r, s} r]]];
rest  = Function[{q}, q[Function[{r}, {s} s]]];
cons  = Function[{m}, m[x, y]];

(* example: infinite list of ones *)
ones = Function[{}, cons[1, ones[]]];
```
Then, at the lazy `Meta` REPL:
```mathematica
Meta> first[ones[]]
1
Meta> first[rest[ones[]]]
1
```

## TODOs

- [ ] Implement a pattern matcher
- [ ] Add tests
- [ ] Church encoding
- [ ] Peano numbers
- [ ] Implement μKanren
