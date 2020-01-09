# Ch 1
Formal Semantics
- concerned with rigorously specifying meaningor behavior or programs
    - can reveal ambiguities
    - basis for implementation,analysis, and verification
Syntax
- concerned with grammar (what it looks like) while `semantics` is what those mean
Semantic Approaches
- `Operational`: meaning is specified by computation it induces when executed. How is the effect (result)computed? (step by step)
- `Denotational`: meanining is modelled by math obj that represent effect of executing constructs, only effect is of interest and not how its obtained
- `Axiomatic`: Some properties of the effect (result of executing constructs) are expressed as assertions. There may be aspectsof the executions that are igonored

## Operational Semantics
Operational explanation will tell us steps on how to compute a construct (statements)

The explanation gives an abstraction of how the program is executed on the machine (i.e. order of operations).

```
⟨z:=x; x:=y; y:=z, [x→5, y→7, z→0]⟩
⇒ ⟨x:=y; y:=z, [x→5, y→7, z→5]⟩
⇒ ⟨y:=z, [x→7, y→7, z→5]⟩
⇒ [x→7, y→5, z→5]
```
Above is an example of `structural operational semantics` (small step semantics). Another is `natural operational semantics`(big step semantics), which hides more execution details.

## Denotational Semantics
Concentrate on the effect of executing the programs modelled by mathematical functions:
- the effect of a seq of statements separated by `;`  is the func. composition of the effects of the individual statements
- `:=` effect will be function that given a state will produce a new state (first variable = second variable)
Used to determine whether:
- All variables are initialized before they are used — if not, a warning may be appropriate.

- A certain expression in the program always evaluates to a constant — if so, one can replace the expression by the constant.

- All parts of the program are reachable — if not, they could be removed or a warning might be appropriate.

## Axiomatic Semantics
Is the program correct?
A program is partially correct if postcondition is guaranteed to be satisified after program terminates with precondition fulfilled.

For above example, the pre and post are:
```
{ x=n ∧ y=m } z:=x; x:=y; y:=z { y=n ∧ x=m }
```
Partial correctness property doesn't guarantee that the program will terminate.
Axiomatic semantics provides logical system for proving partial correctness.

## Example Language While
Syntatic notation based on BNF. The `abstract syntax` (how to build arithmetic, boolean expressions and statements in the language) is:
- `n` will range over numerals, `Num`
- `x` will range over variables, `Var`
- `a` will range over arithmetic expressions, `Aexp`
- `b` will range over boolean expressions, `Bexp`
- `S` will range over statements, `Stm`

Where `Num`, `Var`, `Aexp`, `Bexp`, `Stm` are syntatic categories.
The structure of other constructs:
```
a ::= n|x|a1 +a2 |a1 ⋆a2 |a1 −a2
b ::= true|false|a1 =a2 |a1 ≤a2 |¬b|b1 ∧b2
S ::= x:=a|skip|S1 ;S2 |if b then S1 else S2 | while b do S
```
`basis element`: literal values
`composite element`: composed of basis elements

Semantics of While is given by defining `semantic functions` for each of the syntactic categories, with theidea that it takes a semantic entity as argumentand returns its meaning.

## Semantics of Expressions
Assume numerals are in the binary system, and their abstract syntax be
```
n ::= 0 | 1 | n 0 | n 1
```
To determine the number represented by a numeral, define a `semantic function`
```
N: Num -> Z [Maps numeral to natural number]
```
We want `N` to be a `total function` b.c we want to determine a unique number for each numeral of `Num`.

Numerals are syntactic while numbers are semantic (struture vs meaning)

## Semantic Functions
`state`: to each variable the state will associate its current value.
We define it as afunction fromv variables to values.
```
State: Var -> Z
```
Usage:
`s x = 3`
State of x is 3.

Example: list of states
`[x→5, y→7, z→0]`
Given arithmetic expression `a` and state `s`, we can determine the value of the expression. `A` takes syntactic construct `Aexp` and the state `State`.

`A: Aexp → (State → Z)`

## Properties of the Semantics

### Free Variables
`free variables` of an arithmetic expression `a` are defined to be the set of variables occuring in it.

### Substitutions

## Ref
+ BNF https://www.youtube.com/watch?v=8cEhCx8pwio
+ https://en.wikipedia.org/wiki/Formal_grammar
+ https://en.wikipedia.org/wiki/Context-free_grammar
+ https://www.javatpoint.com/automata-derivation-tree
