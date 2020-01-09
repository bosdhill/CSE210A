# Lecture 1
https://classes.soe.ucsc.edu/cse210a/Winter20/
## Grades
|Grading |Perc    | Desc  |
|---|---|---|---|---|
|6 HWs   | 50-60%  |Mostly math   |
|Readings   | 10%  |Summarize paper (did you submit or not) SwA NN Ch 1 due Friday   |
|Project    |20-30%   | explained later|
|Participation |the rest| |

Check canvas

## Programming Languages
The study of a Programming Language as
* Syntax
What does program look like?
Grammars - look it up

* Semantics (focus of class)
What do programs mean?
    - Operational Semantics
    How programs execute
    step-by-step abstract machine
    - Axiomatic Semantics
    Is my program correct?
    Does my program sastisfy some spec?
    (spec - another language to specify how lang is supposed to behave)
    DAFNY - Rustan Leino
* Denotational Semantics (NOT COVERED)
Deep
Map each syntax to math object
* Pragmatics (more Subjective)
Is it a good PL?
readability, domain-appropriate, efficiency, widely used, small language (C), portable, security
Portability vs Efficiency
Security vs Efficiency
Widely Used - doesn't say much

Who needs semantics?
- compilers, interpreters, bug finding
- 737 Max
- precisely describe new language feature

Standard ML
- Guarantee: no craches/seg faults
- But...
```
fun inc x = x + 1       inc : int -> int
func id y = y           id : alpha -> alpha  For any type alpha
val p = ref id          p : ref( alpha -> alpha ) For any type alpha

p := inc                Set pointer Ok as p: ref(int->int)
(!p)(true)              Derefence OK as p: ref(bool->bool)
CRASHES
```
Explanation
Passing bool to `int->int` so you're doing `true + 1`, type checker only knows `alpha->alpha` and thinks `p` works on any kind of function

## Final Project Guidelines
### Types
- Three kinds
    -  small survey of recent work (individual)
    - programming project
    - researchproject
- Team (1-4) people or individual projects
- Scale 20-40+ hours/person
### Scale
- Doesn't expect fancy projects
- Doesn't need to be fully completed
- Welcome to tackle much  more ambitious projects
- Maybe can use MS project for it (can he be a reader?)
### Academic Integrity
- Can't be project in another course
- Good if you care about project beyoung completion

### Implementation
- Anything that manipulates an abstract syntax tree is in scope
- Implement some small language interpreter (Haskell for Scheme?)
- Implement something in 2 r more languages (client/server?)

### Project Proposal
- One or two page document describing
    - project
    - time budget
    - timeline
    - risks
- Project proposal slide
    - one slide
    - short 2 minute pitch to class

