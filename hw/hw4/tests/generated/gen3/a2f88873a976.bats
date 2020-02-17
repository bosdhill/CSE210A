load ../../harness

@test "a2f88873a976" {
  check 'if (true    ∨     0     -  R0     <   -4)  then 
 x    :=  -4   - -1 else   skip    ' '⇒ x := (-4--1), {}
⇒ skip, {x → -3}'
}
