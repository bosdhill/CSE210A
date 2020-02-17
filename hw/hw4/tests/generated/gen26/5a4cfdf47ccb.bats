load ../../harness

@test "5a4cfdf47ccb" {
  check 'if (L=   1   +-3     ∧    -4   +    z     =  z     +x)  then S0   := x     -0  else  
x  :=     x*   -3 ' '⇒ x := (x*-3), {}
⇒ skip, {x → 0}'
}
