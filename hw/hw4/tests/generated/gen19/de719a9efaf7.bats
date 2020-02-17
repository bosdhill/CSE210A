load ../../harness

@test "de719a9efaf7" {
  check 'if (-4    +    -3     <-2   +     0  ∧ false)     then   x     :=     x   else 
x   :=     -1   -  2  ' '⇒ x := (-1-2), {}
⇒ skip, {x → -3}'
}
