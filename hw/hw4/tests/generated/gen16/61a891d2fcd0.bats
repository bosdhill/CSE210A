load ../../harness

@test "61a891d2fcd0" {
  check 'if (o +1    <   y  *-1    ∧     x  -    x     =     y   - 1)   then 
skip     else x   :=   x    + x   ' '⇒ x := (x+x), {}
⇒ skip, {x → 0}'
}
