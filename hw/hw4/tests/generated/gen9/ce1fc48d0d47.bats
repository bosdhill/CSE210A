load ../../harness

@test "ce1fc48d0d47" {
  check 'if (true  ∧ U     *  -4    <     x     +     y)     then  

z  :=   -3    - y     else  x   :=   y  +   y ' '⇒ x := (y+y), {}
⇒ skip, {x → 0}'
}
