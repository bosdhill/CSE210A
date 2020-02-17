load ../../harness

@test "67dd57349cba" {
  check 'if (1=y     +  -2    ∧  z     *  0  <  T* 2)  then skip     else  y  :=     x + x ' '⇒ y := (x+x), {}
⇒ skip, {y → 0}'
}
