load ../../harness

@test "f7c549e820bc" {
  check 'if (z     +     y   = 2     *   x  ∧z   +     2=     -1   *  -2)   then 
y :=     x +     -2 else  
skip ' '⇒ y := (x+-2), {}
⇒ skip, {y → -2}'
}
