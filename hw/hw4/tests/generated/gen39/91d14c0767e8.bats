load ../../harness

@test "91d14c0767e8" {
  check 'if (y  =     z --2  ∧ 0     +x  <   x)    then aP :=  -1    * x else z :=-3*  x ' '⇒ z := (-3*x), {}
⇒ skip, {z → 0}'
}
