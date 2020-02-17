load ../../harness

@test "6e11698ea498" {
  check 'if (¬(y *   0    =    0   -Lf))      then   

y     :=  z  else   x  :=   z *    x     ' '⇒ x := (z*x), {}
⇒ skip, {x → 0}'
}
