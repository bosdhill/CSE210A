load ../../harness

@test "d163189d81c1" {
  check 'if (0  - 0   =   y-     x    ∧   y*   K     <  T)    then  z := n*   y   else  


 z    :=   4     -z   ' '⇒ z := (4-z), {}
⇒ skip, {z → 4}'
}
