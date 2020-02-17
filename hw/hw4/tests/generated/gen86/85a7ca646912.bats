load ../../harness

@test "85a7ca646912" {
  check 'if (d9   -     -3    =z-     x∧    -3 *   -1     =-4     +    y)   then 

x   :=    -4     -     -2  else z  :=x  *    y ' '⇒ z := (x*y), {}
⇒ skip, {z → 0}'
}
