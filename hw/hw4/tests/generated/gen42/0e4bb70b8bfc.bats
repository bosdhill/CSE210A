load ../../harness

@test "0e4bb70b8bfc" {
  check 'if (false   ∨ true)    then 
 z  :=   -1   *    -1   else skip  ' '⇒ z := (-1*-1), {}
⇒ skip, {z → 1}'
}
