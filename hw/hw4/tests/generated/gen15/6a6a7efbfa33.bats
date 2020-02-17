load ../../harness

@test "6a6a7efbfa33" {
  check 'if (0<   -4    *     -3     ∨     3- 3     <1    +   z)   then z    :=     3  *   z  else   x:=  4    - -4  ' '⇒ z := (3*z), {}
⇒ skip, {z → 0}'
}
