load ../../harness

@test "487c0338c6a6" {
  check 'if (true    ∧ R    +    p   <    y    *     -3   -  x)   then   
 z    := -1 -   z  else  skip' '⇒ skip, {}'
}
