load ../../harness

@test "62582de46fa4" {
  check 'if (-1  *     -3 < t   +    0 ∧    -3 -   -4 =  -4    +   z)   then   skip    else  skip ' '⇒ skip, {}'
}
