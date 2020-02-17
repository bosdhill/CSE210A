load ../../harness

@test "7b38271d8385" {
  check 'while false   ∧    d8     *     x<  w   +  y  do z    :=     -2    ' '⇒ skip, {}'
}
