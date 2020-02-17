load ../../harness

@test "bb512409d9ad" {
  check 'while 4 *y  <   z   ∧    false  do    z:=  x    +   -3' '⇒ skip, {}'
}
