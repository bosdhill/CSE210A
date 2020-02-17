load ../../harness

@test "ffd584883d3a" {
  check 'while 1 - 0 < y    ∧  x   *  y<    z   *   -4     do y:=     3   *x    ' '⇒ skip, {}'
}
