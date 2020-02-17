load ../../harness

@test "d9178e8bc229" {
  check 'while 2  +   -4<z    +   -4     ∨   false      do z    := -2    - y    ' '⇒ skip, {}'
}
