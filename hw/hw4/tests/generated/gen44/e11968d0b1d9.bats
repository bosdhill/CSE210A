load ../../harness

@test "e11968d0b1d9" {
  check 'while e3 *    y    =   4  +z   ∨   -3     +    3<  x     -z  do  z    := x*    z    ' '⇒ skip, {}'
}
