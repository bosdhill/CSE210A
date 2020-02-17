load ../../harness

@test "8195c7283e90" {
  check 'while x    * 2    < bx  +   z    ∨    1 *   -4 =   3    -  y do skip    ' '⇒ skip, {}'
}
