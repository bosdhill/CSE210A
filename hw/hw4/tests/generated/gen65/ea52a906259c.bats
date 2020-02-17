load ../../harness

@test "ea52a906259c" {
  check 'while x     +     y    <  1     + -4    ∨  false do   skip ' '⇒ skip, {}'
}
