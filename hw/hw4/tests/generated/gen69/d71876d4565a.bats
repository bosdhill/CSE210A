load ../../harness

@test "d71876d4565a" {
  check 'U    :=     2     +    R0;skip     ' '⇒ skip; skip, {U → 2}
⇒ skip, {U → 2}'
}
