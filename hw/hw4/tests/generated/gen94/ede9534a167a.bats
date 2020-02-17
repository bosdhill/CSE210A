load ../../harness

@test "ede9534a167a" {
  check 'while y+ y     <   z   +z   ∧ false    do skip   ' '⇒ skip, {}'
}
