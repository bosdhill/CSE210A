load ../../harness

@test "476cf6899401" {
  check 'y  :=     1     +z;   skip ' '⇒ skip; skip, {y → 1}
⇒ skip, {y → 1}'
}
