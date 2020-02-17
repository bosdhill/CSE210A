load ../../harness

@test "d12c62c67787" {
  check 'z :=     x   +     0    ' '⇒ skip, {z → 0}'
}
