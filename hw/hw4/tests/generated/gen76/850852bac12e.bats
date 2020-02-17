load ../../harness

@test "850852bac12e" {
  check 'z   :=     1--3  ' '⇒ skip, {z → 4}'
}
