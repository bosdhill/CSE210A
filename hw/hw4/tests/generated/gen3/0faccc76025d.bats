load ../../harness

@test "0faccc76025d" {
  check 'z   :=     1 * x ' '⇒ skip, {z → 0}'
}
