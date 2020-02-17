load ../../harness

@test "e3a2217e328e" {
  check 'z   :=z;skip   ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
