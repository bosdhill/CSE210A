load ../../harness

@test "f0a900405d1d" {
  check 'z := z   *    -2    ; x    :=    z  ' '⇒ skip; x := z, {z → 0}
⇒ x := z, {z → 0}
⇒ skip, {x → 0, z → 0}'
}
