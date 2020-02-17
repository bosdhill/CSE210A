load ../../harness

@test "b3a4aa106df7" {
  check 'z  :=    0   +   1     ;  x :=     G   - x' '⇒ skip; x := (G-x), {z → 1}
⇒ x := (G-x), {z → 1}
⇒ skip, {x → 0, z → 1}'
}
