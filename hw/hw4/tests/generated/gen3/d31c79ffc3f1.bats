load ../../harness

@test "d31c79ffc3f1" {
  check 'x     :=   4    + -1  ; b9     :=   -4 *     -4  ' '⇒ skip; b9 := (-4*-4), {x → 3}
⇒ b9 := (-4*-4), {x → 3}
⇒ skip, {b9 → 16, x → 3}'
}
