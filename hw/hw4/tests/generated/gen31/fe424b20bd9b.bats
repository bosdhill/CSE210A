load ../../harness

@test "fe424b20bd9b" {
  check 'x := Ai*z;skip  ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
