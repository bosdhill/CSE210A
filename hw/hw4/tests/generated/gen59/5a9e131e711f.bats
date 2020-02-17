load ../../harness

@test "5a9e131e711f" {
  check 'm    :=   y   +-3    ; x    :=     s   + -2    ' '⇒ skip; x := (s+-2), {m → -3}
⇒ x := (s+-2), {m → -3}
⇒ skip, {m → -3, x → -2}'
}
