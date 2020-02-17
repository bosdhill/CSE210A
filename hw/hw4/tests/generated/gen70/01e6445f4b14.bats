load ../../harness

@test "01e6445f4b14" {
  check 'x    :=     -1+  y     ;x    :=    Wy  ' '⇒ skip; x := Wy, {x → -1}
⇒ x := Wy, {x → -1}
⇒ skip, {x → 0}'
}
