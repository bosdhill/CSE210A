load ../../harness

@test "7e3376321856" {
  check 'x :=    -4- 2   ;y :=    0+   -3   ' '⇒ skip; y := (0+-3), {x → -6}
⇒ y := (0+-3), {x → -6}
⇒ skip, {x → -6, y → -3}'
}
