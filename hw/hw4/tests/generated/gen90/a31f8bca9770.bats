load ../../harness

@test "a31f8bca9770" {
  check 'x :=    y    *  z ' '⇒ skip, {x → 0}'
}
