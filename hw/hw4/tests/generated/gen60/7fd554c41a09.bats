load ../../harness

@test "7fd554c41a09" {
  check 'x   := 1     - y ; skip     ' '⇒ skip; skip, {x → 1}
⇒ skip, {x → 1}'
}
