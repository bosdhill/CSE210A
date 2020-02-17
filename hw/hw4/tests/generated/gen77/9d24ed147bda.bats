load ../../harness

@test "9d24ed147bda" {
  check 'x := -1    +y    ' '⇒ skip, {x → -1}'
}
