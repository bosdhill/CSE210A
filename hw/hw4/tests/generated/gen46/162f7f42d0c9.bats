load ../../harness

@test "162f7f42d0c9" {
  check 'y :=    z -  y    ' '⇒ skip, {y → 0}'
}
