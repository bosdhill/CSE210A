load ../../harness

@test "d97fa369a5a8" {
  check 'skip ; y    :=    II + z     ' '⇒ y := (II+z), {}
⇒ skip, {y → 0}'
}
