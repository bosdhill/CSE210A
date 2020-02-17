load ../../harness

@test "c4faef4b4f41" {
  check 'x   := 2    *-3 ' '⇒ skip, {x → -6}'
}
