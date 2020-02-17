load ../../harness

@test "336fdac60711" {
  check 'skip;y   := 4  *-2  ' '⇒ y := (4*-2), {}
⇒ skip, {y → -8}'
}
