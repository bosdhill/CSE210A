load ../../harness

@test "7503d5da15ca" {
  check 'skip    ;y    := y -    -3' '⇒ y := (y--3), {}
⇒ skip, {y → 3}'
}
