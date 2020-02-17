load ../../harness

@test "2c4b3f026b86" {
  check 'skip    ;y :=    C    -z ' '⇒ y := (C-z), {}
⇒ skip, {y → 0}'
}
