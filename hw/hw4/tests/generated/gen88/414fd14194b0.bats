load ../../harness

@test "414fd14194b0" {
  check 'skip    ;y   :=   3 -    x    ' '⇒ y := (3-x), {}
⇒ skip, {y → 3}'
}
