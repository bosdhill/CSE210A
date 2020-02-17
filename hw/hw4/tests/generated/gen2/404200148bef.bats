load ../../harness

@test "404200148bef" {
  check 'y    :=    1  +   y' '⇒ skip, {y → 1}'
}
