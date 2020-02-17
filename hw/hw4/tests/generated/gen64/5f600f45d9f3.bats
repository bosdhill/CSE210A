load ../../harness

@test "5f600f45d9f3" {
  check 'x    :=1 + 2    ' '⇒ skip, {x → 3}'
}
