load ../../harness

@test "47bd1b847eab" {
  check 'skip; fm    := -3    + 2 ' '⇒ fm := (-3+2), {}
⇒ skip, {fm → -1}'
}
