load ../../harness

@test "fe02b48b73bc" {
  check 'skip;  y     := -2   +  3' '⇒ y := (-2+3), {}
⇒ skip, {y → 1}'
}
