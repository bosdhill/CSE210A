load ../../harness

@test "800ddc7e08ce" {
  check 'skip ;y    :=     y     -   0  ' '⇒ y := (y-0), {}
⇒ skip, {y → 0}'
}
