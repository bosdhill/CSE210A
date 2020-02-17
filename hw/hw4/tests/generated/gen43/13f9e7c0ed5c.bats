load ../../harness

@test "13f9e7c0ed5c" {
  check 'skip    ;  y     :=z     - x  ' '⇒ y := (z-x), {}
⇒ skip, {y → 0}'
}
