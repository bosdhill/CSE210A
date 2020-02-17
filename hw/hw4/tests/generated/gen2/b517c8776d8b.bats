load ../../harness

@test "b517c8776d8b" {
  check 'y     :=y   - x    ;skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
