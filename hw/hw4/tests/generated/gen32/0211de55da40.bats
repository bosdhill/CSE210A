load ../../harness

@test "0211de55da40" {
  check 'skip  ;  y     :=   3 *     x' '⇒ y := (3*x), {}
⇒ skip, {y → 0}'
}
