load ../../harness

@test "2c37780e2537" {
  check 'skip    ;  y  :=     -4     *z  ' '⇒ y := (-4*z), {}
⇒ skip, {y → 0}'
}
