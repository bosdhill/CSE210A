load ../../harness

@test "4369f82765f0" {
  check 'x   :=    -2     *  0;skip ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
