load ../../harness

@test "f11db33ba31b" {
  check 'skip    ;  y     :=D + x ' '⇒ y := (D+x), {}
⇒ skip, {y → 0}'
}
