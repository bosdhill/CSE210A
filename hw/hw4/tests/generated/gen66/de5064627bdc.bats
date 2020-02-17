load ../../harness

@test "de5064627bdc" {
  check 'skip ;z   := y     * x  ' '⇒ z := (y*x), {}
⇒ skip, {z → 0}'
}
