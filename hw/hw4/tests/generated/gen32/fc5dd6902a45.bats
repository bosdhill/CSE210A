load ../../harness

@test "fc5dd6902a45" {
  check 'skip  ;z   := -3    + z' '⇒ z := (-3+z), {}
⇒ skip, {z → -3}'
}
