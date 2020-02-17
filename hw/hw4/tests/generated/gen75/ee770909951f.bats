load ../../harness

@test "ee770909951f" {
  check 'skip ; z :=   4   -   3 ' '⇒ z := (4-3), {}
⇒ skip, {z → 1}'
}
