load ../../harness

@test "9edb9f1f8b78" {
  check 'while false  ∧     true      do skip  ' '⇒ skip, {}'
}
