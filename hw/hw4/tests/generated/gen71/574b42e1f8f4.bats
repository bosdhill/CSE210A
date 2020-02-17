load ../../harness

@test "574b42e1f8f4" {
  check 'y := -2  - z    ' '⇒ skip, {y → -2}'
}
