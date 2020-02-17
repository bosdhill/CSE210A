load ../../harness

@test "cd72ed255123" {
  check 'y :=  x  - -2 ' '⇒ skip, {y → 2}'
}
