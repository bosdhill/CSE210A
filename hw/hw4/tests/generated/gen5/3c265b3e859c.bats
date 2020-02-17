load ../../harness

@test "3c265b3e859c" {
  check 'y :=   x  - y  ' '⇒ skip, {y → 0}'
}
