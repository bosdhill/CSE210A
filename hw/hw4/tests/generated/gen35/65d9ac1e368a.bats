load ../../harness

@test "65d9ac1e368a" {
  check 'while z    -0     <   x  - 3  ∨    false      do skip' '⇒ skip, {}'
}
