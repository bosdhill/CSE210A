load ../../harness

@test "0668d1eaaaa2" {
  check 'y :=   4   - y   ' '⇒ skip, {y → 4}'
}
