load ../../harness

@test "01770e24d4db" {
  check 'S :=   x  *   x ' '⇒ skip, {S → 0}'
}
