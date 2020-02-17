load ../../harness

@test "f7d195e2a8bb" {
  check 'skip   ;ap :=-2   - 3   ' '⇒ ap := (-2-3), {}
⇒ skip, {ap → -5}'
}
