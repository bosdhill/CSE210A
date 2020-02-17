load ../../harness

@test "683217b7d99d" {
  check 'b2:=     wp+x  ' '⇒ skip, {b2 → 0}'
}
