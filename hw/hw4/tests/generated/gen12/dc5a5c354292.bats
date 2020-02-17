load ../../harness

@test "dc5a5c354292" {
  check 'z :=  3  *z  ' '⇒ skip, {z → 0}'
}
