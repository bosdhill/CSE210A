load ../../harness

@test "22338f23e9e5" {
  check 'z :=y  +N' '⇒ skip, {z → 0}'
}
