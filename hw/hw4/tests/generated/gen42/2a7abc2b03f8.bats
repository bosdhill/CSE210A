load ../../harness

@test "2a7abc2b03f8" {
  check 'z :=z  --3    ' '⇒ skip, {z → 3}'
}
