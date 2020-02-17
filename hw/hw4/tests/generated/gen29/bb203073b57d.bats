load ../../harness

@test "bb203073b57d" {
  check 'z := y    +  x  ' '⇒ skip, {z → 0}'
}
